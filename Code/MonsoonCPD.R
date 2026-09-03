library(dplyr)
library(changepoint)

water_year <- function(date) {
  y <- as.numeric(format(date, "%Y"))
  m <- as.numeric(format(date, "%m"))
  ifelse(m >= 10, y + 1, y)
}

season <- function(month) {
  if (month %in% c(12, 1, 2, 3)) return(1)
  else if (month %in% c(4, 5, 6)) return(2)
  else if (month %in% c(7, 8, 9)) return(3)
  else if (month %in% c(10, 11)) return(4)
}

flow <- read.csv("Data/USPPFlowMonitoring2006_2025.csv")
flow$Flow.Code[flow$Flow.Code == 2] <- 1
flow <- flow[flow$Flow.Code %in% c(0, 1), ]
flow$Date <- as.Date(flow$Date)
flow$WY <- water_year(flow$Date)
flow$Season <- sapply(as.integer(format(flow$Date, "%m")), season)

camera_map <- data.frame(
  Site = c("Boquillas", "CharlestonMesquite", "Contention", "Fairbank", "Hereford", "Hunter", "Moson", "St.David"),
  Camera = c(8, 2, 6, 1, 5, 4, 3, 7),
  stringsAsFactors = FALSE
)
flow <- left_join(flow, camera_map, by = "Site")

camera_filter <- c(1, 2, 6, 7)
fp_series <- flow %>%
  filter(Camera %in% camera_filter, Season == 3) %>%
  group_by(WY) %>%
  summarise(value = mean(Flow.Code, na.rm = TRUE), .groups = "drop") %>%
  arrange(WY)

climate <- read.csv("Data/Daymet2006_2024.csv")
climate$date <- as.Date(climate$date)
climate$WY <- water_year(climate$date)
climate$Season <- sapply(as.integer(format(climate$date, "%m")), season)
climate <- climate[climate$camera %in% camera_filter, ]

monsoon_climate <- climate %>%
  filter(Season == 3) %>%
  group_by(WY) %>%
  summarise(
    precip = sum(prcp, na.rm = TRUE),
    vpd = mean(tvpd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(WY)

points <- read.csv("Data/pts_for_Jawad.csv")[1:8, ]
points$Name <- gsub(" Camera", "", points$Name)
points$Index <- seq_len(nrow(points))
points <- left_join(points, camera_map, by = c("Name" = "Site"))
points <- points[points$Camera %in% camera_filter, ]

pmdl <- do.call(rbind, lapply(seq_len(nrow(points)), function(j) {
  file <- paste0("Data/Jawad_ET/J", points$Index[j], ".csv")
  df <- read.csv(file)
  df$Site <- points$Name[j]
  df
}))
pmdl$date <- as.Date(pmdl$date)
pmdl$ET <- pmdl$LE_model * 0.0353
pmdl$WY <- water_year(pmdl$date)
pmdl$Season <- sapply(as.integer(format(pmdl$date, "%m")), season)

monsoon_et <- pmdl %>%
  filter(Season == 2) %>%
  group_by(date, WY) %>%
  summarise(ET = mean(ET, na.rm = TRUE), .groups = "drop") %>%
  group_by(WY) %>%
  summarise(value = sum(ET, na.rm = TRUE), .groups = "drop") %>%
  arrange(WY)

sprnca_gw <- read.csv("Data/GW/CamSPRNCA_GW.csv", stringsAsFactors = FALSE)
sprnca_gw$datetime <- as.Date(sprnca_gw$datetime)
sprnca_gw$WY <- water_year(sprnca_gw$datetime)
sprnca_gw$Season <- sapply(as.integer(format(sprnca_gw$datetime, "%m")), season)
sprnca_gw$gw_mm <- sprnca_gw$dtgw_m * 1000

gw_camera_filter <- c(1, 2, 6, 7)

monsoon_gw <- sprnca_gw %>%
  filter(Season == 3, camera %in% gw_camera_filter) %>%
  group_by(WY, site_no) %>%
  summarise(gw_mm = min(gw_mm, na.rm = TRUE), .groups = "drop") %>%
  group_by(WY) %>%
  summarise(value = min(gw_mm, na.rm = TRUE), .groups = "drop") %>%
  arrange(WY)

series_start_WY <- 2006

series <- list(
  FP = fp_series %>% select(WY, value),
  precip = monsoon_climate %>% select(WY, precip) %>% rename(value = precip),
  vpd = monsoon_climate %>% select(WY, vpd) %>% rename(value = vpd),
  gw = monsoon_gw %>% select(WY, value),
  et = monsoon_et %>% select(WY, value)
)
series <- lapply(series, function(df) df[df$WY >= series_start_WY, ])

empty_breakpoints <- function(var_name, method_name) {
  data.frame(
    variable = character(0),
    method = character(0),
    bp_year = numeric(0),
    stringsAsFactors = FALSE
  )
}

empty_segments <- function(var_name) {
  data.frame(
    variable = character(0),
    method = character(0),
    segment_id = integer(0),
    segment_start_year = numeric(0),
    segment_end_year = numeric(0),
    intercept = numeric(0),
    slope = numeric(0),
    mean = numeric(0),
    sd = numeric(0),
    stringsAsFactors = FALSE
  )
}

make_regression_segments <- function(x, years, var_name) {
  matrix_reg <- cbind(x, 1, years)
  cp_reg <- cpt.reg(matrix_reg, method = "PELT")
  change_pts <- cpts(cp_reg)

  if (length(change_pts) == 0) {
    fit <- lm(x ~ years)
    seg <- data.frame(
      variable = var_name,
      method = "regression",
      segment_id = 1L,
      segment_start_year = min(years),
      segment_end_year = max(years),
      intercept = as.numeric(coef(fit)[1]),
      slope = as.numeric(coef(fit)[2]),
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    return(seg)
  }

  segments <- c(0, change_pts, length(x))
  params <- param.est(cp_reg)
  rows <- lapply(seq_along(params$beta[, 1]), function(i) {
    idx <- (segments[i] + 1):segments[i + 1]
    data.frame(
      variable = var_name,
      method = "regression",
      segment_id = i,
      segment_start_year = years[idx[1]],
      segment_end_year = years[idx[length(idx)]],
      intercept = params$beta[i, 1],
      slope = params$beta[i, 2],
      mean = mean(x[idx], na.rm = TRUE),
      sd = sd(x[idx], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

make_variance_segments <- function(x, years, var_name) {
  cp_var <- cpt.var(x, method = "PELT", penalty = "AIC")
  change_pts <- cpts(cp_var)

  if (length(change_pts) == 0) {
    return(data.frame(
      variable = var_name,
      method = "variance",
      segment_id = 1L,
      segment_start_year = min(years),
      segment_end_year = max(years),
      intercept = NA_real_,
      slope = NA_real_,
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    ))
  }

  segments <- c(1, change_pts + 1, length(x) + 1)
  rows <- lapply(seq_len(length(segments) - 1), function(i) {
    start_idx <- segments[i]
    end_idx <- segments[i + 1] - 1
    data.frame(
      variable = var_name,
      method = "variance",
      segment_id = i,
      segment_start_year = years[start_idx],
      segment_end_year = years[end_idx],
      intercept = NA_real_,
      slope = NA_real_,
      mean = mean(x[start_idx:end_idx], na.rm = TRUE),
      sd = sd(x[start_idx:end_idx], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

series_df <- bind_rows(lapply(names(series), function(var_name) {
  data.frame(
    variable = var_name,
    WY = series[[var_name]]$WY,
    value = series[[var_name]]$value,
    stringsAsFactors = FALSE
  )
}))

breakpoints_df <- bind_rows(lapply(names(series), function(var_name) {
  x <- series[[var_name]]$value
  years <- series[[var_name]]$WY

  reg_cp <- cpt.reg(cbind(x, 1, years), method = "PELT")
  reg_breaks <- cpts(reg_cp)
  reg_df <- if (length(reg_breaks) > 0) {
    data.frame(
      variable = var_name,
      method = "regression",
      bp_year = years[reg_breaks],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(variable = character(0), method = character(0), bp_year = numeric(0), stringsAsFactors = FALSE)
  }

  var_cp <- cpt.var(x, method = "PELT", penalty = "AIC")
  var_breaks <- cpts(var_cp)
  var_df <- if (length(var_breaks) > 0) {
    data.frame(
      variable = var_name,
      method = "variance",
      bp_year = years[var_breaks],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(variable = character(0), method = character(0), bp_year = numeric(0), stringsAsFactors = FALSE)
  }

  rbind(reg_df, var_df)
}))

segment_df <- bind_rows(lapply(names(series), function(var_name) {
  x <- series[[var_name]]$value
  years <- series[[var_name]]$WY
  reg_segs <- make_regression_segments(x, years, var_name)
  var_segs <- make_variance_segments(x, years, var_name)
  rbind(reg_segs, var_segs)
}))

results_df <- bind_rows(
  series_df %>% mutate(type = "series"),
  breakpoints_df %>% mutate(type = "breakpoint"),
  segment_df %>% mutate(type = "segment")
) %>%
  select(type, variable, method, WY, value, bp_year, segment_id, segment_start_year,
         segment_end_year, intercept, slope, mean, sd)

write.csv(results_df, file = "Data/msn_cpd_results.csv", row.names = FALSE)
