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

seasonal_cameras <- c(1, 2, 4, 6, 7)
seasonal_wy <- 2007:2025
season_names <- c(`4` = "PMR", `1` = "CSR", `2` = "PMD", `3` = "MSN")
season_position <- c(`4` = 0, `1` = 1, `2` = 2, `3` = 3)

add_seasons <- function(df, dates) {
  df$WY <- water_year(dates)
  df$Season <- sapply(as.integer(format(dates, "%m")), season)
  df
}

pred_fp <- read.csv("Data/SPRNCAPredFP.csv", stringsAsFactors = FALSE)
pred_fp$date <- as.Date(pred_fp$date)
pred_fp <- add_seasons(pred_fp, pred_fp$date) %>% left_join(camera_map, by = c("site" = "Site"))

seasonal_fp <- pred_fp %>%
  filter(Camera %in% seasonal_cameras) %>%
  group_by(Camera, WY, Season) %>%
  summarise(value = mean(fp_final, na.rm = TRUE), .groups = "drop")

decimal_year <- function(dates) {
  y <- as.numeric(format(dates, "%Y"))
  jan1 <- as.Date(paste0(y, "-01-01"))
  y + as.numeric(dates - jan1) / as.numeric(as.Date(paste0(y + 1, "-01-01")) - jan1)
}

raw_start <- as.Date("2006-10-01")
raw_end <- as.Date("2025-09-30")

raw_climate <- read.csv("Data/Daymet2006_2025.csv", stringsAsFactors = FALSE)
raw_climate$date <- as.Date(raw_climate$date, format = "%m/%d/%Y")
raw_climate <- raw_climate %>% filter(camera %in% seasonal_cameras)

nagler_raw <- read.csv("Data/ET/NaglerET.csv", header = FALSE, skip = 7, stringsAsFactors = FALSE)
nagler_cols <- list(Fairbank = c(4, 37), CharlestonMesquite = c(4, 25), Hunter = c(4, 13),
                    Contention = c(55, 58), St.David = c(55, 64))
raw_et <- bind_rows(lapply(names(nagler_cols), function(site) {
  cols <- nagler_cols[[site]]
  data.frame(
    Site = site,
    date = as.Date(nagler_raw[[cols[1]]], format = "%m/%d/%Y") + 8,
    value = suppressWarnings(as.numeric(nagler_raw[[cols[2]]])),
    stringsAsFactors = FALSE
  )
})) %>%
  left_join(camera_map, by = "Site")

raw_gw <- read.csv("Data/GW/CamSPRNCA_GW.csv", stringsAsFactors = FALSE) %>%
  filter(camera %in% seasonal_cameras) %>%
  mutate(date = as.Date(datetime), gw_mm = height_abv_lowest_ft * 0.3048 * 1000) %>%
  group_by(Camera = camera, date) %>%
  summarise(value = min(gw_mm, na.rm = TRUE), .groups = "drop")

raw_series_df <- bind_rows(
  pred_fp %>% filter(Camera %in% seasonal_cameras) %>% transmute(Camera = as.character(Camera), date, value = fp_final, variable = "FP_daily"),
  raw_climate %>% transmute(Camera = as.character(camera), date, value = prcp, variable = "precip"),
  raw_climate %>% transmute(Camera = as.character(camera), date, value = tvpd, variable = "vpd"),
  raw_et %>% transmute(Camera = as.character(Camera), date, value, variable = "et"),
  raw_gw %>% transmute(Camera = as.character(Camera), date, value, variable = "gw")
) %>%
  filter(date >= raw_start, date <= raw_end, is.finite(value)) %>%
  mutate(
    WY = water_year(date),
    season = unname(season_names[as.character(sapply(as.integer(format(date, "%m")), season))]),
    t = decimal_year(date)
  )

seasonal_series_df <- seasonal_fp %>%
  filter(WY %in% seasonal_wy, is.finite(value)) %>%
  transmute(
    Camera = as.character(Camera), date = as.Date(NA), value, variable = "FP", WY,
    season = unname(season_names[as.character(Season)]),
    t = WY - 0.25 + 0.25 * season_position[as.character(Season)]
  ) %>%
  bind_rows(raw_series_df) %>%
  arrange(variable, Camera, t)

seasonal_pen <- "AIC"
seasonal_minseglen <- function(t) max(3, round(4 * length(t) / diff(range(t))))

seasonal_breaks <- function(x, t, method) {
  msl <- seasonal_minseglen(t)
  cp <- tryCatch(
    switch(method,
      regression = cpt.reg(cbind(x, 1, t), method = "PELT", penalty = seasonal_pen, minseglen = msl),
      variance = cpt.var(x, method = "PELT", penalty = seasonal_pen, minseglen = msl),
      mean = cpt.mean(x / sd(x), method = "PELT", penalty = seasonal_pen, minseglen = msl)
    ),
    error = function(e) NULL
  )
  if (is.null(cp)) return(integer(0))
  cpts(cp)
}

seasonal_breakpoints_df <- seasonal_series_df %>%
  group_by(variable, Camera) %>%
  group_modify(function(df, key) {
    bind_rows(lapply(c("regression", "variance", "mean"), function(m) {
      idx <- seasonal_breaks(df$value, df$t, m)
      data.frame(method = rep(m, length(idx)), bp_t = df$t[idx], bp_date = df$date[idx],
                 WY = df$WY[idx], season = df$season[idx], stringsAsFactors = FALSE)
    }))
  }) %>%
  ungroup()

seasonal_results_df <- bind_rows(
  seasonal_series_df %>% mutate(type = "series"),
  seasonal_breakpoints_df %>% mutate(type = "breakpoint")
) %>%
  select(type, variable, Camera, method, WY, season, date, t, value, bp_t, bp_date)

write.csv(seasonal_results_df, file = "Data/seasonal_cpd_results.csv", row.names = FALSE)
