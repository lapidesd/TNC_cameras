library(dplyr)
library(changepoint)
library(ggplot2)
library(patchwork)
library(cowplot)

root <- if (dir.exists("Data")) "." else ".."
out_dir <- file.path(root, "Figures", "CPD")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

START_WY <- 2006
END_WY <- 2025
N_PERM <- 9999
TAG_X <- c(0.041, 0.535)
TAG_Y <- c(0.8775, 0.4475)
MIN_COVERAGE_MEAN <- 0.90
MIN_COVERAGE_TOTAL <- 0.98
set.seed(42)

cameras <- data.frame(
  camera = c(1, 2, 4, 6, 7),
  site = c("Fairbank", "CharlestonMesquite", "Hunter", "Contention", "St.David"),
  name = c("Fairbank", "Charleston Mesquite", "Hunter", "Contention", "St. David"),
  stringsAsFactors = FALSE
)

season_labels <- c("Cool-Season Recharge", "Pre-Monsoon Dry", "Monsoon", "Post-Monsoon Recession")

variables <- list(
  et = list(title = "Evapotranspiration", ylab = "ET (mm)", color = "forestgreen"),
  temp = list(title = "Mean Temperature", ylab = "T (°C)", color = "firebrick"),
  fp = list(title = "Mean Flow Persistence", ylab = "Mean FP", color = "#1F77B4"),
  precip = list(title = "Precipitation", ylab = "P (mm)", color = "#800080"),
  vpd = list(title = "Mean Vapor Pressure Deficit", ylab = "VPD (kPa)", color = "#FFA500"),
  gw = list(title = "Minimum Height Above Lowest Groundwater Level", ylab = "ΔGW Above Min (mm)", color = "saddlebrown")
)

water_year <- function(date) {
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  ifelse(m >= 10, y + 1L, y)
}

season_of <- function(date) {
  c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 1)[as.integer(format(date, "%m"))]
}

calendar <- data.frame(date = seq(as.Date("2005-10-01"), as.Date("2026-09-30"), by = "day"))
calendar$WY <- water_year(calendar$date)
calendar$Season <- season_of(calendar$date)
season_info <- calendar %>%
  group_by(WY, Season) %>%
  summarise(expected = n(), season_end = max(date), .groups = "drop")

seasonal_aggregate <- function(daily, statistic, min_coverage, complete_through = NULL) {
  agg <- switch(statistic, mean = mean, total = sum, min = min)
  out <- daily %>%
    filter(!is.na(value)) %>%
    mutate(WY = water_year(date), Season = season_of(date)) %>%
    group_by(camera, WY, Season) %>%
    summarise(n = n(), value = agg(value), .groups = "drop") %>%
    left_join(season_info, by = c("WY", "Season")) %>%
    filter(n / expected >= min_coverage, WY >= START_WY, WY <= END_WY)
  if (statistic == "total") out$value <- out$value * out$expected / out$n
  if (!is.null(complete_through)) out <- out %>% filter(season_end <= complete_through)
  out %>% select(camera, WY, Season, value) %>% arrange(camera, Season, WY)
}

fp_daily <- read.csv(file.path(root, "Data", "PredFP.csv"), stringsAsFactors = FALSE) %>%
  select(-camera) %>%
  inner_join(cameras, by = "site") %>%
  transmute(camera, date = as.Date(date), value = fp_final)

daymet <- read.csv(file.path(root, "Data", "Daymet2006_2025.csv"), stringsAsFactors = FALSE)
daymet$date <- as.Date(daymet$date, format = "%m/%d/%Y")
daymet <- daymet[daymet$camera %in% cameras$camera, ]
temp_daily <- with(daymet, data.frame(camera, date, value = (tmax + tmin) / 2))
precip_daily <- with(daymet, data.frame(camera, date, value = prcp))
vpd_daily <- with(daymet, data.frame(camera, date, value = tvpd))

nagler_raw <- read.csv(
  file.path(root, "Data", "ET", "NaglerET.csv"),
  header = FALSE, skip = 7, stringsAsFactors = FALSE, check.names = FALSE
)
nagler_columns <- data.frame(
  site = c("Fairbank", "CharlestonMesquite", "Hunter", "Contention", "St.David"),
  date_col = c(4, 4, 4, 55, 55),
  et_col = c(37, 25, 13, 58, 64)
)
nagler_daily_series <- function(dates, rate) {
  keep <- !is.na(dates) & !is.na(rate)
  dates <- dates[keep]
  rate <- rate[keep]
  o <- order(dates)
  dates <- as.numeric(dates[o])
  rate <- rate[o]
  days <- seq(min(dates) - 8, max(dates) + 8, by = 1)
  midpoints <- (dates[-1] + dates[-length(dates)]) / 2
  data.frame(date = as.Date(days, origin = "1970-01-01"), value = rate[findInterval(days, midpoints) + 1])
}
et_daily <- bind_rows(lapply(seq_len(nrow(cameras)), function(i) {
  cols <- nagler_columns[nagler_columns$site == cameras$site[i], ]
  dates <- as.Date(nagler_raw[[cols$date_col]], format = "%m/%d/%Y")
  rate <- suppressWarnings(as.numeric(nagler_raw[[cols$et_col]]))
  cbind(camera = cameras$camera[i], nagler_daily_series(dates, rate))
}))

gw <- read.csv(file.path(root, "Data", "GW", "CamSPRNCA_GW.csv"), stringsAsFactors = FALSE)
gw <- gw[gw$camera %in% cameras$camera, ]
gw_daily <- with(gw, data.frame(camera, date = as.Date(datetime), value = height_abv_lowest_ft * 304.8))

series <- list(
  et = seasonal_aggregate(et_daily, "total", MIN_COVERAGE_TOTAL),
  temp = seasonal_aggregate(temp_daily, "mean", MIN_COVERAGE_MEAN),
  fp = seasonal_aggregate(fp_daily, "mean", MIN_COVERAGE_MEAN),
  precip = seasonal_aggregate(precip_daily, "total", MIN_COVERAGE_TOTAL),
  vpd = seasonal_aggregate(vpd_daily, "mean", MIN_COVERAGE_MEAN),
  gw = seasonal_aggregate(gw_daily, "min", 0, complete_through = max(gw_daily$date))
)

permutation_slope_p <- function(x, y, n_perm = N_PERM) {
  xc <- x - mean(x)
  denom <- sum(xc^2)
  observed <- abs(sum(xc * y) / denom)
  permuted <- replicate(n_perm, abs(sum(xc * sample(y)) / denom))
  (sum(permuted >= observed - 1e-12 * max(1, observed)) + 1) / (n_perm + 1)
}

find_change_points <- function(n, detect) {
  if (n < 6) return(integer(0))
  cps <- tryCatch(suppressWarnings(detect()), error = function(e) integer(0))
  as.integer(cps[cps >= 1 & cps < n])
}

analyze_series <- function(x, y) {
  n <- length(y)
  reg_cps <- find_change_points(n, function() cpts(cpt.reg(cbind(y, 1, x), method = "PELT")))
  var_cps <- find_change_points(n, function() cpts(cpt.var(y, method = "PELT", penalty = "AIC")))
  boundary_x <- function(cps) (x[cps] + x[cps + 1]) / 2

  reg_ends <- c(reg_cps, n)
  reg_starts <- c(1, reg_cps + 1)
  regression <- bind_rows(lapply(seq_along(reg_starts), function(i) {
    idx <- reg_starts[i]:reg_ends[i]
    if (length(idx) < 2) return(NULL)
    xs <- x[idx]
    ys <- y[idx]
    slope <- sum((xs - mean(xs)) * (ys - mean(ys))) / sum((xs - mean(xs))^2)
    intercept <- mean(ys) - slope * mean(xs)
    data.frame(
      segment = i, x0 = min(xs), x1 = max(xs), n = length(idx),
      intercept = intercept, slope = slope,
      p = if (length(idx) >= 3) permutation_slope_p(xs, ys) else NA_real_
    ) %>% mutate(y0 = intercept + slope * x0, y1 = intercept + slope * x1)
  }))

  var_starts <- c(1, var_cps + 1)
  var_ends <- c(var_cps, n)
  var_edges_lo <- c(x[1], boundary_x(var_cps))
  var_edges_hi <- c(boundary_x(var_cps), x[n])
  variance <- bind_rows(lapply(seq_along(var_starts), function(i) {
    ys <- y[var_starts[i]:var_ends[i]]
    if (length(ys) < 2) return(NULL)
    data.frame(
      segment = i, xmin = var_edges_lo[i], xmax = var_edges_hi[i], n = length(ys),
      mean = mean(ys), sd = sd(ys)
    )
  }))

  list(
    regression = regression, variance = variance,
    reg_breaks = boundary_x(reg_cps), var_breaks = boundary_x(var_cps)
  )
}

y_limits <- function(y, is_fp) {
  if (is_fp) return(c(-0.05, 1.05))
  span <- max(max(y) - min(y), 1e-9)
  c(min(y) - 0.10 * span, max(y) + 0.15 * span)
}

panel_plot <- function(df, fit, spec, title, is_fp) {
  color <- spec$color
  p <- ggplot(df, aes(WY, value))
  if (nrow(fit$variance) > 0) {
    p <- p + geom_rect(
      data = fit$variance,
      aes(xmin = xmin, xmax = xmax, ymin = mean - sd, ymax = mean + sd),
      inherit.aes = FALSE, fill = color, alpha = 0.18
    )
  }
  breaks <- c(fit$reg_breaks, fit$var_breaks)
  if (length(breaks) > 0) {
    p <- p + geom_vline(xintercept = breaks, linetype = "dashed", linewidth = 0.4, alpha = 0.6)
  }
  if (nrow(fit$regression) > 0) {
    lines <- fit$regression %>% mutate(lty = ifelse(!is.na(p) & p < 0.05, "solid", "dashed"))
    p <- p + geom_segment(
      data = lines, aes(x = x0, xend = x1, y = y0, yend = y1, linetype = lty),
      inherit.aes = FALSE, color = color, linewidth = 1
    ) + scale_linetype_identity()
  }
  p +
    geom_point(shape = 21, size = 3, fill = color, color = "black", stroke = 0.6) +
    scale_x_continuous(breaks = scales::breaks_width(2), expand = expansion(add = 0.6)) +
    scale_y_continuous(expand = expansion(0), breaks = function(l) {
      b <- scales::breaks_pretty(5)(l)
      b[b >= 0 & b <= 1e9]
    }) +
    coord_cartesian(ylim = y_limits(df$value, is_fp), clip = "on") +
    labs(x = NULL, y = spec$ylab, title = title) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 16),
      axis.text = element_text(color = "black"),
      panel.grid = element_blank()
    )
}

results <- list()

for (var in names(variables)) {
  spec <- variables[[var]]
  for (i in seq_len(nrow(cameras))) {
    cam <- cameras$camera[i]
    panels <- list()
    for (s in 1:4) {
      df <- series[[var]] %>% filter(camera == cam, Season == s) %>% arrange(WY)
      fit <- analyze_series(df$WY, df$value)
      panels[[s]] <- panel_plot(df, fit, spec, season_labels[s], is_fp = var == "fp")

      results[[length(results) + 1]] <- bind_rows(
        df %>% transmute(type = "series", camera, variable = var, season = s, WY, value),
        if (length(c(fit$reg_breaks, fit$var_breaks)) > 0) {
          data.frame(
            type = "breakpoint", camera = cam, variable = var, season = s,
            method = rep(c("regression", "variance"), c(length(fit$reg_breaks), length(fit$var_breaks))),
            bp_year = c(fit$reg_breaks, fit$var_breaks)
          )
        },
        if (nrow(fit$regression) > 0) {
          fit$regression %>% transmute(
            type = "regression_segment", camera = cam, variable = var, season = s,
            segment_id = segment, segment_start_year = x0, segment_end_year = x1,
            n, intercept, slope, p_permutation = p
          )
        },
        if (nrow(fit$variance) > 0) {
          fit$variance %>% transmute(
            type = "variance_segment", camera = cam, variable = var, season = s,
            segment_id = segment, segment_start_year = xmin, segment_end_year = xmax, n, mean, sd
          )
        }
      )
    }

    figure <- wrap_plots(panels, nrow = 2, ncol = 2) +
      plot_annotation(
        title = sprintf("%s (C%d)", cameras$name[i], cam),
        subtitle = sprintf("Seasonal %s Change-Points", spec$title),
        theme = theme(
          plot.title = element_text(size = 18, hjust = 0.5),
          plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 25)),
          plot.margin = margin(t = 19, r = 5.5, b = 20, l = 5.5)
        )
      )

    canvas <- ggdraw(patchworkGrob(figure))
    for (k in 1:4) {
      canvas <- canvas + draw_label(
        paste0(letters[k], ")"), x = TAG_X[(k - 1) %% 2 + 1], y = TAG_Y[(k - 1) %/% 2 + 1],
        hjust = 0, vjust = 0.5, size = 13
      )
    }
    if (var == "fp") canvas <- canvas + draw_label("*GF", x = 0.99, y = 0.012, hjust = 1, vjust = 0, size = 12)

    file <- file.path(out_dir, sprintf("%s_cpd_c%d.png", var, cam))
    ggsave(file, canvas, width = 13, height = 10, dpi = 300, bg = "white", device = ragg::agg_png)
    cat(sprintf("saved %s\n", basename(file)))
  }
}

write.csv(bind_rows(results), file.path(root, "Data", "permutation_cpd_results.csv"), row.names = FALSE)
