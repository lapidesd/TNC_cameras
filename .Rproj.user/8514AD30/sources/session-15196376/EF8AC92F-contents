library(dplyr)
library(changepoint)
library(ggplot2)

flow <- read.csv("USPPFlowMonitoring2006_2025.csv")

flow$Flow.Code[flow["Flow.Code"] == 2] <- 1
flow <- flow[flow$Flow.Code %in% c(0, 1),]
flow$Date <- as.Date(flow$Date)

names <- data.frame(Site = c("Boquillas", "CharlestonMesquite", "Contention", "Fairbank", "Hereford", "Hunter", "Moson", "St.David"),
                    Camera = c(8, 2, 6, 1, 5, 4, 3, 7))
flow <- left_join(flow, names, by = "Site")

water_year <- function(date) {
  y <- as.numeric(format(date, "%Y"))
  m <- as.numeric(format(date, "%m"))
  ifelse(m >= 10, y + 1, y)
}
flow$WY <- water_year(flow$Date)

season <- function(month) {
  if (month %in% c(12, 1, 2, 3)) return(1)
  else if (month %in% c(4, 5, 6)) return(2)
  else if (month %in% c(7, 8, 9)) return(3)
  else if (month %in% c(10, 11)) return(4)
}
flow$Season <- sapply(as.integer(format(flow$Date, "%m")), season)

reach <- flow[flow$Camera %in% c(1, 2, 6, 7),]
reach <- aggregate(Flow.Code ~ WY + Season, data = reach, FUN = mean)

labels <- c("Cool-Season Recharge", "Pre-Monsoon Dry", "Monsoon", "Post-Monsoon Recession")

par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
for (s in 1:4) {
  subset <- reach[reach$Season == s,]
  matrix <- cbind(subset$Flow.Code, 1, subset$WY)
  cp_reg <- cpt.reg(matrix, method = "PELT")
  segments <- c(0, cpts(cp_reg), nrow(subset))
  params <- param.est(cp_reg)
  plot(subset$WY, subset$Flow.Code,
       pch = 19, col = "black",
       xlab = "", ylab = "Mean FP", main = labels[s], font.main = 1)
  colors <- rainbow(length(segments) - 1)
  for (i in 1:length(colors)) {
    idx <- (segments[i] + 1):segments[i + 1]
    intercept <- params$beta[i, 1]
    slope <- params$beta[i, 2]
    fitted <- intercept + slope * subset$WY[idx]
    lines(subset$WY[idx], fitted, col = colors[i], lwd = 2)
  }
}
mtext("Seasonal Mean FP Regression Change Points", side = 3, outer = TRUE)

par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
for (s in 1:4) {
  subset <- reach[reach$Season == s,]
  cp_mean <- cpt.mean(subset$Flow.Code, method = "PELT")
  plot(subset$WY, subset$Flow.Code,
       pch = 19, col = "black",
       xlab = "", ylab = "Mean FP", main = labels[s], font.main = 1)
  years <- subset$WY[cpts(cp_mean)]
  segments <- c(0, cpts(cp_mean), nrow(subset))
  means <- sapply(1:(length(segments) - 1), function(i) {
    start <- segments[i] + 1
    end <- segments[i + 1]
    mean(subset$Flow.Code[start:end])
  })
  sp <- data.frame(
    x = subset$WY[head(segments, -1) + 1],
    xend = subset$WY[tail(segments, -1)],
    y = means,
    yend = means
  )
  segments(sp$x, sp$y, sp$xend, sp$yend, col = "red", lwd = 2)
}
mtext("Seasonal Mean FP Change Points", side = 3, outer = TRUE)

par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
for (s in 1:4) {
  subset <- reach[reach$Season == s, ]
  cp_var <- cpt.var(subset$Flow.Code, method = "PELT", penalty = "AIC")
  segments <- c(1, cpts(cp_var) + 1, nrow(subset) + 1)
  plot(subset$WY, subset$Flow.Code,
       pch = 19, col = "black",
       xlab = "", ylab = "Mean FP", main = labels[s], font.main = 1)
  for (i in 1:(length(segments) - 1)) {
    j <- segments[i]:(segments[i + 1] - 1)
    left  <- subset$WY[segments[i]]
    right <- subset$WY[segments[i + 1] - 1]
    sm <- mean(subset$Flow.Code[j])
    ssd   <- sd(subset$Flow.Code[j])
    bottom <- sm - ssd
    top    <- sm + ssd
    rect(left, bottom,
         right, top,
         col = adjustcolor("skyblue", alpha.f = 0.3),
         border = NA)
  }
}
mtext("Seasonal FP Variance Change Points", side = 3, outer = TRUE)

daymet <- read.csv("Daymet2006_2024.csv")
daymet$date <- as.Date(daymet$date)
daymet$wy <- water_year(daymet$date)
daymet$season <- sapply(as.integer(format(daymet$date, "%m")), season)
daymet$temp <- (daymet$tmax + daymet$tmin) / 2
daymet <- daymet[daymet$camera %in% c(1, 2, 6, 7),]

seasonal <- daymet %>%
  group_by(wy, season) %>%
  summarise(
    precip = sum(prcp, na.rm = TRUE),
    vpd = mean(tvpd, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    .groups = "drop"
)

cp_reg <- function(c, df) {
  var <- c("Precipitation", "Vapor Pressure Deficit", "Temperature")
  ylabs <- c("Precipitation (mm)", "VPD (kPa)", "Temperature (°C)")
  labels <- c("Cool-Season Recharge", "Pre-Monsoon Dry", "Monsoon", "Post-Monsoon Recession")
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  for (s in 1:4) {
    subset <- df[df$season == s,]
    matrix <- cbind(subset[[c]], 1, subset$wy)
    cp_reg <- cpt.reg(matrix, method = "PELT")
    segments <- c(0, cpts(cp_reg), nrow(subset))
    params <- param.est(cp_reg)
    plot(subset$wy, subset[[c]],
         pch = 19, col = "black",
         xlab = "", ylab = ylabs[c - 2], main = labels[s], font.main = 1)
    colors <- rainbow(length(segments) - 1)
    for (i in 1:length(colors)) {
      idx <- (segments[i] + 1):segments[i + 1]
      intercept <- params$beta[i, 1]
      slope <- params$beta[i, 2]
      fitted <- intercept + slope * subset$wy[idx]
      lines(subset$wy[idx], fitted, col = colors[i], lwd = 2)
    }
    mtext(paste("Seasonal", var[c - 2], "Regression Change Points"), side = 3, outer = TRUE)
  }
}

cp_mean <- function(c, df) {
  var <- c("Precipitation", "Vapor Pressure Deficit", "Temperature")
  ylabs <- c("Precipitation (mm)", "VPD (kPa)", "Temperature (°C)")
  labels <- c("Cool-Season Recharge", "Pre-Monsoon Dry", "Monsoon", "Post-Monsoon Recession")
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  for (s in 1:4) {
    subset <- df[df$season == s,]
    cp_mean <- cpt.mean(subset[[c]], method = "PELT")
    plot(subset$wy, subset[[c]],
         pch = 19, col = "black",
         xlab = "", ylab = ylabs[c - 2], main = labels[s], font.main = 1)
    years <- subset$wy[cpts(cp_mean)]
    segments <- c(0, cpts(cp_mean), nrow(subset))
    means <- sapply(1:(length(segments) - 1), function(i) {
      start <- segments[i] + 1
      end <- segments[i + 1]
      mean(subset[[c]][start:end])
    })
    sp <- data.frame(
      x = subset$wy[head(segments, -1) + 1],
      xend = subset$wy[tail(segments, -1)],
      y = means,
      yend = means
    )
    segments(sp$x, sp$y, sp$xend, sp$yend, col = "red", lwd = 2)
  }
  mtext(paste("Seasonal", var[c - 2], "Change Points"), side = 3, outer = TRUE)
}

cp_var <- function(c, df) {
  var <- c("Precipitation", "Vapor Pressure Deficit", "Temperature")
  ylabs <- c("Precipitation (mm)", "VPD (kPa)", "Temperature (°C)")
  labels <- c("Cool-Season Recharge", "Pre-Monsoon Dry", "Monsoon", "Post-Monsoon Recession")
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  for (s in 1:4) {
    subset <- df[df$season == s, ]
    cp_var <- cpt.var(subset[[c]], method = "PELT", penalty = "AIC")
    segments <- c(1, cpts(cp_var) + 1, nrow(subset) + 1)
    plot(subset$wy, subset[[c]],
         pch = 19, col = "black",
         xlab = "", ylab = ylabs[c - 2], main = labels[s], font.main = 1)
    for (i in 1:(length(segments) - 1)) {
      j <- segments[i]:(segments[i + 1] - 1)
      left <- subset$wy[segments[i]]
      right <- subset$wy[segments[i + 1] - 1]
      sm <- mean(subset[[c]][j])
      ssd <- sd(subset[[c]][j])
      bottom <- sm - ssd
      top <- sm + ssd
      rect(left, bottom,
           right, top,
           col = adjustcolor("skyblue", alpha.f = 0.3),
           border = NA)
    }
  }
  mtext(paste("Seasonal", var[c - 2], "Variance Change Points"), side = 3, outer = TRUE)
}