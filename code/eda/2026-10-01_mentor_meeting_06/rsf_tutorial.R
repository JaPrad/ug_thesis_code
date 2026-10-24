# Author: Jyotiraditya Pradhan
# Date: October 12, 2026

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  randomForestSRC
)
data(peakVO2, package = "randomForestSRC")

dta <- tibble(peakVO2)
glimpse(dta)
dta |>
  select(ttodead, died)
obj <- rfsrc(
  Surv(ttodead, died) ~ .,
  dta,
  ntree = 100,
  nodesize = 5,
  nsplit = 50,
  importance = TRUE
)
print(obj)

obj$predicted


newdata <- data.frame(lapply(1:ncol(obj$xvar), function(i) {
  median(obj$xvar[, i])
}))
colnames(newdata) <- obj$xvar.names
newdata1 <- newdata2 <- newdata
newdata1[, which(obj$xvar.names == "peak.vo2")] <- quantile(
  obj$xvar$peak.vo2,
  0.25
)
newdata2[, which(obj$xvar.names == "peak.vo2")] <- quantile(
  obj$xvar$peak.vo2,
  0.75
)
newdata <- rbind(newdata1, newdata2)
y.pred <- predict(obj, newdata = rbind(newdata, obj$xvar)[1:2, ])


plot(
  round(y.pred$time.interest, 2),
  y.pred$survival[1, ],
  type = "l",
  xlab = "Time (Year)",
  ylab = "Survival"
)
lines(
  round(y.pred$time.interest, 2),
  y.pred$survival[2, ],
  col = 2,
  lty = 2,
  lwd = 2
)
legend(
  "topright",
  legend = c("Peak VO2=12.8", "Peak VO2=19.3"),
  col = c(1:2)
)
dev.off()
