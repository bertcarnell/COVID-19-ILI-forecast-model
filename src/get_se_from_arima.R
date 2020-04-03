get_se_from_arima <- function(obj)
{
  assertthat::assert_that("forecast" %in% class(obj))
  # in the forecast:::forecast.Arima function the lower and upper are created by
  #   qq <- qnorm(0.5 * (1 + level[i]/100))
  #   lower[, i] <- pred$pred - qq * pred$se
  #   upper[, i] <- pred$pred + qq * pred$se
  as.numeric((obj$upper[,1] - obj$lower[,1]) / 2 / qnorm(0.5 * (1 + obj$level[1] / 100)))
}

