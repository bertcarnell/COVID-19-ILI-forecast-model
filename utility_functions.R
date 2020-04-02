# Utility functions

get_se_from_arima <- function(obj)
{
  assertthat::assert_that("forecast" %in% class(obj))
  # in the forecast:::forecast.Arima function the lower and upper are created by
  #   qq <- qnorm(0.5 * (1 + level[i]/100))
  #   lower[, i] <- pred$pred - qq * pred$se
  #   upper[, i] <- pred$pred + qq * pred$se
  as.numeric((obj$upper[,1] - obj$lower[,1]) / 2 / qnorm(0.5 * (1 + obj$level[1] / 100)))
}

create_result_from_arima_fcst <- function(obj)
{
  #obj <- arima2_fcst
  assertthat::assert_that("forecast" %in% class(obj))
  # point estimate on the logit scale
  point_est_logit_scale <- as.numeric(obj$mean)
  # point estimate on the percent scale
  point_est <- plogis(point_est_logit_scale) * 100
  # standard errors on the logit scale
  se_est_logit_scale <- get_se_from_arima(obj)
  assertthat::assert_that(length(point_est_logit_scale) == length(se_est_logit_scale))
  # bins on the percent scale
  bins <- c(seq(0.0, 25, by = 0.1), 100)
  bin_res <- matrix(NA, nrow = length(bins) - 1, ncol = length(obj$mean))
  for (j in 1:length(obj$mean))
  {
    for (i in 1:(length(bins) - 1))
    {
      bin_res[i, j] <- pnorm(qlogis(bins[i + 1] / 100), 
                             point_est_logit_scale[j], 
                             se_est_logit_scale[j]) -
        pnorm(qlogis(bins[i] / 100), 
              point_est_logit_scale[j], 
              se_est_logit_scale[j])
    }
  }
  return(list(point = point_est,
              bins = bins,
              p_bin = bin_res))
}

create_result_from_arima_fcst_plus <- function(obj, add_ili, mmwrweeks)
{
  #obj <- arima_result_list[[3]]$arima_fcst
  #add_ili <- c(covid_result_list[[3]]$covid_daily_state_post$pred_ili, covid_result_list[[3]]$covid_daily_state_pred$pred_ili)
  #mmwrweeks <- c(covid_result_list[[3]]$covid_daily_state_post$mmwrweek, covid_result_list[[3]]$covid_daily_state_pred$mmwrweek)
  assertthat::assert_that("forecast" %in% class(obj))
  
  # point estimate on the logit scale
  point_est_logit_scale <- as.numeric(obj$mean)
  # point estimate on the percent scale
  point_est <- plogis(point_est_logit_scale) * 100
  # this can go over 100%, so take the max
  point_est_new <- pmin(point_est + add_ili, 99.9)
  point_est_logit_scale_new <- qlogis(point_est_new / 100)
  # standard errors on the logit scale
  se_est_logit_scale <- get_se_from_arima(obj)
  assertthat::assert_that(length(point_est_logit_scale) == length(se_est_logit_scale))
  # bins on the percent scale
  bins <- c(seq(0.0, 25, by = 0.1), 100)
  bin_res <- matrix(NA, nrow = length(bins) - 1, ncol = length(obj$mean))
  for (j in 1:length(obj$mean))
  {
    for (i in 1:(length(bins) - 1))
    {
      bin_res[i, j] <- pnorm(qlogis(bins[i + 1] / 100), 
                             point_est_logit_scale_new[j], 
                             se_est_logit_scale[j]) -
        pnorm(qlogis(bins[i] / 100), 
              point_est_logit_scale_new[j], 
              se_est_logit_scale[j])
    }
  }
  return(list(point = point_est_new,
              bins = bins,
              p_bin = bin_res,
              mmwrweeks = mmwrweeks))
}
