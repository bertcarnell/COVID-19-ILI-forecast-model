create_result_from_arima_fcst_plus <- function(obj, # logit scale
                                               add_ili, # percent scale
                                               add_ili_se_log_scale,  # log scale
                                               mmwrweeks,
                                               actual_ili)
{
  # i <- 8
  #obj <- arima_result_list[[i]]$arima_fcst
  #add_ili <- c(covid_result_list[[i]]$covid_daily_state_post$pred_ili, covid_result_list[[i]]$covid_daily_state_pred$pred_ili)
  #add_ili_se_log_scale <- c(covid_result_list[[i]]$covid_daily_state_post$pred_ili_se, covid_result_list[[i]]$covid_daily_state_pred$pred_ili_se)
  #mmwrweeks <- c(covid_result_list[[i]]$covid_daily_state_post$mmwrweek, covid_result_list[[i]]$covid_daily_state_pred$mmwrweek)
  #actual_ili <- arima_result_list[[i]]$model_data_post$unweighted_ili
  assertthat::assert_that("forecast" %in% class(obj))
  
  # point estimate on the logit scale
  point_est_logit_scale <- as.numeric(obj$mean)
  # point estimate on the percent scale
  point_est <- plogis(point_est_logit_scale) * 100
  # this can go over 100%, so take the max
  point_est_new <- pmin(point_est + add_ili, 99.9)
  # difference with actuals
  diff_ili <- actual_ili / point_est_new[1:length(actual_ili)]
  last_diff <- ifelse(diff_ili[length(diff_ili)] > 0.001, diff_ili[length(diff_ili)], 0.001)
  # add the difference to the point estimates 
  point_est_new <- pmin(point_est_new * last_diff, 99.0)
  point_est_logit_scale_new <- qlogis(point_est_new / 100)
  # standard errors on the logit scale
  se_est_logit_scale <- get_se_from_arima(obj)
  assertthat::assert_that(length(point_est_logit_scale) == length(se_est_logit_scale))
  # if the regression undertainty is NA, then only include the ARIMA uncertainty
  if (any(is.na(add_ili_se_log_scale)))
  {
    # bins on the percent scale
    bin_res <- matrix(NA, nrow = length(reporting_bins) - 1, ncol = length(obj$mean))
    for (j in 1:length(obj$mean))
    {
      for (i in 1:(length(reporting_bins) - 1))
      {
        bin_res[i, j] <- pnorm(qlogis(reporting_bins[i + 1] / 100), 
                               point_est_logit_scale_new[j], 
                               se_est_logit_scale[j]) -
          pnorm(qlogis(reporting_bins[i] / 100), 
                point_est_logit_scale_new[j], 
                se_est_logit_scale[j])
      }
    }
  } else
  {
    N <- 100000
    # simulate regression estimate and uncertainty estimate
    arima_logit_scale <- matrix(NA, nrow = N, ncol = length(point_est_logit_scale))
    for (i in seq_along(point_est_logit_scale))
    {
      arima_logit_scale[, i] <- plogis(rnorm(N, point_est_logit_scale, se_est_logit_scale)) * 100 +
        exp(rnorm(N, log(add_ili / 100), add_ili_se_log_scale)) * 100
      arima_logit_scale[, i] <- pmin(arima_logit_scale[, i] * last_diff, 99.9)
    }
    bin_res <- apply(arima_logit_scale, 2, function(z) hist(z, breaks = reporting_bins, plot = FALSE)$counts)
    bin_res_sum <- apply(bin_res, 2, sum)
    bin_res <- t(t(bin_res) / bin_res_sum)
  }
  # simulate the uncertainty distribution
  return(list(point = point_est_new,
              point_logit_scale = point_est_logit_scale_new,
              p_bin = bin_res,
              mmwrweeks = mmwrweeks))
}

