create_result_from_arima_fcst_plus <- function(obj, add_ili, mmwrweeks)
{
  #obj <- arima_result_list[[1]]$arima_fcst
  #add_ili <- c(covid_result_list[[1]]$covid_daily_state_post$pred_ili, covid_result_list[[1]]$covid_daily_state_pred$pred_ili)
  #mmwrweeks <- c(covid_result_list[[1]]$covid_daily_state_post$mmwrweek, covid_result_list[[1]]$covid_daily_state_pred$mmwrweek)
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
  return(list(point = point_est_new,
              point_logit_scale = point_est_logit_scale_new,
              p_bin = bin_res,
              mmwrweeks = mmwrweeks))
}

