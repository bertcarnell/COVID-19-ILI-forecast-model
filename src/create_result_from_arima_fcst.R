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
  bin_res <- matrix(NA, nrow = length(reporting_bins) - 1, ncol = length(obj$mean))
  for (j in 1:length(obj$mean))
  {
    for (i in 1:(length(reporting_bins) - 1))
    {
      bin_res[i, j] <- pnorm(qlogis(reporting_bins[i + 1] / 100), 
                             point_est_logit_scale[j], 
                             se_est_logit_scale[j]) -
        pnorm(qlogis(reporting_bins[i] / 100), 
              point_est_logit_scale[j], 
              se_est_logit_scale[j])
    }
  }
  return(list(point = point_est,
              p_bin = bin_res))
}

