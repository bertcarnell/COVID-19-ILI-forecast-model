state_arima_build <- function(model_state, model_data, 
                              saved_arima_object_list)
{
  # model_state <- hhs_regions_map$state[36]
  # model_data <- subset(stateflu3, stateflu3$region == model_state)
  # saved_arima_object_list <- arima_model_list[[36]]
  arima2 <- saved_arima_object_list$arima_model
  model_data_pre <- saved_arima_object_list$model_data_pre
  
  # create model_data_post again for new data
  all_confirmed_ind <- which(model_data$active > 0 & model_data$year == 2020)
  if (length(all_confirmed_ind) == 0)
  {
    split_ind <- nrow(model_data)
    model_data_post <- model_data[nrow(model_data),]
  } else {
    split_ind <- min(which(model_data$active > 0 & model_data$year == 2020))
    assertthat::assert_that(length(split_ind) == 1, 
                            msg = paste0("Error in state splitting 2", model_state))
    model_data_post <- model_data[split_ind:nrow(model_data),]
  }
  
  # forecast 6 weeks ahead of today plus predict weeks since first active case
  arima2_fcst <- forecast::forecast(arima2, h = nrow(model_data_post) + WEEKS_AHEAD, 
                                    level = c(0.90))
  fcst_weeks <- c(model_data_post$week, max(model_data_post$week) + 1:WEEKS_AHEAD)
  arima2_fcst_result <- create_result_from_arima_fcst(arima2_fcst)
  
  assertthat::assert_that(all(abs(apply(arima2_fcst_result$p_bin, 2, sum) - 1) < 1E-6))
  
  model_data_post$pred_unw_ili <- arima2_fcst_result$point[1:nrow(model_data_post)]
  model_data_post <- within(model_data_post, 
                            resid_unw_ili <- pred_unw_ili - unweighted_ili)
  # relate the number of active COVID patients to the ILI residual
  #   (Active COVID Patients) * (percent that need provider care) * 
  #         (percent of COVID's that need care in the ILI sentinels) / (total patients) * 100
  # wrap all that up in one inflation factor

  model_data_post$inflation <- with(model_data_post, (ilitotal - min(unweighted_ili, pred_unw_ili) / 100 * total_patients) / active)

  # since the inflation factor is dropping as cases get higher, use the most recent ones
  if (nrow(model_data_post) > 3)
  {
    ind_last_3 <- (nrow(model_data_post) - 3 + 1):nrow(model_data_post)
    mean_inflation <- pmax(1, mean(model_data_post$inflation[ind_last_3]))
  } else
  {
    mean_inflation <- pmax(1, model_data_post$inflation[nrow(model_data_post)])
  }

  return(list(mean_inflation = mean_inflation,
              model_data_post = model_data_post,
              model_data_pre = model_data_pre,
              arima = arima2,
              arima_fcst = arima2_fcst,
              arima_fcst_result = arima2_fcst_result))
}

