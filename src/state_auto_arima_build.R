state_auto_arima_build <- function(model_state, model_data)
{
  # model_state <- hhs_regions_map$state[51]
  # model_data <- subset(stateflu3, stateflu3$region == model_state)
  
  # find the first mmwrweek where confirmed is > 0
  all_confirmed_ind <- which(model_data$confirmed > 0 & model_data$year == 2020)
  if (length(all_confirmed_ind) == 0)
  {
    split_ind <- nrow(model_data)
    model_data_pre <- model_data[1:(split_ind - 1),]
    model_data_post <- model_data[nrow(model_data),]
  } else {
    split_ind <- min(which(model_data$confirmed > 0 & model_data$year == 2020))
    assertthat::assert_that(length(split_ind) == 1, 
                            msg = paste0("Error in state splitting", model_state))
    
    model_data_pre <- model_data[1:(split_ind - 1),]
    model_data_post <- model_data[split_ind:nrow(model_data),]
  }
  model_data_pre <- within(model_data_pre, 
                           patient_per_provider <- total_patients / num_of_providers)
  
  # data is on [0,100] on a percent scale
  #  translate to [0,1] with a logistic
  #  qlogis is ln(p / (1-p))
  # since unweighted_ili can be zero, qlogis will produce NA
  #  add a level just inside the minimum detectable bin [0, 0.1] / 100 = 0.0005
  model_data_pre$unweighted_ili_modeling <- model_data_pre$unweighted_ili / 100
  ind <- which(model_data_pre$unweighted_ili == 0)
  model_data_pre$unweighted_ili_modeling[ind] <- 0.0005
  model_data_pre$unweighted_ili_modeling <- qlogis(model_data_pre$unweighted_ili_modeling)
  
  ts_model_data <- ts(model_data_pre$unweighted_ili_modeling, 
                      start = c(2010, 40), frequency = 52)
  
  arima2 <- forecast::auto.arima(ts_model_data, max.p = 3, max.q = 2, max.P = 2,
                                 max.Q = 2, max.d = 2, max.D = 1,
                                 start.p = 1, start.q = 1, start.P = 1, start.Q = 1)
  # Auto arima is producting some negative coefficient varinaces
  suppressWarnings({
    if (any(is.na(sqrt(diag(vcov(arima_model_list[[i]]$arima_model))))))
    {
      arima2 <- forecast::Arima(ts_model_data, order = c(1, 0, 1),
                                seasonal = c(1, 1, 0), include.drift = TRUE)
    }
  })
  return(list(arima_model = arima2,
              ts_model_data = ts_model_data,
              model_data_pre = model_data_pre,
              model_data_post = model_data_post))
}
