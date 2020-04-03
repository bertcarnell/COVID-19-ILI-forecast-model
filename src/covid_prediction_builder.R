covid_prediction_builder <- function(model_state, mean_inflation)
{
  #model_state <- hhs_regions_map$state[2]
  #mean_inflation <- arima_result_list[[2]]$mean_inflation
  model_state_c <- as.character(model_state)
  if (model_state_c == "New York City")
  {
    covid_daily_state <- covid_daily %>%
      subset(Combined_Key == "New York City, New York, US" &
               Confirmed > 0,
             select = c("Confirmed", "mmwrweek", "date"))
  } else
  {
    covid_daily_state <- covid_daily %>%
      subset(Province_State == as.character(model_state) &
               Confirmed > 0,
             select = c("Confirmed", "mmwrweek", "date"))
  }
  coiv_daily_state <- covid_daily_state %>%
    group_by(mmwrweek, date) %>%
    summarise(Confirmed = sum(Confirmed))
  covid_daily_state$ndate <- as.numeric(covid_daily_state$date)
  
  covid_daily_state_pred_data <- data.frame(date = as.Date(today_date) + 1:((WEEKS_AHEAD - 1) * 7))
  covid_daily_state_pred_data$ndate <- as.numeric(covid_daily_state_pred_data$date)
  covid_daily_state_pred_data$mmwrweek <-
    MMWRweek::MMWRweek(covid_daily_state_pred_data$date)$MMWRweek
  
  lm1 <- lm(log(Confirmed) ~ ndate + I(ndate^2), data = covid_daily_state,
            weights = 1 / as.numeric((as.Date(today_date) - covid_daily_state$date + 1)))
  covid_pred_params <- coef(lm1)
  covid_daily_state$pred <- exp(predict(lm1))
  # suppress messages about a rank deficient fit since we are going to fix it
  suppressWarnings({
    covid_daily_state_pred_data$pred <- exp(predict(lm1, newdata = covid_daily_state_pred_data))
  })
  
  if (is.na(coef(lm1)[3]) || coef(lm1)[3] >= 0)
  {
    M <- list(
      X = matrix(c(rep(1, nrow(covid_daily_state)),
                   covid_daily_state$ndate,
                   covid_daily_state$ndate^2),
                 nrow = nrow(covid_daily_state), ncol = 3), 
      p = c(1, 2, -1),
      off = array(0, 0),
      S = list(),
      Ain = matrix(c(0, 0, -1), nrow = 1, ncol = 3),
      bin = 0.001,
      C = matrix(0, 0, 0),
      sp = array(0, 0),
      y = log(covid_daily_state$Confirmed),
      w = 1 / as.numeric((as.Date(today_date) - covid_daily_state$date + 1))
    )
    covid_pred_params <- pcls(M)
    covid_daily_state$pred <- exp(as.numeric(M$X %*% covid_pred_params))
    Xnew <- matrix(c(rep(1, nrow(covid_daily_state_pred_data)),
                     covid_daily_state_pred_data$ndate,
                     covid_daily_state_pred_data$ndate^2),
                   nrow = nrow(covid_daily_state_pred_data), ncol = 3)
    covid_daily_state_pred_data$pred <- exp(as.numeric(Xnew %*% covid_pred_params))
  }
  covid_daily_state_post <- covid_daily_state %>%
    group_by(mmwrweek) %>%
    summarise(pred = max(pred),
              actual = max(Confirmed),
              pred_ili = max(pred) * mean_inflation) # percent scale
  
  covid_daily_state_pred <- covid_daily_state_pred_data %>%
    group_by(mmwrweek) %>%
    summarise(pred = max(pred),
              pred_ili = max(pred) * mean_inflation) # percent scale
  
  return(list(covid_daily_state_post = covid_daily_state_post,
              covid_daily_state_pred = covid_daily_state_pred))
}
