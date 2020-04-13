covid_prediction_builder <- function(model_state, mean_inflation, total_patients)
{
  #model_state <- hhs_regions_map$state[1]
  #mean_inflation <- arima_result_list[[1]]$mean_inflation
  #total_patients <- arima_result_list[[1]]$model_data_post$total_patients[nrow(arl$model_data_post)]
  model_state_c <- as.character(model_state)
  if (model_state_c == "New York City")
  {
    covid_daily_state <- covid_daily %>%
      subset(Combined_Key == "New York City, New York, US" &
               !is.na(Active) & Active > 0,
             select = c("Active", "Confirmed", "mmwrweek", "date"))
  } else
  {
    covid_daily_state <- covid_daily %>%
      subset(Province_State == as.character(model_state) &
               !is.na(Active) & Active > 0,
             select = c("Active", "Confirmed", "mmwrweek", "date"))
  }
  covid_daily_state <- covid_daily_state %>%
    group_by(mmwrweek, date) %>%
    summarise(Confirmed = sum(Confirmed),
              Active = sum(Active))
  covid_daily_state$ndate <- as.numeric(covid_daily_state$date)
  
  #covid_daily_state_pred_data <- data.frame(date = as.Date(today_date) + 1:((WEEKS_AHEAD - 1) * 7))
  
  covid_daily_state_pred_data <- data.frame(
    date = MMWRweek::MMWRweek2Date(current_ili_year, current_ili_week + 2, 1) + 
      0:(WEEKS_AHEAD - 2)*7)
  
  covid_daily_state_pred_data$ndate <- as.numeric(covid_daily_state_pred_data$date)
  covid_daily_state_pred_data$mmwrweek <-
    MMWRweek::MMWRweek(covid_daily_state_pred_data$date)$MMWRweek
  
  # when dates are before march 1st, they are usually low numbers and don't fit
  #   the concave down quadratic well.  
  ind <- which(covid_daily_state$date >= as.Date("2020-03-01"))
  if (length(ind) < 3)
    ind <- 1:nrow(covid_daily_state)
  
  lm1 <- lm(log(Active) ~ ndate + I(ndate^2), data = covid_daily_state[ind,],
            weights = 1 / as.numeric((as.Date(today_date) - covid_daily_state$date[ind] + 1)))
  covid_pred_params <- coef(lm1)
  # suppress messages about a rank deficient fit since we are going to fix it
  suppressWarnings({
    covid_lm_pred <- predict(lm1, newdata = covid_daily_state, se.fit = TRUE)
    covid_daily_state$pred <- exp(covid_lm_pred$fit)
    covid_daily_state$se <- covid_lm_pred$se.fit
    covid_lm_pred <- predict(lm1, newdata = covid_daily_state_pred_data, se.fit = TRUE)
    covid_daily_state_pred_data$pred <- exp(covid_lm_pred$fit)
    covid_daily_state_pred_data$se <- covid_lm_pred$se.fit
  })
  
  if (is.na(coef(lm1)[3]) || coef(lm1)[3] >= 0)
  {
    M <- list(
      X = matrix(c(rep(1, nrow(covid_daily_state[ind,])),
                   covid_daily_state$ndate[ind],
                   covid_daily_state$ndate[ind]^2),
                 nrow = nrow(covid_daily_state[ind,]), ncol = 3), 
      p = c(1, 2, -1),
      off = array(0, 0),
      S = list(),
      Ain = matrix(c(0, 0, -1), nrow = 1, ncol = 3),
      bin = 0.001,
      C = matrix(0, 0, 0),
      sp = array(0, 0),
      y = log(covid_daily_state$Active[ind]),
      w = 1 / as.numeric((as.Date(today_date) - covid_daily_state$date[ind] + 1))
    )
    covid_pred_params <- pcls(M)
    m <- matrix(c(rep(1, nrow(covid_daily_state)),
                  covid_daily_state$ndate,
                  covid_daily_state$ndate^2),
                nrow = nrow(covid_daily_state), ncol = 3)
    covid_daily_state$pred <- exp(as.numeric(m %*% covid_pred_params))
    covid_daily_state$se <- rep(NA, length(covid_daily_state$pred))
    Xnew <- matrix(c(rep(1, nrow(covid_daily_state_pred_data)),
                     covid_daily_state_pred_data$ndate,
                     covid_daily_state_pred_data$ndate^2),
                   nrow = nrow(covid_daily_state_pred_data), ncol = 3)
    covid_daily_state_pred_data$pred <- exp(as.numeric(Xnew %*% covid_pred_params))
    covid_daily_state_pred_data$se <- rep(NA, length(covid_daily_state_pred_data$pred))
  }
  covid_daily_state_post <- covid_daily_state %>%
    group_by(mmwrweek) %>%
    summarise(pred = max(pred),
              actual = max(Active),
              actual_confirmed = max(Confirmed),
              pred_ili = max(pred) * mean_inflation / (total_patients + max(pred)), # percent scale
              pred_ili_se = max(se)) # log scale
  
  covid_daily_state_pred <- covid_daily_state_pred_data %>%
    group_by(mmwrweek) %>%
    summarise(pred = max(pred),
              pred_ili = max(pred) * mean_inflation / (total_patients + max(pred)), # percent scale
              pred_ili_se = max(se)) # log scale
  
  return(list(covid_daily_state_post = covid_daily_state_post,
              covid_daily_state_pred = covid_daily_state_pred,
              covid_daily_state = covid_daily_state,
              covid_daily_state_pred_data = covid_daily_state_pred_data))
}
