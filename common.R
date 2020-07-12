################################################################################
# Packages

# Install from GitHub
if (FALSE)
{
  require(devtools)
  devtools::install_github("hrbrmstr/cdcfluview")
}

require(cdcfluview)
require(reshape2)
require(assertthat)
require(magrittr)
require(MMWRweek)
require(ggplot2)
require(dplyr)
require(mgcv)
require(readr)
require(forecast)
require(triangle)

################################################################################
# common constants

today_date <- as.Date("2020-07-10") # Sys.Date() - Friday

current_ili_week <- 27 # max(model_data_post$week)
current_ili_year <- 2020
current_ili_date <- as.Date("2020-06-28") # Sunday - MMWRweek(current_ili_date)

assertthat::assert_that(current_ili_week == MMWRweek(today_date)$MMWRweek - 1)
assertthat::assert_that(current_ili_week == MMWRweek(current_ili_date)$MMWRweek)

# centers of the probability bins
bin_centers <- c(seq(0.05, 24.5, by = 0.1), (25 + 100)/2)
# weeks which have actuals since week 10 (e.g. if we are on the ew13 forecast
#   then this should be 10:12)
fcst_weeks <- (current_ili_week - 6):(current_ili_week - 1)
WEEKS_AHEAD <- 6

