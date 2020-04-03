# Model Constants

require(MMWRweek)
require(magrittr)
require(readr)

reporting_bins <- c(seq(0.0, 25, by = 0.1), 100)

reporting_bin_centers <- (reporting_bins[1:(length(reporting_bins) - 1)] + 
                            reporting_bins[2:length(reporting_bins)]) / 2

WEEKS_AHEAD <- 6

## shared parameters/data across state/national templates
ili_targets <- c(paste(1:6, "wk ahead"), "Peak height")
date_targets <- c("Peak week", "First week below baseline")
binary_targets <- c("Below baseline for 3 weeks")
ili_bins <- as.character(sprintf("%.1f", seq(0, 25, by = 0.1)))
date_bins_df <- MMWRweek(seq.Date(as.Date("2020-03-01"), 
                                  as.Date("2020-08-29"), by = "1 week"))
date_bins <- paste0(date_bins_df$MMWRyear, "-ew", date_bins_df$MMWRweek)

## note, this excludes Florida and includes Virgin Islands, Puerto Rico, and District of Columbia
flusight_states <- read_csv("https://raw.githubusercontent.com/cdcepi/State_FluSight_Forecasts/master/2017-2018_StateILI_Submission_Template.csv") %>%
  .$Location %>% unique() %>% append("New York City", after = 32)

