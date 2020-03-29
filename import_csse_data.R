library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(tibbletime)
library(scales)

rolling_mean_7 <- rollify(mean, window = 7)

# Get list of states ---------------------------------------------------

list_of_states <- 
  read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
  clean_names()

# Get data from John Hopkins -------------------------------------------

# John Hopkins changed the way they were sharing their data.
# I imported the old form and saved it locally.
# As new data is shared daily, I bind it to the locally saved file.

# confirmed_archive <-
#   read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
#   clean_names() %>%
#   filter(country_region == "US")
# 
# write_csv(confirmed_archive, here::here("covid_states", "data", "us_confirmed_archive.csv"))

# Read the saved archived data
us_confirmed <- 
  read_csv(here::here("covid_states", "data", "us_confirmed_archive.csv")) %>% 
  separate(province_state, into = c("county", "state_abbr"), sep = ",") %>% 
  filter(is.na(state_abbr)) %>% 
  select("state" = county, everything(), -state_abbr, -country_region, -lat, -long, -x3_23_20)

us_cases <- 
  left_join(list_of_states, us_confirmed, by = c("state" = "state"))

us_cases_long <-
  us_cases %>% 
  pivot_longer(names_to = "date", values_to = "cases", c(-state, -abbreviation)) %>% 
  mutate(date = str_remove(date, "x")) %>% 
  mutate(date = mdy(date))

timeseries_previous <- 
  read_csv(here::here("covid_states", "data", "us_case_timeseries.csv"))

# Read in the new daily data
new_daily_total <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-28-2020.csv") %>% 
  clean_names() %>% 
  filter(country_region == "US") %>% 
  group_by(province_state) %>% 
  summarise(cases = sum(confirmed)) %>% 
  select("state" = province_state, everything())

us_daily <- 
  left_join(list_of_states, new_daily_total, by = c("state" = "state")) %>% 
  add_column("date" = "2020-03-28") %>% 
  mutate(date = ymd(date))

# Bind the two
# us_case_timeseries<- 
#   bind_rows(us_cases_long, us_daily)

us_case_timeseries<- 
  bind_rows(timeseries_previous, us_daily)

# Write the new file locally
write_csv(us_case_timeseries, here::here("covid_states", "data", "us_case_timeseries.csv"))


