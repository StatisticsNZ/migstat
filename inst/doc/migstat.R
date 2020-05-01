## ----setup, include = FALSE-------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(width = 150)

## ---- message = FALSE-------------------------------------------------------------------------------------------------------------------------------
library(migstat)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
dur_threshold <- 270
dur_test <- 365

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- migstat::border_crossings %>%
  as_data_frame()
crossings

## ---------------------------------------------------------------------------------------------------------------------------------------------------
date_classif_start <- as.Date("2009-01-01")
date_classif_end <- as.Date("2011-12-31")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  mutate(date_crossing = as_date(datetime_crossing))

## ---------------------------------------------------------------------------------------------------------------------------------------------------
filter(crossings, passport_no == "E105")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
filter(crossings, name == "Cai")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  mutate(personId = make_person_id(key1 = name, key2 = passport_no)) %>%
  arrange(personId)
crossings

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  select(-name, -passport_no)
crossings

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  mutate(is_arrival = 1 * (direction == "Arrival")) %>%
  select(personId, crossing_id, date_crossing, is_arrival, res_status_default)
crossings

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  mutate(days_obs = calc_days_obs(date_crossing = date_crossing,
                                  date_obs_end = date_classif_end,
                                  dur_test = dur_test))
select(crossings, crossing_id, date_crossing, is_arrival, days_obs)

## ---- eval = FALSE----------------------------------------------------------------------------------------------------------------------------------
#  pmin(pmax(date_obs_end - date_crossing, 0L), dur_test)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  mutate(days_in_country = calc_days_in_country(personId = personId,
                                                date_crossing = date_crossing,
                                                is_arrival = is_arrival,
                                                date_obs_end = date_classif_end,
                                                dur_test = dur_test))
select(crossings, crossing_id, date_crossing, is_arrival, days_obs, days_in_country)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
res_status_initial <- crossings %>%
  group_by(personId) %>%
  arrange(date_crossing) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(res_status_initial = ifelse(date_crossing < date_classif_start + days(dur_threshold),
                                     res_status_default,
                                     1 - is_arrival))
res_status_initial

## ---------------------------------------------------------------------------------------------------------------------------------------------------
res_status_initial <- res_status_initial %>%
  select(personId, res_status_initial)
res_status_initial

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  filter(date_crossing >= date_classif_start) %>%
  left_join(res_status_initial, by = "personId")
select(crossings, personId, crossing_id, res_status_initial)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  mutate(res_status_after = calc_res_status_after(personId = personId,
                                                  is_arrival = is_arrival,
                                                  days_in_country = days_in_country,
                                                  days_obs = days_obs,
                                                  res_status_initial = res_status_initial,
                                                  dur_test = dur_test,
                                                  dur_threshold = dur_threshold))
select(crossings, personId, is_arrival, days_in_country, days_obs, res_status_initial, res_status_after)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  mutate(res_status_before = calc_res_status_before(personId = personId,
                                                    res_status_initial = res_status_initial,
                                                    res_status_after = res_status_after))
select(crossings, personId, res_status_initial, res_status_after, res_status_before)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
crossings <- crossings %>%
  mutate(is_long_term_mig = calc_is_long_term_mig(personId = personId,
                                                  is_arrival = is_arrival,
                                                  res_status_before = res_status_before,
                                                  res_status_after = res_status_after,
                                                  days_in_country = days_in_country,
                                                  days_obs = days_obs,
                                                  dur_test = dur_test,
                                                  dur_threshold = dur_threshold))
crossings %>% 
  select(personId, is_arrival, res_status_before, res_status_after, days_in_country,
         days_obs, is_long_term_mig) %>%
  filter(personId %in% c(1, 2, 6))

## ---- fig.width = 7.15, fig.height = 10-------------------------------------------------------------------------------------------------------------
plot_one_person <- function(personId, mig_hist) {
  plot_mig_hist(date_crossing = mig_hist$date_crossing, 
                is_arrival = mig_hist$is_arrival,
                date_first = date_classif_start,
                date_last = date_classif_end,
                show_date = FALSE, 
                cex = 0.8)
  mtext(text = sprintf("personId: %d", personId),
        line = 1.5)
}
old_par <- par(mfrow = c(6, 1))
mig_histories <- migstat::border_crossings %>% 
  mutate(personId = make_person_id(key1 = name, key2 = passport_no),
         date_crossing = as_date(datetime_crossing),
         is_arrival = direction == "Arrival") %>%
  nest(-personId)
personIds <- pull(mig_histories, personId)
mig_hists <- pull(mig_histories, data)
walk2(personIds, mig_hists, plot_one_person)
par(old_par)

