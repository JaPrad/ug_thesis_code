# Author: Jyotiraditya Pradhan
# Date: 30 August, 2026

# Notes:
# Employment status is taken as binary - employed/non-empl0yed
# Data is urban, male, >= 15 age
# Data is non weighted
# Left censored data has been ignored
# ~15% of data with changing interview location/religion/caste has been excluded

# Load Packages ----------------------------------------------------------

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(here, tidyverse, janitor, glue)
source("code/utilities.R")


# Functions --------------------------------------------------------------

obtain_counting_process_data <- function(monthly_panel) {
  # FUnction to obtain counting process data from a monthly panel created using panel_data_construction.R

  # Pivot the panel data into long format using monthid:
  monthly_panel |>
    pivot_longer(
      cols = matches("_\\d{4}_\\d{2}$"), # Columns with suffix in _yyyy_mm form
      names_to = c(".value", "month_id"),
      names_pattern = "^(.*)_(\\d{4}_\\d{2})$"
    ) |>
    mutate(
      employment_status = case_when(
        employment_status %in% c("U", "D", "O") ~ "N", # Make employment status binary
        .default = employment_status
      )
    ) -> monthly_panel_long # Long panel data

  month_ids_sorted <- sort(unique(monthly_panel_long$month_id)) # Get sorted vector of all month ids

  ids_employed_at_some_point <- character(0) # Initialize vectors for IDs who have been employed at some point (tells observation is not left-censored)
  ids_non_employed_last_month <- character(0) # Initialize vectors for IDs who have not been employed last month

  lapply(1:length(month_ids_sorted), function(t) {
    # Iterate over every month
    cat("Current month:", t, "\n") # Print current month to keep track of loop

    month_id_current <- month_ids_sorted[t] # Current monthid

    monthly_panel_long |>
      filter(month_id == month_id_current) -> obs_current # Observation from current month

    ids_employed <- filter(obs_current, employment_status == "E")$id # IDs employed in current month
    ids_non_employed <- filter(obs_current, employment_status != "E")$id # IDs non-employed in current month

    obs_current |>
      filter(id %in% ids_employed_at_some_point) |> # Filter out left-censored IDs
      filter(id %in% union(ids_non_employed, ids_non_employed_last_month)) |> # IDs surviving currently or had survived last month
      mutate(start = t - 2, stop = t - 1, .after = id) |> # Indexing starts from 0. At t=1, no observation will be considered because they are either failed or left censored. Observations are only recorded from t=2
      mutate(
        event = ifelse(id %in% ids_employed, 1, 0), # Employed -> Failure -> 1. Else 0
        .after = stop
      ) -> output

    # Modifications for next iteration
    ids_employed_at_some_point <<- union(
      ids_employed,
      ids_employed_at_some_point
    ) # Add IDs who got employed in current month to IDs employed at some point
    ids_non_employed_last_month <<- ids_non_employed # IDs not employed currently become IDs not employed in current month.

    return(output)
  }) |>
    bind_rows() -> counting_process_data
  return(counting_process_data)
}


# Pre-Covid -------------------------------------------------------------

## Read Data --------------------------------------------------------------

monthly_panel_pre_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-08-28_lab_presentation_01",
    "monthly_panel_pre_covid.csv"
  )
)
## Eliminate Duplicates and Weights ---------------------------------------

nrow(monthly_panel_pre_covid)
#> 265474

monthly_panel_pre_covid |>
  filter(n() > 1, .by = id) |>
  nrow()
#> 27719

monthly_panel_pre_covid |>
  distinct(id, .keep_all = TRUE) -> monthly_panel_pre_covid

nrow(monthly_panel_pre_covid)
#> 251470

monthly_panel_pre_covid |>
  select(
    -starts_with("r_ge15_mem_wgt_for_country_w"),
    -starts_with("ge15_mem_nr_for_country_w")
  ) -> monthly_panel_pre_covid

## Construction and Storage -----------------------------------------------

counting_process_data_pre_covid <- obtain_counting_process_data(
  monthly_panel_pre_covid
)

write_csv(
  counting_process_data_pre_covid,
  here(
    "data",
    "eda",
    "2026-08-28_lab_presentation_01",
    "counting_process_data_pre_covid.csv"
  )
)

# Post-Covid --------------------------------------------------------------

## Read Data --------------------------------------------------------------

monthly_panel_post_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-08-28_lab_presentation_01",
    "monthly_panel_post_covid.csv"
  )
)
## Eliminate Duplicates and Weights ---------------------------------------

nrow(monthly_panel_post_covid)
#> 348916

monthly_panel_post_covid |>
  filter(n() > 1, .by = id) |>
  nrow()
#> 59950

monthly_panel_post_covid |>
  distinct(id, .keep_all = TRUE) -> monthly_panel_post_covid

nrow(monthly_panel_post_covid)
#> 318439

monthly_panel_post_covid |>
  select(
    -starts_with("r_ge15_mem_wgt_for_country_w"),
    -starts_with("ge15_mem_nr_for_country_w")
  ) -> monthly_panel_post_covid

## Construction and Storage -----------------------------------------------

counting_process_data_post_covid <- obtain_counting_process_data(
  monthly_panel_post_covid
)

write_csv(
  counting_process_data_post_covid,
  here(
    "data",
    "eda",
    "2026-08-28_lab_presentation_01",
    "counting_process_data_post_covid.csv"
  )
)
