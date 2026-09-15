# Load Packages ----------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, survival)
source("code/utilities.R")


# File List --------------------------------------------------------------

files <- list.files(here("data", "cmie", "cmie_people_raw")) |>
  sort()

names(files) <- 1:37


# Pre Covid Data ---------------------------------------------------------

initial_data <- read_csv(here(
  "data",
  "cmie",
  "cmie_people_raw",
  files[[7]]
)) |>
  filter(GENDER == "M") |>
  filter(AGE_YRS >= 15) |>
  filter(RESPONSE_STATUS == "Accepted") |>
  filter(EMPLOYMENT_STATUS != "Not Applicable")

initial_data |>
  filter(
    EMPLOYMENT_STATUS %in%
      c(
        "Unemployed, willing and looking for a job",
        "Unemployed, willing but not looking for a job",
        "Unemployed, not willing and not looking for a job"
      )
  ) |>
  cphs_get_id() |>
  cphs_classify_caste() |>
  cphs_classify_religion() |>
  select(id, caste, religion) |>
  mutate(T = 0, delta = NA_integer_, initial_wave = 7) -> survival_data

for (wave in 8:18) {
  #CHANGE FUN

  current_data <- read_csv(here(
    "data",
    "cmie",
    "cmie_people_raw",
    files[[wave]]
  )) |>
    filter(GENDER == "M") |>
    filter(AGE_YRS >= 15) |>
    filter(RESPONSE_STATUS == "Accepted") |>
    filter(EMPLOYMENT_STATUS != "Not Applicable")

  current_data |>
    filter(
      EMPLOYMENT_STATUS %in%
        c(
          "Unemployed, willing and looking for a job",
          "Unemployed, willing but not looking for a job",
          "Unemployed, not willing and not looking for a job"
        )
    ) |>
    mutate(id = paste0(HH_ID, MEM_ID)) |>
    pull(id) -> non_employed_ids # workers non emplyed in new wave

  current_data_pre_covid |>
    filter(EMPLOYMENT_STATUS == "Employed") |>
    mutate(id = paste0(HH_ID, MEM_ID)) |>
    pull(id) -> employed_ids # workers employed in new wave

  existing_ids <- survival_data$id

  all_ids <- union(union(existing_ids, non_employed_ids), employed_ids)

  counter_total <- length(all_ids)

  cat("Wave", wave, ":", "Total", counter_total, "\n")

  lapply(all_ids, function(x) {
    # for workers who are already in study, and failure/censoring has not occured, and non-emplyed in next period
    if (
      (x %in% existing_ids) &&
        (x %in% survival_data[is.na(survival_data$delta), ]$id) &&
        (x %in% non_employed_ids)
    ) {
      survival_data |>
        filter(id == x, is.na(delta)) |>
        mutate(T = T + 1)
    } else if (
      # for workers who are already in study, and failure/censoring has not occured, and employed in next period
      (x %in% existing_ids) &&
        (x %in% survival_data[is.na(survival_data$delta), ]$id) &&
        (x %in% employed_ids)
    ) {
      survival_data |>
        filter(id == x & is.na(delta)) |>
        mutate(delta = 1)
    } else if (x %in% non_employed_ids) {
      # for remaining workers who are non employed in new wave

      current_data |>
        cphs_get_id() |>
        cphs_classify_caste() |>
        cphs_classify_religion() |>
        select(id, caste, religion) |>
        mutate(T = 0, delta = NA_integer_, initial_wave = wave)
    } else if (x %in% existing_ids) {
      # for remaining workers who were already in study, we will consider them censored
      survival_data |>
        filter(id == x) |>
        mutate(delta = 0)
    }
  }) |>
    bind_rows() -> survival_data
}

write_rds(
  survival_data,
  file = here(
    "data",
    "eda",
    "2026-08-17_mentor_meeting_04",
    "survival_data_pre_covid.Rda"
  )
)
