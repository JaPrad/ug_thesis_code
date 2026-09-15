# Jyotiraditya Pradhan, 19 August 2026
#
# Test constructing a survival dataset from post 2022 data
# Estimate and plot a KM Model
# Estimate a Cox PH Model
# Classify/control for religion and caste

# Load Packages ----------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(here, tidyverse, survival, ggsurvfit)
source("code/utilities.R")


# File List --------------------------------------------------------------

files <- list.files(here("data", "cmie", "cmie_people_raw")) |>
  sort()

names(files) <- 1:37


# Survival Data Construction ---------------------------------------------

# Initial Wave -----------------------------------------------------------

initial_data <- read_csv(here(
  "data",
  "cmie",
  "cmie_people_raw",
  files[[25]]
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
  mutate(T = 0, delta = NA_integer_) -> survival_data

rm(initial_data)


## Subsequent Waves -------------------------------------------------------

for (wave in 26:37) {
  cat("Wave", wave, "\n")

  current_data <- read_csv(
    here(
      "data",
      "cmie",
      "cmie_people_raw",
      files[[wave]]
    ),
    show_col_types = FALSE
  ) |>
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
    pull(id) -> non_employed_ids # workers non employed in new wave

  current_data |>
    filter(EMPLOYMENT_STATUS == "Employed") |>
    mutate(id = paste0(HH_ID, MEM_ID)) |>
    pull(id) -> employed_ids # workers employed in new wave

  rm(current_data)

  non_censored_ids <- survival_data[is.na(survival_data$delta), ]$id
  censored_ids <- survival_data[!is.na(survival_data$delta), ]$id

  survival_data |>
    filter(id %in% intersect(non_censored_ids, non_employed_ids)) |>
    mutate(T = T + 1) -> current_alive

  survival_data |>
    filter(id %in% intersect(non_censored_ids, employed_ids)) |>
    mutate(T = T + 1, delta = 1) -> current_failed

  survival_data |>
    filter(
      id %in% non_censored_ids,
      !(id %in% union(non_employed_ids, employed_ids))
    ) |>
    mutate(delta = 0) -> current_censored

  survival_data |>
    filter(id %in% censored_ids) |> #past censored IDs
    add_row(current_alive) |>
    add_row(current_failed) |>
    add_row(current_censored) -> survival_data
}


## Study End Cesoring -----------------------------------------------------

survival_data |>
  mutate(delta = ifelse(is.na(delta), 0, delta)) -> survival_data

write_rds(
  survival_data,
  file = here(
    "data",
    "eda",
    "2026-08-17_mentor_meeting_04",
    "mock_survival_data.Rda"
  )
)

surv_obj <- Surv(survival_data$T, survival_data$delta)
str(surv_obj)


# KM ---------------------------------------------------------------------

## Overall ----------------------------------------------------------------

km_overall <- survfit(Surv(T, delta) ~ 1, data = survival_data)

ggsurvfit(km_overall) +
  add_confidence_interval() +
  add_risktable() +
  labs(
    x = "Time (Waves)",
    y = "Probability of staying Non-Employed"
  ) -> km_overall_plot
km_overall_plot


## Religion Wise ----------------------------------------------------------

km_religion <- survfit(Surv(T, delta) ~ religion, data = survival_data)

ggsurvfit(km_religion) +
  add_confidence_interval() +
  add_risktable() +
  labs(
    x = "Time (Waves)",
    y = "Probability of staying Non-Employed"
  ) -> km_religion_plot
km_religion_plot


## Caste Wise -------------------------------------------------------------

km_caste <- survfit(Surv(T, delta) ~ caste, data = survival_data)

ggsurvfit(km_caste) +
  add_confidence_interval() +
  add_risktable() +
  labs(
    x = "Time (Waves)",
    y = "Probability of staying Non-Employed"
  ) -> km_caste_plot
km_caste_plot


## Cox Plot ---------------------------------------------------------------

survival_data |>
  mutate(
    caste = as.factor(caste),
    religion = as.factor(religion)
  ) -> survival_data

survival_data$caste <- relevel(survival_data$caste, ref = "uc")
survival_data$religion <- relevel(survival_data$religion, ref = "hindu")


cox_model <- coxph(Surv(T, delta) ~ religion + caste, data = survival_data)
summary(cox_model)
cox.zph(cox_model)

# Sandobox ---------------------------------------------------------------

# for (wave in 26:27) {
#   #CHANGE FUN

#   current_data <- read_csv(here(
#     "data",
#     "cmie",
#     "cmie_people_raw",
#     files[[wave]]
#   )) |>
#     filter(GENDER == "M") |>
#     filter(AGE_YRS >= 15) |>
#     filter(RESPONSE_STATUS == "Accepted") |>
#     filter(EMPLOYMENT_STATUS != "Not Applicable")

#   current_data |>
#     filter(
#       EMPLOYMENT_STATUS %in%
#         c(
#           "Unemployed, willing and looking for a job",
#           "Unemployed, willing but not looking for a job",
#           "Unemployed, not willing and not looking for a job"
#         )
#     ) |>
#     mutate(id = paste0(HH_ID, MEM_ID)) |>
#     pull(id) -> non_employed_ids # workers non employed in new wave

#   current_data_pre_covid |>
#     filter(EMPLOYMENT_STATUS == "Employed") |>
#     mutate(id = paste0(HH_ID, MEM_ID)) |>
#     pull(id) -> employed_ids # workers employed in new wave

#   non_censored_ids <- survival_data[is.na(survival_data$delta), ]$id
#   length_non_censored_ids <- length(non_censored_ids)

#   lapply(1:length_non_censored_ids, function(i) {
#     # for workers not yet censored

#     cat("Wave", wave, ":", i, "of", length_non_censored_ids, "\n")
#     x <- non_censored_ids[i]

#     if (x %in% non_employed_ids) {
#       # workers who are non-employed in next period too
#       survival_data |>
#         filter(id == x) |>
#         mutate(T = T + 1)
#     } else if (x %in% employed_ids) {
#       # workers who are employed in next period
#       survival_data |>
#         filter(id == x) |>
#         mutate(T = T + 1, delta = 1)
#     } else {
#       # for remaining non censored workers

#       survival_data |>
#         filter(id == x) |>
#         mutate(delta = 0)
#     }
#   }) |>
#     bind_rows() |>
#     bind_rows(survival_data[!is.na(survival_data$delta), ]) -> survival_data
# }

# write_rds(
#   survival_data,
#   file = here(
#     "data",
#     "eda",
#     "2026-08-17_mentor_meeting_04",
#     "survival_data_pre_covid.Rda"
#   )
# )
