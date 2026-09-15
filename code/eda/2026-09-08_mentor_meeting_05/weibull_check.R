# Author: Jyotiraditya Pradhan
# Date: 8 September, 2026

# This file tests if the Weibull model can be used in our data by testing if PH & Weibull assumptions hold

# Load Packages ----------------------------------------------------------

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  here,
  tidyverse,
  survival,
  ggsurvfit,
  stargazer,
  modelsummary,
  flexsurv,
  SurvRegCensCov
)
source("code/utilities.R")
theme_set(theme_classic() + theme(legend.position = "bottom"))

counting_process_data <- read_csv(here(
  "data",
  "eda",
  "2026-08-28_lab_presentation_01",
  "counting_process_data_combined.csv"
))

models_write_location <- here(
  "data",
  "eda",
  "2026-09-08_mentor_meeting_05",
  "models"
)


# Fit Model --------------------------------------------------------------

weibull_fit <- flexsurvreg(
  Surv(start, stop, event) ~ age_yrs +
    family_income_thousands +
    religion +
    caste_category +
    edu +
    relation_with_hoh +
    post_covid,
  data = counting_process_data,
  dist = "weibull"
)
write_rds(weibull_fit, here(models_write_location, "weibull_fit.RDS"))


# Summary and Diagnostics ------------------------------------------------

weibull_fit <- read_rds(here(models_write_location, "weibull_fit.RDS"))
weibull_fit

WeibullDiag(
  Surv(start, stop, event) ~ caste_category,
  data = counting_process_data
)

WeibullDiag(
  Surv(start, stop, event) ~ religion,
  data = counting_process_data
)
