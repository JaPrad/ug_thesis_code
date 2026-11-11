# Author: Jyotiraditya Pradhan
# Date: November 4, 2026

# Load Packages ----------------------------------------------------------

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  here,
  tidyverse,
  randomForestSRC,
  qs2
)
source("code/utilities.R")

write_location <- here(
  "data",
  "eda",
  "2026-11-05_presentation_assessment"
)

# Data -------------------------------------------------------------------

## Read Data --------------------------------------------------------------

combined_data <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-24_mentor_meeting_07",
    "survival_data_combined.csv"
  )
)


## Factor Data ------------------------------------------------------------

# Convert strings to factors:
combined_data |>
  mutate(
    caste = factor(caste),
    caste = relevel(caste, ref = "uc")
  ) |>
  mutate(
    religion = factor(religion),
    religion = relevel(religion, ref = "hindu")
  ) |>
  mutate(
    edu = factor(
      edu,
      level = c(
        "no_edu",
        "primary_school",
        "middle_school",
        "high_school",
        "vocational",
        "higher_edu"
      )
    ),
  ) |>
  mutate(
    relation = factor(relation),
    relation = relevel(relation, ref = "self")
  ) |>
  mutate(
    family_income_ten_thousands = (family_income / 10000),
    family_income_five_thousands = (family_income / 5000),
  ) -> combined_data

combined_data |>
  mutate(
    event = recode(event, "C" = 0L, "E" = 1L, "D" = 2L, "O" = 2L)
  ) -> combined_data

# Model ------------------------------------------------------------------

## Training ---------------------------------------------------------------

competing_rsf <- rfsrc.anonymous(
  Surv(time, event) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation,
  combined_data,
  ntree = 100,
  nodesize = 10
)
competing_rsf_forest <- competing_rsf$forest
environment(competing_rsf_forest$sampsize) <- globalenv()
saveRDS(competing_rsf_forest, here(write_location, "competing_rsf.rds"))


## Shortcut Read ----------------------------------------------------------

competing_rsf_forest <- readRDS(here(write_location, "competing_rsf.rds"))
