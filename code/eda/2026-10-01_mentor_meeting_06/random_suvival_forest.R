# Author: Jyotiraditya Pradhan
# Date: October 6, 2026

# Load Packages ----------------------------------------------------------

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  here,
  tidyverse,
  randomForestSRC
)
source("code/utilities.R")
theme_set(theme_classic() + theme(legend.position = "bottom"))

write_location <- here(
  "data",
  "eda",
  "2026-10-01_mentor_meeting_06",
  "models"
)


# Data -------------------------------------------------------------------

## Read Data --------------------------------------------------------------

combined_data <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "survival_data_combined.csv"
  )
)


## Factor Data ------------------------------------------------------------

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
  ) -> combined_data


# Model ------------------------------------------------------------------

## Training ---------------------------------------------------------------

emp_rsf <- rfsrc(
  Surv(time, event) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  combined_data,
  ntree = 100,
  nodesize = 5,
  importance = TRUE
)
write_rds(emp_rsf, here(write_location, "emp_rsf.rds"))


## Shortcut Read ----------------------------------------------------------

emp_rsf <- read_rds(here(write_location, "emp_rsf.rds"))


## Prediction -------------------------------------------------------------

pred_1 <- predict(
  emp_rsf,
  data.frame(
    age = 45,
    family_income_ten_thousands = 3,
    religion = "hindu",
    caste = "uc",
    edu = "high_school",
    relation = "self",
    post_covid = 1
  )
)
pred_2 <- predict(
  emp_rsf,
  data.frame(
    age = 45,
    family_income_ten_thousands = 3,
    religion = "muslim",
    caste = "obc",
    edu = "high_school",
    relation = "self",
    post_covid = 1
  )
)

plot(
  pred_1$time.interest,
  pred_1$survival[1, ],
  type = "l",
  xlab = "Time (Months)",
  ylab = "Survival"
)
lines(
  pred_2$time.interest,
  pred_2$survival[1, ],
  xlab = "Time (Months)",
  ylab = "Survival",
  lty = "dotted"
)

pred_1$survival[2, ]
