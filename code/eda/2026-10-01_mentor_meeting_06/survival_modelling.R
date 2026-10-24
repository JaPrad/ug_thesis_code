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
  survival,
  ggsurvfit,
  stargazer,
  modelsummary
)
source("code/utilities.R")
theme_set(theme_classic() + theme(legend.position = "bottom"))

write_location <- here(
  "data",
  "eda",
  "2026-10-01_mentor_meeting_06",
  "models"
)


# Data Prep --------------------------------------------------------------

survival_data_pre_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "survival_data_pre_covid.csv"
  )
)

survival_data_post_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "survival_data_post_covid.csv"
  )
)

combined_data <- bind_rows(
  mutate(survival_data_pre_covid, post_covid = 0),
  mutate(survival_data_post_covid, post_covid = 1)
)

write_csv(
  combined_data,
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "survival_data_combined.csv"
  )
)


## Shortcut Read ----------------------------------------------------------

combined_data <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "survival_data_combined.csv"
  )
)

## Conversion -------------------------------------------------------------

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
  ) -> combined_data

# KM Estimator -----------------------------------------------------------

## Overall ----------------------------------------------------------------

km_overall <- survfit(
  Surv(time, event) ~ 1,
  data = combined_data
)
write_rds(km_overall, here(write_location, "km_overall.rds"))

ggsurvfit(km_overall) +
  add_confidence_interval() +
  labs(
    x = "Time (in Months) ",
    y = "Probability of remaining Non-Employed"
  )


# COVID ------------------------------------------------------------------

km_covid <- survfit(
  Surv(time, event) ~ post_covid,
  data = combined_data
)

write_rds(km_covid, here(write_location, "km_covid.rds"))

ggsurvfit(km_covid) +
  add_confidence_interval() +
  labs(
    x = "Time (in Months) ",
    y = "Probability of remaining Non-Employed"
  )


## Religion ---------------------------------------------------------------

km_religion <- survfit(
  Surv(time, event) ~ religion,
  data = combined_data
)

write_rds(km_religion, here(write_location, "km_religion.rds"))

ggsurvfit(km_religion) +
  add_confidence_interval() +
  labs(
    x = "Time (in Months) ",
    y = "Probability of remaining Non-Employed"
  )


## Caste ------------------------------------------------------------------

km_caste <- survfit(
  Surv(time, event) ~ caste,
  data = combined_data
)

write_rds(km_caste, here(write_location, "km_caste.rds"))

ggsurvfit(km_caste) +
  add_confidence_interval() +
  labs(
    x = "Time (in Months) ",
    y = "Probability of remaining Non-Employed"
  )

# Log Logistic -----------------------------------------------------------

log_logistic <- survreg(
  Surv(time, event) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = mutate(combined_data, time = time + 1),
  dist = "loglogistic"
)
exp(coef(log_logistic))
summary(log_logistic)


# Sandbox ----------------------------------------------------------------


log_logistic_2 <- survreg(
  Surv(time, event) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid +
    religion*caste +
    family_income_ten_thousands*edu,
  data = mutate(combined_data, time = time + 1),
  dist = "loglogistic"
)
exp(coef(log_logistic_2))




check <- tibble(
  time = km_overall$time,
  surv = km_overall$surv
) |>
  filter(surv > 0, surv < 1, time > 0) |>
  mutate(
    log_time = log(time),
    log_odds_failure = log((1 - surv) / surv)
  )

ggplot(check, aes(log_time, log_odds_failure)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
