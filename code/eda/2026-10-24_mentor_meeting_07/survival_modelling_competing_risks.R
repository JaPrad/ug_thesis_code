# Author: Jyotiraditya Pradhan
# Date: October 27, 2026

# Load Packages ----------------------------------------------------------

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  here,
  tidyverse,
  flexsurvreg,
  survival,
  ggsurvfit,
  tidycmprsk,
  stargazer,
  modelsummary,
  gt
)
source("code/utilities.R")
theme_set(theme_classic() + theme(legend.position = "bottom"))

write_location <- here(
  "data",
  "eda",
  "2026-10-24_mentor_meeting_07",
  "models"
)


# Data Prep --------------------------------------------------------------

survival_data_pre_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-24_mentor_meeting_07",
    "survival_data_pre_covid.csv"
  )
)

survival_data_post_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-24_mentor_meeting_07",
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
    "2026-10-24_mentor_meeting_07",
    "survival_data_combined.csv"
  )
)


## Shortcut Read ----------------------------------------------------------

combined_data <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-24_mentor_meeting_07",
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
    family_income_five_thousands = (family_income / 5000),
    family_income_twenty_thousands = (family_income / 20000)
  ) -> combined_data

# Convert str event to integers

combined_data |>
  mutate(event = recode(event, "C" = 0L, "E" = 1L, "D" = 2L, "O" = 2L)) |>
  mutate(
    event = factor(
      event,
      levels = c(0, 1, 2),
      labels = c("censored", "employed", "olf")
    )
  ) -> combined_data


# Aalen Johansen Estimator -----------------------------------------------

## General ----------------------------------------------------------------

fit_general <- cuminc(Surv(time, event) ~ 1, data = combined_data)

fit_general |>
  ggcuminc(c("employed", "olf")) +
  add_confidence_interval() +
  xlab("Months") +
  ylab("P(Outcome)")


## Caste ------------------------------------------------------------------

fit_caste <- cuminc(Surv(time, event) ~ caste, data = combined_data)

{
  fit_caste |>
    ggcuminc("employed") +
    add_confidence_interval() +
    xlab("Months") +
    ylab("P(Employed)") -> p

  p$data <- p$data |>
    filter(strata != "not_stated")

  p
}

{
  fit_caste |>
    ggcuminc("olf") +
    add_confidence_interval() +
    xlab("Months") +
    ylab("P(OLF)") -> p

  p$data <- p$data |>
    filter(strata != "not_stated")

  p
}


## Religion ---------------------------------------------------------------

fit_religion <- cuminc(Surv(time, event) ~ religion, data = combined_data)

fit_religion |>
  ggcuminc("employed") +
  add_confidence_interval() +
  xlab("Months") +
  ylab("P(Employed)")

fit_religion |>
  ggcuminc("olf") +
  add_confidence_interval() +
  xlab("Months") +
  ylab("P(OLF)")

# Log Logistic -----------------------------------------------------------

## Table Function ---------------------------------------------------------

## Re Employment ----------------------------------------------------------

loglogistic_emp <- survreg(
  Surv(time, event) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == "censored" ~ 0,
        event == "employed" ~ 1,
        event == "olf" ~ 0
      )
    ),
  dist = "loglogistic"
  #control = survreg.control(maxiter = 30)
)
#exp(coef(log_logistic))
#summary(log_logistic)

loglogistic_emp |>
  tidy() |>
  select(term, estimate, p.value) |>
  mutate(
    estimate = exp(estimate)
  ) |>
  gt() |>
  fmt_number(
    columns = estimate,
    decimals = 3
  ) |>
  fmt_number(
    columns = p.value,
    decimals = 7
  )


## OLF --------------------------------------------------------------------

loglogistic_olf <- survreg(
  Surv(time, event == 2) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == "censored" ~ 0,
        event == "employed" ~ 0,
        event == "olf" ~ 1
      )
    ),
  dist = "loglogistic",
  control = survreg.control(maxiter = 110)
)

loglogistic_olf |>
  tidy() |>
  select(term, estimate, p.value) |>
  mutate(
    estimate = exp(estimate)
  ) |>
  gt() |>
  fmt_number(
    columns = estimate,
    decimals = 3
  ) |>
  fmt_number(
    columns = p.value,
    decimals = 7
  )

# Sandbox ----------------------------------------------------------------

## Income 5k --------------------------------------------------------------

survreg(
  Surv(time, event) ~ age +
    family_income_five_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == "censored" ~ 0,
        event == "employed" ~ 1,
        event == "olf" ~ 0
      )
    ),
  dist = "loglogistic"
  #control = survreg.control(maxiter = 30)
) |>
  tidy() |>
  select(term, estimate, p.value) |>
  mutate(
    estimate = exp(estimate)
  ) |>
  gt() |>
  fmt_number(
    columns = estimate,
    decimals = 3
  ) |>
  fmt_number(
    columns = p.value,
    decimals = 7
  )


## Caste Religion Interaction ---------------------------------------------

survreg(
  Surv(time, event) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid +
    caste * religion,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == "censored" ~ 0,
        event == "employed" ~ 1,
        event == "olf" ~ 0
      )
    ),
  dist = "loglogistic"
  #control = survreg.control(maxiter = 30)
) |>
  tidy() |>
  select(term, estimate, p.value) |>
  mutate(
    estimate = exp(estimate)
  ) |>
  gt() |>
  fmt_number(
    columns = estimate,
    decimals = 3
  ) |>
  fmt_number(
    columns = p.value,
    decimals = 7
  )


# Income Edu Interaction -------------------------------------------------

survreg(
  Surv(time, event) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid +
    edu * family_income_ten_thousands,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == "censored" ~ 0,
        event == "employed" ~ 1,
        event == "olf" ~ 0
      )
    ),
  dist = "loglogistic"
  #control = survreg.control(maxiter = 30)
) |>
  tidy() |>
  select(term, estimate, p.value) |>
  mutate(
    estimate = exp(estimate)
  ) |>
  gt() |>
  fmt_number(
    columns = estimate,
    decimals = 3
  ) |>
  fmt_number(
    columns = p.value,
    decimals = 7
  )


survreg(
  Surv(time, event) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid +
    edu * age,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == "censored" ~ 0,
        event == "employed" ~ 1,
        event == "olf" ~ 0
      )
    ),
  dist = "loglogistic"
  #control = survreg.control(maxiter = 30)
) |>
  tidy() |>
  select(term, estimate, p.value) |>
  mutate(
    estimate = exp(estimate)
  ) |>
  gt() |>
  fmt_number(
    columns = estimate,
    decimals = 3
  ) |>
  fmt_number(
    columns = p.value,
    decimals = 7
  )


ggplot(check, aes(log_time, log_odds_failure)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


survreg(
  Surv(time, event) ~ age +
    family_income_twenty_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == "censored" ~ 0,
        event == "employed" ~ 1,
        event == "olf" ~ 0
      )
    ),
  dist = "loglogistic"
  #control = survreg.control(maxiter = 30)
) |>
  tidy() |>
  select(term, estimate, p.value) |>
  mutate(
    estimate = exp(estimate)
  ) |>
  gt() |>
  fmt_number(
    columns = estimate,
    decimals = 3
  ) |>
  fmt_number(
    columns = p.value,
    decimals = 7
  )
