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
  #flexsurvreg,
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
  "2026-11-04_mentor_meeting_08",
  "models"
)


# Data Prep --------------------------------------------------------------

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
    family_income_thousands = (family_income / 1000),
    family_income_ten_thousands = (family_income / 10000),
    family_income_five_thousands = (family_income / 5000),
  ) -> combined_data

# Convert str event to integers

combined_data |>
  mutate(
    event = recode(event, "C" = 0L, "E" = 1L, "D" = 2L, "O" = 2L)
  ) -> combined_data

# Table Function ---------------------------------------------------------

get_table <- function(tbl) {
  tidy(tbl) |>
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
    ) %>%
    return()
}

# Re Employment ----------------------------------------------------------

## Basic ------------------------------------------------------------------

loglogistic_emp <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == 2 ~ 0,
        .default = event
      )
    ),
  dist = "loglogistic"
)

loglogistic_emp |>
  get_table() -> a

a


## Income Below Bound -----------------------------------------------------

loglogistic_emp <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    income_below * family_income_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == 2 ~ 0,
        .default = event
      )
    ) |>
    mutate(income_below = ifelse(family_income < 9000, 1, 0)),
  dist = "loglogistic"
) |>
  get_table() -> a


## Statewise Filter -------------------------------------------------------

loglogistic_emp <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == 2 ~ 0,
        .default = event
      )
    ) |>
    filter(state == "Uttar Pradesh"),
  dist = "loglogistic"
) |>
  get_table() -> up


loglogistic_emp <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == 2 ~ 0,
        .default = event
      )
    ) |>
    filter(state == "Odisha"),
  dist = "loglogistic"
) |>
  get_table() -> od


loglogistic_emp <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == 2 ~ 0,
        .default = event
      )
    ) |>
    filter(state == "Tamil Nadu"),
  dist = "loglogistic"
) |>
  get_table() -> tn

loglogistic_emp <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == 2 ~ 0,
        .default = event
      )
    ) |>
    filter(state == "Bihar"),
  dist = "loglogistic"
) |>
  get_table() -> bh

loglogistic_emp <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == 2 ~ 0,
        .default = event
      )
    ) |>
    filter(state == "Karnataka"),
  dist = "loglogistic"
) |>
  get_table() -> kar

loglogistic_emp <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    religion +
    caste +
    edu +
    relation +
    post_covid,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == 2 ~ 0,
        .default = event
      )
    ) |>
    filter(state == "Maharashtra"),
  dist = "loglogistic"
) |>
  get_table() -> mh


up
bh
tn
mh
c
d
kar
mh
a

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
