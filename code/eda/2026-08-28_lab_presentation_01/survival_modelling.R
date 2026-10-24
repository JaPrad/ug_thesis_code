# Author: Jyotiraditya Pradhan
# Date: 30 August, 2026

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
  "2026-08-28_lab_presentation_01",
  "models"
)


# Data Prep --------------------------------------------------------------

counting_process_data_pre_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-08-28_lab_presentation_01",
    "counting_process_data_pre_covid.csv"
  )
)

counting_process_data_post_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-08-28_lab_presentation_01",
    "counting_process_data_post_covid.csv"
  )
)

combined_data <- bind_rows(
  mutate(counting_process_data_pre_covid, post_covid = 0),
  mutate(counting_process_data_post_covid, post_covid = 1)
)

rm(
  list = c(
    "counting_process_data_pre_covid",
    "counting_process_data_post_covid"
  )
)

# Convert strings to factors:
combined_data |>
  cphs_classify_caste() |>
  mutate(
    caste_category = factor(caste_category),
    caste_category = relevel(caste_category, ref = "uc")
  ) |>
  cphs_classify_religion() |>
  mutate(
    religion = factor(religion),
    religion = relevel(religion, ref = "hindu")
  ) |>
  mutate(
    edu = case_when(
      edu %in%
        c(
          "No Education",
          "Pre School"
        ) ~ "no_edu",

      edu %in%
        c(
          "1st Std. Pass",
          "2nd Std. Pass",
          "3rd Std. Pass",
          "4th Std. Pass",
          "5th Std. Pass"
        ) ~ "primary_school",

      edu %in%
        c(
          "6th Std. Pass",
          "7th Std. Pass",
          "8th Std. Pass"
        ) ~ "middle_school",

      edu %in%
        c(
          "9th Std. Pass",
          "10th Std. Pass",
          "11th Std. Pass",
          "12th Std. Pass"
        ) ~ "high_school",

      edu == "Diploma" ~ "vocational",

      edu %in%
        c(
          "Graduate",
          "Post Graduate",
          "Ph.D / M.Phil"
        ) ~ "higher_edu",

      TRUE ~ NA_character_
    )
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
    relation_with_hoh = case_when(
      relation_with_hoh == "HOH" ~ "self",
      relation_with_hoh == "Son" ~ "son",
      .default = "other"
    )
  ) |>
  mutate(
    relation_with_hoh = factor(relation_with_hoh),
    relation_with_hoh = relevel(relation_with_hoh, ref = "self")
  ) |>
  mutate(
    family_income_thousands = (inc_of_all_mems_frm_all_srcs / 1000)
  ) -> combined_data

write_csv(
  combined_data,
  here(
    "data",
    "eda",
    "2026-08-28_lab_presentation_01",
    "counting_process_data_combined.csv"
  )
)


## Shortcut Read ----------------------------------------------------------

combined_data <- read_csv(
  here(
    "data",
    "eda",
    "2026-08-28_lab_presentation_01",
    "counting_process_data_combined.csv"
  )
)

# KM Estimator -----------------------------------------------------------

## General ----------------------------------------------------------------

km_overall <- survfit(
  Surv(start, stop, event) ~ 1,
  data = combined_data
)

write_rds(km_overall, here(write_location, "km_overall.rds"))

ggsurvfit(km_general) +
  add_confidence_interval() +
  labs(
    x = "Time (in Months) ",
    y = "Probability of remaining Non-Employed"
  )


# COVID ------------------------------------------------------------------

km_covid <- survfit(
  Surv(start, stop, event) ~ post_covid,
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
  Surv(start, stop, event) ~ religion,
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
  Surv(start, stop, event) ~ caste_category,
  data = combined_data
)

write_rds(km_caste, here(write_location, "km_caste.rds"))

ggsurvfit(km_caste) +
  add_confidence_interval() +
  labs(
    x = "Time (in Months) ",
    y = "Probability of remaining Non-Employed"
  )


# Cox PH -----------------------------------------------------------------

cox_main <- coxph(
  Surv(start, stop, event) ~ age_yrs +
    family_income_thousands +
    religion +
    caste_category +
    edu +
    relation_with_hoh +
    post_covid,
  data = combined_data
)

write_rds(cox_main, here(write_location, "cox_main.rds"))
summary(cox_main)

# Calculate Hazard Ratios and corresponding SEs
hr <- exp(coef(cox_main))
se_hr <- hr * summary(cox_main)$coefficients[, "se(coef)"]

# Generate LaTeX Table
stargazer(cox_main, type = "html", coef = list(hr), se = list(se_hr))


## Diagnostics ------------------------------------------------------------

cox_main_zph <- cox.zph(cox_main)
plot(cox_main_zph)

write_rds(cox_main_zph, here(write_location, "cox_main_zph.rds"))

## Stratify with State ----------------------------------------------------

cox_strata <- coxph(
  Surv(start, stop, event) ~ age_yrs +
    family_income_thousands +
    religion +
    caste_category +
    edu +
    relation_with_hoh +
    post_covid +
    strata(state),
  data = combined_data
)

cox.zph(cox_strata)
