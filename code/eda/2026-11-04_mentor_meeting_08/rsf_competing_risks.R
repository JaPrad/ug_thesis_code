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
  randomForestSRC
)
source("code/utilities.R")
theme_set(theme_classic() + theme(legend.position = "bottom"))

write_location <- here(
  "data",
  "eda",
  "2026-11-04_mentor_meeting_08",
  "models"
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

competing_rsf <- rfsrc(
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
write_rds(competing_rsf, here(write_location, "competing_rsf.rds"))


### Training nodesize 5 ----------------------------------------------------

competing_rsf_ns5 <- rfsrc(
  Surv(time, event) ~ age +
    family_income_ten_thousands +
    religion +
    caste +
    edu +
    relation,
  combined_data,
  ntree = 100,
  nodesize = 5
)
write_rds(competing_rsf_ns5, here(write_location, "competing_rsf_ns5.rds"))

## Shortcut Read ----------------------------------------------------------

competing_rsf <- read_rds(here(write_location, "competing_rsf.rds"))
competing_rsf_ns5 <- read_rds(here(write_location, "competing_rsf_ns5.rds"))


## Prediction -------------------------------------------------------------

prediction_1 <- predict(
  competing_rsf_ns5,
  data.frame(
    age = 45,
    family_income_ten_thousands = 6,
    religion = "hindu",
    caste = "uc",
    edu = "high_school",
    relation = "self"
  )
)

prediction_2 <- predict(
  competing_rsf_ns5,
  data.frame(
    age = 45,
    family_income_ten_thousands = 1,
    religion = "muslim",
    caste = "obc",
    edu = "high_school",
    relation = "self"
  )
)

plot.competing.risk(prediction_1)
plot.competing.risk(prediction_2)

# plot(
#   pred_1$time.interest,
#   pred_1$survival[1, ],
#   type = "l",
#   xlab = "Time (Months)",
#   ylab = "Survival"
# )
# lines(
#   pred_2$time.interest,
#   pred_2$survival[1, ],
#   xlab = "Time (Months)",
#   ylab = "Survival",
#   lty = "dotted"
# )

# pred_1$survival[2, ]

# Sandbox ----------------------------------------------------------------

get.cindex(competing_rsf)

get.cindex(
  time = competing_rsf$yvar[, 1],
  censoring = competing_rsf$yvar[, 2],
  predicted = competing_rsf$predicted.oob
)

get.cindex(
  time = competing_rsf_ns5$yvar[, 1],
  censoring = competing_rsf_ns5$yvar[, 2],
  predicted = competing_rsf_ns5$predicted.oob
)

tibble(
  time = prediction_1$time.interest,
  cif = prediction_1$cif[1, , "CIF.1"]
) |>
  mutate(Individual = "1", `Exit Type` = "Re-Employment") |>
  add_row(
    tibble(
      time = prediction_1$time.interest,
      cif = prediction_1$cif[1, , "CIF.2"]
    ) |>
      mutate(Individual = "1", `Exit Type` = "Quitting")
  ) |>
  add_row(
    tibble(
      time = prediction_2$time.interest,
      cif = prediction_2$cif[1, , "CIF.1"]
    ) |>
      mutate(Individual = "2", `Exit Type` = "Re-Employment")
  ) |>
  add_row(
    tibble(
      time = prediction_2$time.interest,
      cif = prediction_2$cif[1, , "CIF.2"]
    ) |>
      mutate(Individual = "2", `Exit Type` = "Quitting")
  ) |>
  filter(time <= 24) -> prediction_table

prediction_table |>
  #filter(`Exit Type` == "Re-Employment") |>
  ggplot(aes(x = time, y = cif, colour = Individual, linetype = `Exit Type`)) +
  geom_line()
