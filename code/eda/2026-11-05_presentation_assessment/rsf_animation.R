rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  here,
  tidyverse,
  randomForestSRC
)

write_location <- here(
  "data",
  "eda",
  "2026-11-05_presentation_assessment"
)

rsf <- readRDS(here(write_location, "competing_rsf.rds"))

income_range <- seq(0, 25, 0.5)

cif_tibble <- lapply(income_range, function(income) {
  income <- round(income, 1)
  print(income)

  # Create input dataframe
  data_1 <- data.frame(
    age = 30,
    family_income_ten_thousands = income / 10,
    religion = "hindu",
    caste = "upper_caste",
    edu = "high_school",
    relation = "self"
  )

  data_2 <- data.frame(
    age = 30,
    family_income_ten_thousands = income / 10,
    religion = "muslim",
    caste = "upper_caste",
    edu = "high_school",
    relation = "self"
  )

  # Predict using loaded model
  prediction_1 <- predict(rsf, data_1)
  prediction_2 <- predict(rsf, data_2)

  tibble(
    time = prediction_1$time.interest,
    cif = prediction_1$cif[1, , "CIF.1"]
  ) |>
    mutate(Religion = "Hindu", family_income = income) |>
    add_row(
      tibble(
        time = prediction_2$time.interest,
        cif = prediction_2$cif[1, , "CIF.1"]
      ) |>
        mutate(Religion = "Muslim", family_income = income)
    ) |>
    filter(time <= 24)
}) |>
  bind_rows()


write_csv(cif_tibble, here(write_location, "animation_cif_tibble.csv"))


# Sandbox ----------------------------------------------------------------

cif_tibble |>
  filter(family_income == 0.6)

predict(
  rsf,
  data.frame(
    age = 30,
    family_income_ten_thousands = 0.3 / 10,
    religion = "hindu",
    caste = "obc_ic",
    edu = "middle_school",
    relation = "self"
  )
)$cif[1, , "CIF.1"]

predict(
  rsf,
  data.frame(
    age = 30,
    family_income_ten_thousands = 0.3 / 10,
    religion = "hindu",
    caste = "obc_ic",
    edu = "middle_school",
    relation = "self"
  )
)$time.interest
