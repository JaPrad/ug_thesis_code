# Author: Jyotiraditya Pradhan
# Date: November 10, 2026

# Load Packages ----------------------------------------------------------

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  here,
  qs2,
  tidyverse,
  gt,
  survival,
  ggsurvfit,
  tidycmprsk
)

# Read & Write -----------------------------------------------------------

combined_data <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-24_mentor_meeting_07",
    "survival_data_combined.csv"
  )
)

write_location <- here(
  "data",
  "eda",
  "2026-11-05_presentation_assessment"
)

## Conversion -------------------------------------------------------------
{
  # Convert strings to factors:
  combined_data |>
    mutate(
      caste = factor(
        caste,
        levels = c(
          "uc",
          "obc_ic",
          "sc",
          "st",
          "not_stated"
        )
      )
    ) |>
    mutate(
      caste = recode(
        caste,
        "uc" = "Upper Caste",
        "obc_ic" = "OBC/Intermediate Caste",
        "sc" = "SC",
        "st" = "ST",
        "not_stated" = "Not Stated"
      )
    ) |>
    mutate(
      religion = factor(
        religion,
        levels = c(
          "hindu",
          "muslim",
          "other"
        )
      )
    ) |>
    mutate(
      religion = recode(
        religion,
        "hindu" = "Hindu",
        "muslim" = "Muslim",
        "other" = "Other"
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
      edu = recode(
        edu,
        "no_edu" = "No Education",
        "primary_school" = "Primary School",
        "middle_school" = "Middle School",
        "high_school" = "High School",
        "vocational" = "Vocational Education",
        "higher_edu" = "Higher Education"
      )
    ) |>
    mutate(
      relation = factor(
        relation,
        levels = c(
          "self",
          "son",
          "other"
        )
      )
    ) |>
    mutate(
      relation = recode(
        relation,
        "self" = "Self",
        "son" = "Son",
        "other" = "Other"
      )
    ) |>
    mutate(
      family_income_thousands = (family_income / 1000),
      family_income_ten_thousands = (family_income / 10000)
    ) -> combined_data

  # Convert str event to integers

  combined_data |>
    mutate(event = recode(event, "C" = 0L, "E" = 1L, "D" = 2L, "O" = 2L)) |>
    mutate(
      event = factor(
        event,
        levels = c(0, 1, 2),
        labels = c("censored", "Re-Employment", "Quitting")
      )
    ) -> combined_data
}


# Aalen Johansen Estimator -----------------------------------------------

### Plotting Functions -----------------------------------------------------

plot_re_employment <- function(model) {
  model |>
    ggcuminc("Re-Employment") +
    add_confidence_interval() +
    xlab("Time (in Months)") +
    ylab("P(Re-Employment)")
}

plot_quitting <- function(model) {
  model |>
    ggcuminc("Quitting") +
    add_confidence_interval() +
    xlab("Time (in Months)") +
    ylab("P(Quitting)")
}

## Overall ----------------------------------------------------------------

aj_overall <- cuminc(Surv(time, event) ~ 1, data = combined_data)

qs_save(aj_overall, here(write_location, "aj_overall.qs2"))

aj_overall |>
  ggcuminc(c("Re-Employment", "Quitting")) +
  add_confidence_interval() +
  xlab("Time (in Months)") +
  ylab("P(Outcome)")


## Religion ---------------------------------------------------------------

aj_religion <- cuminc(Surv(time, event) ~ religion, data = combined_data)

qs_save(aj_religion, here(write_location, "aj_religion.qs2"))

aj_religion |>
  plot_re_employment()
aj_religion |>
  plot_quitting()

## Caste ------------------------------------------------------------------

aj_caste <- cuminc(Surv(time, event) ~ caste, data = combined_data)

qs_save(aj_caste, here(write_location, "aj_caste.qs2"))

{
  aj_caste |>
    plot_re_employment() -> p

  p$data <- p$data |>
    filter(strata != "Not Stated")

  p
}

{
  aj_caste |>
    plot_quitting() -> p

  p$data <- p$data |>
    filter(strata != "Not Stated")

  p
}

## Education --------------------------------------------------------------

aj_edu <- cuminc(Surv(time, event) ~ edu, data = combined_data)

qs_save(aj_edu, here(write_location, "aj_edu.qs2"))

aj_edu |>
  plot_re_employment()
aj_edu |>
  plot_quitting()

## Relation ---------------------------------------------------------------

aj_relation <- cuminc(Surv(time, event) ~ relation, data = combined_data)

qs_save(aj_relation, here(write_location, "aj_relation.qs2"))

aj_relation |>
  plot_re_employment()
aj_relation |>
  plot_quitting()


# Log-Logistic Model -----------------------------------------------------

ll_re_employment <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    religion +
    caste +
    edu +
    relation,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == "censored" ~ 0,
        event == "Re-Employment" ~ 1,
        event == "Quitting" ~ 0
      )
    ),
  dist = "loglogistic"
)

ll_quitting <- survreg(
  Surv(time, event) ~ age +
    family_income_thousands +
    religion +
    caste +
    edu +
    relation,
  data = combined_data |>
    mutate(time = time + 1) |>
    mutate(
      event = case_when(
        event == "censored" ~ 0,
        event == "Re-Employment" ~ 0,
        event == "Quitting" ~ 1
      )
    ),
  dist = "loglogistic"
)

qs_save(ll_re_employment, here(write_location, "ll_re_employment.qs2"))
qs_save(ll_quitting, here(write_location, "ll_quitting.qs2"))

ll_re_employment |>
  tidy() |>
  select(term, estimate, p.value) |>
  filter(term != "(Intercept)") |>
  mutate(
    estimate_num = exp(estimate),
    estimate = paste0(
      sprintf("%.3f", estimate_num),
      case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1 ~ "*",
        TRUE ~ ""
      )
    ),
    category = case_when(
      term %in%
        c(
          "age",
          "family_income_thousands"
        ) ~ "",
      grepl("^religion", term) ~ "Religion (Baseline: Hindu)",
      grepl("^caste", term) ~ "Caste (Baseline: Upper Caste)",
      grepl("^edu", term) ~ "Education (Baseline: No Schooling)",
      grepl(
        "^relation",
        term
      ) ~ "Relation to Head of Household (Baseline: Self)",
      term == "Log(scale)" ~ " "
    ),
    term = case_when(
      term == "age" ~ "Age",
      term == "family_income_thousands" ~ "Family Income (INR 1000)",
      term == "religionMuslim" ~ "Muslim",
      term == "religionOther" ~ "Other",
      term == "casteOBC/Intermediate Caste" ~ "OBC / Intermediate Caste",
      term == "casteSC" ~ "SC",
      term == "casteST" ~ "ST",
      term == "casteNot Stated" ~ "Not Stated",
      term == "eduPrimary School" ~ "Primary School",
      term == "eduMiddle School" ~ "Middle School",
      term == "eduHigh School" ~ "High School",
      term == "eduVocational Education" ~ "Vocational Education",
      term == "eduHigher Education" ~ "Higher Education",
      term == "relationSon" ~ "Son",
      term == "relationOther" ~ "Other",
      term == "Log(scale)" ~ "Scale",
      TRUE ~ term
    ),
    order = case_when(
      term == "Log(scale)" ~ 2L,
      TRUE ~ 1L
    )
  ) |>
  arrange(order) |>
  select(category, term, estimate, estimate_num) |>
  gt(groupname_col = "category") |>
  cols_label(
    term = "Variable",
    estimate = "Time Ratio"
  ) |>
  cols_hide(columns = estimate_num) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_text(
      weight = "bold",
      size = px(22)
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(color = "#1E8449"),
    locations = cells_body(
      columns = estimate,
      rows = estimate_num < 1 & term != "Scale"
    )
  ) |>
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = estimate,
      rows = estimate_num > 1 & term != "Scale"
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "n = ",
        format(nobs(ll_re_employment), big.mark = ",")
      )
    )
  ) |>
  tab_source_note(
    source_note = html(
      "<em>Note:</em>
        <ol>
      <li>*** p < 0.01, ** p < 0.05, * p < 0.10. </li>
      <li> Estimates are exponentiated coefficients (time ratios), except the scale parameter.</li>
      <li> Observations that quit the labour force were taken to be censored. Given the assumption of non-informative censoring, this means the observations would have had similar probabilities for re-employment had they not quit the labour force.</li>"
    )
  ) |>
  opt_table_font(
    size = px(21)
  ) |>
  tab_options(table.width = pct(60))
