# Author: Jyotiraditya Pradhan
# Date: October 24, 2026

# Survival Data with E/D/O Competing risk
# Age >= 65 excluded
# This uses panel data from 2026-10-01_mentor_meeting_06

# Load Packages ----------------------------------------------------------

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(here, tidyverse, janitor, glue)
source("code/utilities.R")

# Function ---------------------------------------------------------------

obtain_survival_data <- function(
  monthly_panel,
  continuous_covariates = c("age", "family_income"),
  categorical_covariates = c(
    "state",
    "religion",
    "caste",
    "edu",
    "relation"
  )
) {
  # Pivot the panel data into long format using monthid:
  print("Pivoting data...")

  monthly_panel_long <- monthly_panel |>
    pivot_longer(
      cols = matches("_\\d{4}_\\d{2}$"), # Columns with suffix in _yyyy_mm form
      names_to = c(".value", "month_id"),
      names_pattern = "^(.*)_(\\d{4}_\\d{2})$"
    ) |>
    filter(!is.na(month_slot)) |>
    filter(age_yrs < 65) |>

    # Classify caste
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

    # Classify edu
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

    #  Classify relation
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
    # rename columns for shorter name
    rename(
      family_income = inc_of_all_mems_frm_all_srcs,
      caste = caste_category,
      age = age_yrs,
      relation = relation_with_hoh
    )

  month_ids_sorted <- sort(unique(monthly_panel_long$month_id)) # Get sorted vector of all month ids

  sub_categorical_columns <- lapply(categorical_covariates, function(x) {
    paste0(x, "_", unique(monthly_panel_long[[x]]))
  }) |>
    unlist()

  # Initialize Survival data
  {
    survival_data <- tibble(
      id = character(0),
      time = integer(0),
      event = character(0),
      start_month_id = character(0)
    )
    survival_data[categorical_covariates] <- character(0)
    survival_data[sub_categorical_columns] <- integer(0)
    survival_data[continuous_covariates] <- integer(0)
    survival_data[paste0(continuous_covariates, "_sum")] <- integer(0)
    survival_data[paste0(continuous_covariates, "_current")] <- integer(0)

    ids_surviving_last_month <- character(0) # Initialize vectors for IDs who have been unemployed last month
    ids_n_u_last_month <- character(0) # Initialize vectors for IDs who have not been unemployed last month
  }

  {
    for (t in 1:length(month_ids_sorted)) {
      # Iterate over every month
      cat("Current month:", t, "\n") # Print current month to keep track of loop

      month_id_current <- month_ids_sorted[t] # Current monthid

      # Observation from current month
      obs_current <- monthly_panel_long |>
        filter(month_id == month_id_current) |>
        rename_with(
          \(x) paste0(x, "_current"),
          all_of(continuous_covariates)
        ) # renaming will be useful for updation later

      ids_e_current_month <- filter(
        obs_current,
        employment_status == "E"
      )$id # IDs employed in current month
      ids_u_current_month <- filter(
        obs_current,
        employment_status == "U"
      )$id # IDs unemployed in current month
      ids_d_current_month <- filter(
        obs_current,
        employment_status == "D"
      )$id # IDs discouraged in current month
      ids_o_current_month <- filter(
        obs_current,
        employment_status == "O"
      )$id # IDs olf in current month

      ids_starting_spell <- intersect(
        ids_n_u_last_month,
        ids_u_current_month
      ) # Only take employees non-employed last month to eliminate left censored data
      ids_continuing_spell <- intersect(
        ids_surviving_last_month,
        ids_u_current_month
      )
      ids_surviving_current_month <- union(
        ids_starting_spell,
        ids_continuing_spell
      )

      ids_ending_spell_e <- intersect(
        ids_surviving_last_month,
        ids_e_current_month
      )
      ids_ending_spell_d <- intersect(
        ids_surviving_last_month,
        ids_d_current_month
      )
      ids_ending_spell_o <- intersect(
        ids_surviving_last_month,
        ids_o_current_month
      )
      ids_ending_spell <- Reduce(
        union,
        list(
          ids_ending_spell_e,
          ids_ending_spell_d,
          ids_ending_spell_o
        )
      )

      survival_data |>
        # Add those starting spell
        add_row(
          id = ids_starting_spell,
          time = rep(0, length(ids_starting_spell)),
          event = rep("C", length(ids_starting_spell)),
          start_month_id = rep(month_id_current, length(ids_starting_spell))
        ) |>

        # Update time and event data for others
        mutate(
          time = ifelse(
            id %in% union(ids_continuing_spell, ids_ending_spell),
            time + 1,
            time
          ),
          event = case_when(
            id %in% ids_ending_spell_e ~ "E",
            id %in% ids_ending_spell_d ~ "D",
            id %in% ids_ending_spell_o ~ 'O',
            .default = event
          )
        ) |>

        # Update continuous variables
        rows_update(
          obs_current |>
            select(id, all_of(paste0(continuous_covariates, "_current"))),
          by = "id",
          unmatched = "ignore"
        ) |>
        mutate(
          across(all_of(paste0(continuous_covariates, "_sum")), \(x) {
            variable <- str_remove(cur_column(), "_[^_]*$")
            variable_value_column <- get(paste0(variable, "_current"))

            ifelse(
              id %in% ids_surviving_current_month,
              rowSums(cbind(x, variable_value_column), na.rm = TRUE),
              x
            )
          })
        ) |>

        # Update categorical variables
        mutate(
          across(all_of(sub_categorical_columns), \(x) {
            category <- str_extract(cur_column(), "^[^_]+")
            sub_category <- str_remove(cur_column(), "^[^_]+_")

            ids_sub_category <- obs_current |>
              filter(get(category) == sub_category) |>
              pull(id)

            if_else(
              (id %in% ids_surviving_current_month) &
                (id %in% ids_sub_category),
              coalesce(x, 0) + 1,
              x
            )
          })
        ) -> survival_data

      # Modifications for next iteration
      ids_n_u_last_month <- Reduce(
        union,
        list(
          ids_e_current_month,
          ids_d_current_month,
          ids_o_current_month
        )
      )
      ids_surviving_last_month <- ids_surviving_current_month
    }

    survival_data |>

      # Duration of unemployment spell in months
      mutate(
        spell_duration = ifelse(
          event != "C", # If ended in getting re-employed
          # Note: time starts from 0 but spell_duration will start from 1
          time, # spell ended in the previous time period => spell_duration = (time + 1) - 1
          time + 1 # If censored, spell_duration = time + 1
        ),
        .after = start_month_id
      ) |>

      # Find average of continuous variables
      mutate(
        across(all_of(continuous_covariates), \(x) {
          sum_col <- get(paste0(cur_column(), "_sum")) # get the corresponding column with sum
          sum_col / spell_duration # take average
        })
      ) |>
      select(-starts_with(paste0(continuous_covariates, "_"))) |> # remove sum and current columns

      # Find max of sub categorical columns
      mutate(
        across(all_of(categorical_covariates), \(x) {
          prefix <- paste0(cur_column(), "_")
          col_names <- sub_categorical_columns[startsWith(
            sub_categorical_columns,
            prefix
          )] # get sub categorical columns corresponding to the column

          values <- as.matrix(survival_data[, col_names]) # convert values from those columns to a matri

          all_na <- rowSums(!is.na(values)) == 0 # identify rows with all NA values
          values[is.na(values)] <- -Inf # convert NA values to -Inf to not interfere with finding max

          result <- col_names[
            max.col(values, ties.method = "random")
          ] # find names of column with maximum value (using indexing)

          result <- str_remove(result, fixed(prefix)) # remove prefix
          result[all_na] <- NA_character_ # convert to NA for all NA rows

          result
        })
      ) |>
      select(-all_of(sub_categorical_columns)) -> survival_data
  }

  return(survival_data)
}

# Pre-Covid -------------------------------------------------------------

## Read Data --------------------------------------------------------------

monthly_panel_pre_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "monthly_panel_pre_covid.csv"
  )
)

## Construction -----------------------------------------------------------

survival_data_pre_covid <- obtain_survival_data(
  monthly_panel_pre_covid
)


## Storage ----------------------------------------------------------------

write_csv(
  survival_data_pre_covid,
  here(
    "data",
    "eda",
    "2026-10-24_mentor_meeting_07",
    "survival_data_pre_covid.csv"
  )
)

# Post-Covid --------------------------------------------------------------

## Read Data --------------------------------------------------------------

monthly_panel_post_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "monthly_panel_post_covid.csv"
  )
)


## Construction -----------------------------------------------------------

survival_data_post_covid <- obtain_survival_data(
  monthly_panel_post_covid
)

write_csv(
  survival_data_post_covid,
  here(
    "data",
    "eda",
    "2026-10-24_mentor_meeting_07",
    "survival_data_post_covid.csv"
  )
)
