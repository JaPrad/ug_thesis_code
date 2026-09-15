# Setup ------------------------------------------------------------------

library(tidyverse)
library(here)

files <- list.files(here("data", "cmie", "cmie_people_raw")) |>
  sort()

names(files) <- 1:37


# Functions --------------------------------------------------------------

euo_matrix <- function(wave_1, wave_2) {
  data_list <- lapply(list(wave_1, wave_2), function(wave) {
    file <- files[[wave]]

    read_csv(
      here("data", "cmie", "cmie_people_raw", file),
      show_col_types = FALSE
    ) |>

      filter(GENDER == "M") |>
      filter(AGE_YRS >= 15) |>
      filter(RESPONSE_STATUS == "Accepted") |>
      filter(EMPLOYMENT_STATUS != "Not Applicable") |>

      mutate(
        EUO_STATUS = case_when(
          EMPLOYMENT_STATUS == "Employed" ~ "E",
          EMPLOYMENT_STATUS ==
            "Unemployed, willing and looking for a job" ~ "U",
          EMPLOYMENT_STATUS %in%
            c(
              "Unemployed, willing but not looking for a job",
              "Unemployed, not willing and not looking for a job"
            ) ~ "O"
        )
      ) |>

      mutate(id = paste0(HH_ID, MEM_ID)) %>%

      return()
  })

  common_id <- intersect(data_list[[1]]$id, data_list[[2]]$id) # take common IDs

  # Retain only common data
  data_list <- lapply(data_list, function(x) {
    x |>
      filter(id %in% common_id) %>%
      return()
  })

  # For each state at t0:
  lapply(list("E", "U", "O"), function(state_t0) {
    wave1_state_t0 <- data_list[[1]] |>
      filter(EUO_STATUS == state_t0) # Obs from wave_1 with state_t0

    wave2_state_t0 <- data_list[[2]] |>
      filter(id %in% wave1_state_t0$id) # Obs from wave_2 with state_t0

    # For each state at t1:
    lapply(list("E", "U", "O"), function(state_t1) {
      wave2_state_t0t1 <- wave2_state_t0 |>
        filter(EUO_STATUS == state_t1) # Obs in wave_2 with state_t0 and state_t1

      wave1_state_t0t1 <- wave1_state_t0 |>
        filter(id %in% wave2_state_t0t1$id) # Obs in wave_1 with state_t0 and state_t1

      trans_p <-
        sum(
          wave1_state_t0t1$R_GE15_MEM_WGT_FOR_COUNTRY_W *
            wave1_state_t0t1$GE15_MEM_NR_FOR_COUNTRY_W
        ) /
        sum(
          wave1_state_t0$R_GE15_MEM_WGT_FOR_COUNTRY_W *
            wave1_state_t0$GE15_MEM_NR_FOR_COUNTRY_W
        )

      tibble(
        state_t0 = state_t0,
        state_t1 = state_t1,
        p = trans_p
      ) %>%
        return()
    }) |>
      bind_rows() %>%
      return()
  }) |>
    bind_rows() %>%
    mutate(n = length(common_id)) %>%
    return()
}


# Implementation ---------------------------------------------------------

## Test for Wave 35 & 36 --------------------------------------------------

w_35_36 <- euo_matrix(35, 36)

w_35_36 |>
  pivot_wider(names_from = state_t1, values_from = p)


## Full implementation ----------------------------------------------------

lapply(7:36, function(wave_1) {
  cat("Working on Wave", wave_1, "\n")

  wave_2 <- wave_1 + 1

  euo_matrix(wave_1, wave_2) |>
    mutate(wave = wave_1, .before = 1) %>%
    return()
}) |>
  bind_rows() -> transition_matrix


write_rds(
  transition_matrix,
  file = here(
    "data",
    "eda",
    "2026-08-17_mentor_meeting_04",
    "euo_transitions.Rda"
  )
)

# Sandbox ----------------------------------------------------------------

# tfp <-here("data", "cmie", "cmie_people_raw", files[[35]])

# dt <- read_csv(tfp)

# dt  |>
#   filter(GENDER == "M") |>
#   filter(AGE_YRS >= 15 )
#   filter(RESPONSE_STATUS == "Accepted") -> data_1 # take males who accepted response

# dt$R_GE15_MEM_WGT_FOR_COUNTRY_W
# dt$GE15_MEM_NR_FOR_COUNTRY_W
