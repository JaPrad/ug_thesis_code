# Setup ------------------------------------------------------------------

library(tidyverse)
library(here)

files <- list.files("data/cmie/cmie_people_raw") |> 
  sort()

names(files) <- 1:36


# Functions --------------------------------------------------------------

euo_matrix <- function(wave_1, wave_2){

  data_list <- lapply(list(wave_1, wave_2), function(wave){

    file <- files[[wave]]

    read_csv(here("data", "cmie", "cmie_people_raw", file), show_col_types = FALSE) |>

      filter(GENDER == "M") |> 
      filter(R_MEM_WGT_W != 0) |> 
      filter(RESPONSE_STATUS == "Accepted") |> 
      filter(EMPLOYMENT_STATUS != "Not Applicable") |> 
      
      mutate(EUO_STATUS = case_when(
        EMPLOYMENT_STATUS == "Employed" ~ "E",
        EMPLOYMENT_STATUS %in% c(
          "Unemployed, willing and looking for a job",
          "Unemployed, willing but not looking for a job"
        ) ~ "U",
        EMPLOYMENT_STATUS == "Unemployed, not willing and not looking for a job" ~ "O"
      )) |> 
      
      mutate(id = paste0(HH_ID, MEM_ID)) %>% 
      
      return()
  })

  common_id <- intersect(data_list[[1]]$id, data_list[[2]]$id)

  data_list <- lapply(data_list, function(x){
    x |> 
      filter(id %in% common_id) %>%
      return()
  })

  lapply(list("E", "U", "O"), function(state_1){

    wave1_state1_t0 <- data_list[[1]] |> 
      filter(EUO_STATUS == state_1) # Obs from wave_1 in state_1 at t_0 

    wave2_state1_t0 <- data_list[[2]] |> 
      filter(id %in% wave1_state1_t0$id) # Obs from wave_2 in state_0 at t_0

    lapply(list("E", "U", "O"), function(state_2){

      wave2_state2_t1 <- wave2_state1_t0 |> 
        filter(EUO_STATUS == state_2)

      trans_p <- nrow(wave2_state2_t1) / nrow(wave1_state1_t0)
      
      tibble(
        state_1 = state_1,
        state_2 = state_2,
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


w_35_36 <- euo_matrix(35, 36) 

w_35_36 |> 
  pivot_wider(names_from = state_2, values_from = p)


lapply(7:35, function(wave_1){

  cat("Working on Wave", wave_1, "\n")

  wave_2 <- wave_1 + 1

  euo_matrix(wave_1, wave_2) |> 
    mutate(wave = wave_1, .before = 1) %>%
    return()
}) |> 
  bind_rows() -> transition_matrix


write_rds(
  transition_matrix, 
  file = here("data", "eda", "2026-06-08_mentor_meeting_02", "euo_transition.Rda")
)



# Sandbox ----------------------------------------------------------------

# tfp <-here("data", "cmie", "cmie_people_raw", files[[35]])

# read_csv(tfp) |>
#   filter(GENDER == "M") |> 
#   filter(R_MEM_WGT_W != 0) |> 
#   filter(RESPONSE_STATUS == "Accepted") -> data_1 # take males who accepted response

# data_1 |> 
#   filter(EMPLOYMENT_STATUS != "Not Applicable")

# data_1 |> 
#   count(EMPLOYMENT_STATUS)

# data_1 |> 
#   filter(EMPLOYMENT_STATUS == "Unemployed, not willing and not looking for a job") |> 
#   count(OCCUPATION) |> 
#   arrange(desc(n))

# data_1 |> 
#   filter(EMPLOYMENT_STATUS == "Not Applicable") |> 
#   count(OCCUPATION) |> 
#   arrange(desc(n))

# data_1 |> 
#   mutate(euo_status = case_when(
#     EMPLOYMENT_STATUS == "Employed" ~ "E",
#     EMPLOYMENT_STATUS %in% c(
#       "Unemployed, willing and looking for a job",
#       "Unemployed, willing but not looking for a job"
#     ) ~ "U",
#     EMPLOYMENT_STATUS == "Unemployed, not willing and not looking for a job" ~ "O",
#     .default = "Z"
#   )) |> 
#   count(EMPLOYMENT_STATUS, euo_status)

# tibble(state_1 = c("E", "U", "O")) |> 
#   cross_join(tibble(state_2 = c("E", "U", "O")))
  
