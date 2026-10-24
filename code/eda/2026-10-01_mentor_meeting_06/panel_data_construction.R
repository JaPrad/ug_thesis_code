# Author: Jyotiraditya Pradhan
# Date: October 1, 2026

# Load Packages ----------------------------------------------------------

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(here, tidyverse, janitor, glue)
source("code/utilities.R")


# Functions --------------------------------------------------------------

## District Change --------------------------------------------------------

dist_change <- read_csv(
  here(
    "data",
    "eda",
    "2026-09-08_mentor_meeting_05",
    "dist_change.csv"
  )
)

# change_dist_name <- function(district){
#   if(district %in% dist_change$dist_name_1){
#     dist_change |>
#       filter(dist_name_1 == district) |>
#       pull(dist_name_2) %>%
#       return()
#   } else{
#     return(district)
#   }
# } %>%
#   Vectorize()

change_dist_name <- function(district) {
  output <- dist_change$dist_name_2[match(district, dist_change$dist_name_1)]
  output <- ifelse(is.na(output), district, output)

  return(output)
}


## Income Panel -----------------------------------------------------------

obtain_income_panel <- function(start_year, end_year) {
  # Function to obtain panel of combined member and household income data

  obtain_income_current <- function(monthid) {
    # Function to obtain combined member and household income data for current monthid

    # Obtain member data in current monthod:
    member_current <- read_csv(
      here(
        "data",
        "cmie",
        "cmie_income_raw",
        "member",
        cphs_monthid_to_income_filename(monthid, "member") # Gets filename from monthid
      ),
      show_col_types = FALSE
    ) |>
      clean_names() |>
      filter(
        response_status == "Accepted",
        gender == "M",
        region_type == "URBAN",
        age_yrs >= 15
      ) |>
      select(
        hh_id,
        mem_id,
        state,
        district,
        religion,
        caste_category,
        month_slot,
        age_yrs,
        relation_with_hoh,
        edu,
        nature_of_occupation,
        industry_of_occupation,
        inc_of_mem_frm_all_srcs,
        inc_of_mem_frm_wages
      )

    # Obtain household data in current monthid
    household_current <- read_csv(
      here(
        "data",
        "cmie",
        "cmie_income_raw",
        "household",
        cphs_monthid_to_income_filename(monthid, "household")
      ),
      show_col_types = FALSE
    ) |>
      clean_names() |>
      filter(
        response_status == "Accepted",
        region_type == "URBAN"
      ) |>
      select(hh_id, inc_of_all_mems_frm_all_srcs)

    # Combine them
    combined_income <- member_current |>
      left_join(household_current, by = "hh_id") |>
      mutate(id = paste0(hh_id, "_", mem_id), .before = 1) |>
      mutate(
        # convert month to monthid
        month_slot = format(
          as.Date(paste0("01 ", month_slot), format = "%d %b %Y"),
          "%Y_%m"
        )
      ) |>
      mutate(district = change_dist_name(district)) |>
      rename_with(
        # Time varying variables
        ~ paste0(.x, "_", monthid),
        .cols = c(
          religion,
          caste_category,
          month_slot,
          age_yrs,
          relation_with_hoh,
          edu,
          nature_of_occupation,
          industry_of_occupation,
          inc_of_mem_frm_all_srcs,
          inc_of_mem_frm_wages,
          inc_of_all_mems_frm_all_srcs
        )
      )

    return(combined_income)
  }

  income_panel <- NULL # Initializing
  for (year in start_year:end_year) {
    # Loop over each year
    for (month in 1:12) {
      # Loop over each month
      monthid <- ifelse(
        # yyyy_mm
        month < 10,
        glue("{year}_0{month}"),
        glue("{year}_{month}")
      )

      cat("Current monthid:", monthid, "\n")

      income_current <- obtain_income_current(monthid) # Combined household and member income data from current month

      if (year == start_year && month == 1) {
        # If first month, intialize
        income_panel <- income_current
      } else {
        #Else, append
        income_panel |>
          full_join(
            income_current,
            by = c(
              "id",
              "hh_id",
              "mem_id",
              "state",
              "district"
            )
          ) -> income_panel
      }
    }
  }

  return(income_panel)
}

obtain_poi_long <- function(start_year, end_year) {
  # Function to POI panel

  poi_long <- lapply(start_year:end_year, function(year) {
    # Iterate over each year
    lapply(1:3, function(wave) {
      # Iterate over each wave
      waveid <- glue("{year}_{wave}")

      cat("Current waveid:", waveid, "\n")

      # POI data from current wave:
      poi_current <- read_csv(
        here(
          "data",
          "cmie",
          "cmie_people_raw",
          cphs_waveid_to_poi_filename(waveid)
        ),
        show_col_types = FALSE
      ) |>
        clean_names() |>
        filter(
          response_status == "Accepted",
          gender == "M",
          region_type == "URBAN",
          age_yrs >= 15
        ) |>
        select(
          wave_no,
          hh_id,
          mem_id,
          month_slot,
          r_ge15_mem_wgt_for_country_w,
          ge15_mem_nr_for_country_w,
          employment_status
        ) |>
        mutate(
          # Convert to EUDO
          employment_status = case_when(
            employment_status == "Employed" ~ "E",
            employment_status ==
              "Unemployed, willing and looking for a job" ~ "U",
            employment_status ==
              "Unemployed, willing but not looking for a job" ~ "D",
            employment_status ==
              "Unemployed, not willing and not looking for a job" ~ "O",
            .default = NA
          )
        ) |>
        mutate(id = paste0(hh_id, "_", mem_id), .before = 1) |>
        mutate(
          # Convert month to monthid
          month_slot = format(
            as.Date(paste0("01 ", month_slot), format = "%d %b %Y"),
            "%Y_%m"
          )
        )

      return(poi_current)
    }) |>
      bind_rows()
  }) |>
    bind_rows()
}


obtain_monthly_panel <- function(start_year, end_year, income_panel, poi_long) {
  # Function to obtain monthly panel with both income and POI data

  monthly_panel <- income_panel # Initializing output tibble
  for (year in start_year:end_year) {
    # Iterate over each year
    for (month in 1:12) {
      # Iterate over each month
      monthid <- ifelse(
        # yyyy_mm
        month < 10,
        glue("{year}_0{month}"),
        glue("{year}_{month}")
      )

      cat("Current monthid:", monthid, "\n")

      income_panel |>
        filter(
          !is.na(!!sym(paste0("inc_of_mem_frm_wages_", monthid))) # NA wages mean attrition/not interviewed yet
        ) |>
        select(id, ends_with(monthid)) -> current_obs_income # Observations with data on current month

      current_ids <- current_obs_income$id
      current_month_slots <- unique(current_obs_income[[paste0(
        "month_slot_",
        monthid
      )]]) # All slots of actual interview for observation with data in that month

      lapply(current_month_slots, function(current_month_slot) {
        # Iterate over each slot of interview

        current_obs_income |>
          filter(!!sym(paste0("month_slot_", monthid)) == current_month_slot) |>
          pull(id) -> current_month_ids # IDs interviewed in current month slot

        # Obtain POI data from those interviewed in that month slot:
        poi_long |>
          filter(id %in% current_month_ids, month_slot == current_month_slot) |>
          select(
            id,
            wave_no,
            month_slot,
            r_ge15_mem_wgt_for_country_w,
            ge15_mem_nr_for_country_w
          )
      }) |>
        bind_rows() -> current_obs_poi

      # Suffix name with current monthid
      current_obs_poi |>
        select(id, r_ge15_mem_wgt_for_country_w, ge15_mem_nr_for_country_w) |>
        rename(
          !!paste0(
            "r_ge15_mem_wgt_for_country_w_",
            monthid
          ) := r_ge15_mem_wgt_for_country_w,
          !!paste0(
            "ge15_mem_nr_for_country_w_",
            monthid
          ) := ge15_mem_nr_for_country_w
        ) |>
        right_join(monthly_panel, by = "id") -> monthly_panel # Join with monthly panel

      # Next, we shall impute employment status in every month using wage income from current month, and employment status from current and previous wav.
      # Refer to employment_imputation_rule.md

      # Initialize current month's employment status column with NA values
      monthly_panel |>
        mutate(
          !!sym(paste0("employment_status_", monthid)) := NA_character_,
          .after = !!sym(paste0("employment_status_", monthid))
        ) -> monthly_panel

      current_waves <- unique(current_obs_poi$wave_no) # Waves in observations with current POI were interviewed
      for (current_wave in current_waves) {
        # Iterate over each wave
        previous_wave <- current_wave - 1

        # Info from current wave:
        poi_long |>
          filter(wave_no == current_wave, id %in% current_ids) |>
          select(id, month_slot, employment_status) -> current_wave_obs

        # Info from previous wave:
        poi_long |>
          filter(wave_no == previous_wave, id %in% current_ids) |>
          select(id, month_slot, employment_status) -> previous_wave_obs

        # IDs with a specific employment status in current wave:
        ids_current_wave_e <- filter(
          current_wave_obs,
          employment_status == "E"
        )$id
        ids_current_wave_u <- filter(
          current_wave_obs,
          employment_status == "U"
        )$id
        ids_current_wave_d <- filter(
          current_wave_obs,
          employment_status == "D"
        )$id
        ids_current_wave_o <- filter(
          current_wave_obs,
          employment_status == "O"
        )$id

        ids_available_previous_wave <- previous_wave_obs$id # IDs fro which previous wave data is available

        # IDs with a specific employment status in previous wave:
        ids_previous_wave_e <- filter(
          previous_wave_obs,
          employment_status == "E"
        )$id
        ids_previous_wave_u <- filter(
          previous_wave_obs,
          employment_status == "U"
        )$id
        ids_previous_wave_d <- filter(
          previous_wave_obs,
          employment_status == "D"
        )$id
        ids_previous_wave_o <- filter(
          previous_wave_obs,
          employment_status == "O"
        )$id

        employment_status_col <- paste0("employment_status_", monthid) # Suffix column name with monthid
        wage_income_col <- paste0("inc_of_mem_frm_wages_", monthid) # Suffix column name with monthid

        # Employment status imputation. For explanation, refer to employment_imputation_rule.md
        monthly_panel |>
          mutate(
            !!sym(employment_status_col) := case_when(
              is.na(!!sym(wage_income_col)) ~ NA,
              !(id %in% ids_available_previous_wave) ~ !!sym(
                employment_status_col
              ),
              !!sym(wage_income_col) > 0 ~ "E",

              id %in% ids_previous_wave_u ~ "U",
              id %in% ids_previous_wave_d ~ "D",
              id %in% ids_previous_wave_o ~ "O",

              id %in% ids_current_wave_u ~ "U",
              id %in% ids_current_wave_d ~ "D",
              id %in% ids_current_wave_o ~ "O",

              id %in% intersect(ids_previous_wave_e, ids_current_wave_e) ~ "U",

              .default = !!sym(employment_status_col)
            )
          ) -> monthly_panel
      }
    }
  }

  return(monthly_panel)
}

# Pre-Covid --------------------------------------------------------------

## Income Panel -----------------------------------------------------------

#temp <- obtain_income_panel(2016, 2016)

income_panel_pre_covid <- obtain_income_panel(2016, 2019)

write_csv(
  income_panel_pre_covid,
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "income_panel_pre_covid.csv"
  )
)

## POI Long Data ----------------------------------------------------------

poi_long_pre_covid <- obtain_poi_long(2016, 2019)

write_csv(
  poi_long_pre_covid,
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "poi_long_pre_covid.csv"
  )
)


## Monthly Panel ----------------------------------------------------------

income_panel_pre_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "income_panel_pre_covid.csv"
  )
)
poi_long_pre_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "poi_long_pre_covid.csv"
  )
)

monthly_panel_pre_covid <- obtain_monthly_panel(
  2016,
  2019,
  income_panel_pre_covid,
  poi_long_pre_covid
)

write_csv(
  monthly_panel_pre_covid,
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "monthly_panel_pre_covid.csv"
  )
)


# Post Covid -------------------------------------------------------------

## Income Panel -----------------------------------------------------------

income_panel_post_covid <- obtain_income_panel(2022, 2025)

write_csv(
  income_panel_post_covid,
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "income_panel_post_covid.csv"
  )
)


## POI Long ---------------------------------------------------------------

poi_long_post_covid <- obtain_poi_long(2022, 2025)

write_csv(
  poi_long_post_covid,
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "poi_long_post_covid.csv"
  )
)


## Monthly Panel ----------------------------------------------------------

income_panel_post_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "income_panel_post_covid.csv"
  )
)
poi_long_post_covid <- read_csv(
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "poi_long_post_covid.csv"
  )
)

monthly_panel_post_covid <- obtain_monthly_panel(
  2022,
  2025,
  income_panel_post_covid,
  poi_long_post_covid
)

write_csv(
  monthly_panel_post_covid,
  here(
    "data",
    "eda",
    "2026-10-01_mentor_meeting_06",
    "monthly_panel_post_covid.csv"
  )
)
