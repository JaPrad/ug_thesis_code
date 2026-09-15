if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(here, tidyverse, glue)

# Wave Number to Duration Label ------------------------------------------

cmie_wave_duration <- function(wave_num, start_date = F) {
  label_vec <- c(
    "Jan-Apr (2014)",
    "May-Aug (2014)",
    "Sep-Dec (2014)",
    "Jan-Apr (2015)",
    "May-Aug (2015)",
    "Sep-Dec (2015)",
    "Jan-Apr (2016)",
    "May-Aug (2016)",
    "Sep-Dec (2016)",
    "Jan-Apr (2017)",
    "May-Aug (2017)",
    "Sep-Dec (2017)",
    "Jan-Apr (2018)",
    "May-Aug (2018)",
    "Sep-Dec (2018)",
    "Jan-Apr (2019)",
    "May-Aug (2019)",
    "Sep-Dec (2019)",
    "Jan-Apr (2020)",
    "May-Aug (2020)",
    "Sep-Dec (2020)",
    "Jan-Apr (2021)",
    "May-Aug (2021)",
    "Sep-Dec (2021)",
    "Jan-Apr (2022)",
    "May-Aug (2022)",
    "Sep-Dec (2022)",
    "Jan-Apr (2023)",
    "May-Aug (2023)",
    "Sep-Dec (2023)",
    "Jan-Apr (2024)",
    "May-Aug (2024)",
    "Sep-Dec (2024)",
    "Jan-Apr (2025)",
    "May-Aug (2025)",
    "Sep-Dec (2025)",
    "Jan-Apr (2026)"
  )

  if (wave_num < 1 | wave_num > length(label_vec)) {
    warning(paste0("Enter integer between 1 and ", length(label_vec)))
    return(NULL)
  }

  duration_label <- label_vec[[wave_num]]

  if (start_date == T) {
    start_month <- sub("-.*", "", sub(" .*", "", duration_label))
    year <- sub(".*\\(([0-9]{4})\\)", "\\1", duration_label)

    as.Date(
      paste(start_month, "1", year),
      format = "%b %d %Y"
    )
  } else {
    return(duration_label)
  }
}

cphs_get_id <- function(cphs_data) {
  cphs_data |>
    mutate(id = paste0(HH_ID, MEM_ID))
}

cphs_classify_caste <- function(cphs_data) {
  cphs_data |>
    mutate(
      caste_category = case_when(
        caste_category == "Upper Caste" ~ "uc",
        caste_category %in% c("OBC", "Intermediate Caste") ~ "obc_ic",
        caste_category %in% c("SC", "ST") ~ "sc_st",
        .default = "not_stated"
      )
    )
}

cphs_classify_religion <- function(cphs_data) {
  cphs_data |>
    mutate(
      religion = case_when(
        religion == "Hindu" ~ "hindu",
        religion == "Muslim" ~ "muslim",
        .default = "other"
      )
    )
}


#' Convert a wave id to corresponding POI filename
#'
#' @param waveid A string in yyyy_w format where yyyy is the year between 2014 and 2016 and w is the wave number in te year between 1 and 3
#'
#' @returns A string of POI dataset filename
#'
#' @export
#' @examples cphs_waveid_to_poi_filename("2019_1")
cphs_waveid_to_poi_filename <- function(waveid) {
  if (!grepl("^20(1[4-9]|2[0-6])_[1-3]$", waveid)) {
    stop("Enter valid waveid")
  }

  year <- as.integer(str_split_fixed(waveid, "_", 2)[, 1])
  wave <- as.integer(str_split_fixed(waveid, "_", 2)[, 2])

  if (wave == 1) {
    filename <- glue("people_of_india_{year}0101_{year}0430_R.csv")
  } else if (wave == 2) {
    filename <- glue("people_of_india_{year}0501_{year}0831_R.csv")
  } else if (wave == 3) {
    filename <- glue("people_of_india_{year}0901_{year}1231_R.csv")
  }
  return(filename)
}

#' Convert a month id to corresponding income filename
#'
#' @param monthid A string in yyyy_mm format where yyyy is the year between 2014 and 2016 and mm is the month between 01 and 12
#' @param level A string, "household" or "member"
#'
#' @returns A string of the corresponding Income dataset filename
#'
#' @examples
#' cphs_monthid_to_income_filename("2024_03", "household")
#' cphs_monthid_to_income_filename("2024_03", "member")
#'
#' @export
cphs_monthid_to_income_filename <- function(monthid, level) {
  if (!grepl("^20(1[4-9]|2[0-6])_(0[1-9]|1[0-2])$", monthid)) {
    stop("Enter valid monthid")
  }

  if (!level %in% c("household", "member")) {
    stop("Enter valid level")
  }

  year <- as.integer(str_split_fixed(monthid, "_", 2)[, 1])
  month <- str_split_fixed(monthid, "_", 2)[, 2]
  month_int <- as.integer(month)

  months_30 <- c(4, 6, 9, 11) # months with 30 days
  months_31 <- c(1, 3, 5, 7, 8, 10, 12) # months with 31 days

  if (month_int %in% months_30) {
    filename <- glue("{level}_income_{year}{month}30_MS_rev.csv")
  } else if (month_int %in% months_31) {
    filename <- glue("{level}_income_{year}{month}31_MS_rev.csv")
  } else if (month_int == 2) {
    if (year %% 4 == 0) {
      filename <- glue("{level}_income_{year}0229_MS_rev.csv")
    } else {
      filename <- glue("{level}_income_{year}0228_MS_rev.csv")
    }
  }

  return(filename)
}

#' Covert monthid to waveid
#'
#' @param monthid A string in yyyy_mm format where yyyy is the year between 2014 and 2016 and mm is the month between 01 and 12
#'
#' @returns A string in yyyy_w format where yyyy is the year between 2014 and 2016 and w is the wave number in te year between 1 and 3
#'
#' @examples
#' monthid_to_waveid("2024_12")
#'
#' @export
monthid_to_waveid <- function(monthid) {
  if (!grepl("^20(1[4-9]|2[0-6])_(0[1-9]|1[0-2])$", monthid)) {
    stop("Enter valid monthid")
  }

  year <- as.integer(str_split_fixed(monthid, "_", 2)[, 1])
  month <- as.integer(str_split_fixed(monthid, "_", 2)[, 2])

  wave <- ceiling(month / 4)

  return(glue("{year}_{wave}"))
}
