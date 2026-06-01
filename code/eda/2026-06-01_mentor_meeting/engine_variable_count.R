library(tidyverse)
library(gt)


files <- list.files("data/cmie/cmie_people_raw")



# Define date range function ----------------------------------------------

date_range_label <- function(filename) {
  dates <- regmatches(
    filename,
    regexec("(\\d{8})_(\\d{8})", filename)
  )[[1]][2:3]
  
  start_date <- as.Date(dates[1], "%Y%m%d")
  end_date   <- as.Date(dates[2], "%Y%m%d")
  
  start_year <- format(start_date, "%Y")
  end_year   <- format(end_date, "%Y")
  
  if (start_year == end_year) {
    sprintf(
      "%s (%s-%s)",
      start_year,
      format(start_date, "%b"),
      format(end_date, "%b")
    )
  } else {
    sprintf(
      "%s - %s",
      format(start_date, "%Y %b"),
      format(end_date, "%Y %b")
    )
  }
}



# Classify education function ----------------------------------------------

classify_education <- function(x) {
  dplyr::case_when(
    x %in% c(
      "Pre School",
      "1st Std. Pass",
      "2nd Std. Pass",
      "3rd Std. Pass",
      "4th Std. Pass"
    ) ~ "Some Education",
    
    x %in% c(
      "5th Std. Pass",
      "6th Std. Pass"
    ) ~ "Primary School",
    
    x %in% c(
      "7th Std. Pass",
      "8th Std. Pass",
      "9th Std. Pass"
    ) ~ "Middle School",
    
    x %in% c(
      "10th Std. Pass",
      "11th Std. Pass",
      "12th Std. Pass"
    ) ~ "High School",
    
    TRUE ~ x
  )
}

# Define Summarize Function -----------------------------------------------

summ <- function(source, variable, target){
  
  files <- list.files(source) # list all files
  
  lapply(files, function(file_name){ # for each file
    
    df <- read_csv(paste0(source, "/", file_name)) # read csv
    
    df %>% 
      filter(GENDER == "M") %>% 
      filter(R_MEM_WGT_W != 0) %>% 
      filter(RESPONSE_STATUS == "Accepted") -> df # take males who accepted response
    
    wave_num <- df$WAVE_NO[1]
    
    if (!(variable %in% names(df))){
      return(NA)
    }
    
    if (variable == "EDU"){
      df %>% 
        mutate(EDU = classify_education(EDU)) -> df
    }
    
    df %>% 
      mutate(var = .data[[variable]]) %>% 
      
      count(var) %>% 
      mutate(prop = (n/sum(n))*100) %>% 
      mutate(pasted_str = paste0(format(n, big.mark = ","), " <br> (", round(prop, 2), "%)")) %>% 
      
      mutate(Wave = wave_num) %>% 
      mutate(Date = date_range_label(file_name)) %>% 
      return()
   }) %>% 
    
    Filter(function(x) !all(is.na(x)), .) %>% 

    bind_rows() %>%
    arrange(desc(n)) %>%

    select(Wave, Date, var, pasted_str) %>%

    pivot_wider(names_from = var, values_from = pasted_str) %>%
    arrange(desc(Wave)) -> output_df

  saveRDS(output_df, file = paste0(target, "/", variable, ".Rda"))

  return(output_df)
}

# Run Summarize Function --------------------------------------------------

source_default = "data/cmie/cmie_people_raw"
target_default = "data/eda/2026-06-01_mentor_meeting"


summ(source_default, "CASTE_CATEGORY", target_default)

summ(source_default, "RELIGION", target_default) 

summ(source_default, "EMPLOYMENT_STATUS", target_default)

summ(source_default, "EMPLOYMENT_ARRANGEMENT", target_default) 

summ(source_default, "NATURE_OF_OCCUPATION", target_default)

summ(source_default, "INDUSTRY_OF_OCCUPATION", target_default) 

summ(source_default, "EDU", target_default)

summ(source_default, "REGION_TYPE", target_default)
  


# Sample size -------------------------------------------------------------

source = "data/cmie/cmie_people_raw"
target = "data/eda/2026-06-01_mentor_meeting"

files <- list.files(source) # list all files

lapply(files, function(file_name){
  df <- read_csv(paste0(source, "/", file_name))
  
  df %>% 
    filter(GENDER == "M") %>% 
    filter(R_MEM_WGT_W != 0) %>% 
    filter(RESPONSE_STATUS == "Accepted") -> df # take males who accepted response
  
  wave_num <- df$WAVE_NO[1]
  
  tibble(
    Wave = wave_num,
    Date = date_range_label(file_name),
    `Size of Valid Sample` = format(nrow(df), big.mark = ",")
  ) %>% 
    return()
  
}) %>% 
  bind_rows() %>% 
  arrange(desc(Wave)) %>% 
  saveRDS(file = paste0(target, "/sample_size.Rda"))




# Sandbox -----------------------------------------------------------------



list(
  tibble(a = 1:3, b = 1:3),
  NA
  ) %>% 
  Filter(function(x) !all(is.na(x)), .) %>% 
  bind_rows()

a <- readRDS("data/eda/2026-06-01_mentor_meeting/religion.Rda")

a %>% View()

a %>% 
  mutate(across(where(is.character), trimws)) %>% 
  gt() %>% 
  fmt_markdown(columns = everything())

tibble(
  a = c(1,2,3),
  b = c("Hi", "Bye <br> Hi", "Die")
    ) %>% 
  gt() %>% 
  fmt_markdown(columns = everything())



tibble(
  a = 1:1000,
  b = 1:1000
) %>% 
  pivot_wider(names_from = a, values_from = b) %>% 
  gt()

df$WAVE_NO[1]


df_wt %>% 
  
  mutate(var = .data[["RELIGION"]]) %>% 
  count(var)
  
  mutate(prop = (n/sum(n))*100)
