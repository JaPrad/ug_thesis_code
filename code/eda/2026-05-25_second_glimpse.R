

library(tidyverse)

df <- read_csv("data/cmie_income_raw/member_income_2025_12.csv")


glimpse(df)


# Remove 0 Weights --------------------------------------------------------


df%>% 
  filter(R_MEM_WGT_MS != 0) -> df_wt

df_wt %>% 
  count(RESPONSE_STATUS)

df_wt %>% 
  filter(RESPONSE_STATUS == "Non-Response") %>% 
  View()


# Exploring Occupations ---------------------------------------------------


df_wt %>% 
  names()

df_wt %>% 
  distinct(NATURE_OF_OCCUPATION) %>% View()

df_wt %>% 
  distinct(INDUSTRY_OF_OCCUPATION) %>% View()

df_wt %>% 
  distinct(OCCUPATION) %>% View()

df_wt %>% 
  head() %>% View()
