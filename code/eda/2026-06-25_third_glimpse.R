library(tidyverse)


# Wave 36 -----------------------------------------------------------------

df <- read_csv("data/cmie/cmie_people_raw/people_of_india_20250901_20251231_R.csv")

glimpse(df)

df %>% 
  count(GENDER)

df %>% 
  filter(GENDER == "M") -> df


## Remove 0 weights --------------------------------------------------------

df%>% 
  filter(R_MEM_WGT_W != 0) -> df_wt

df_wt %>% 
  count(RESPONSE_STATUS)

df_wt %>% 
  filter(RESPONSE_STATUS == "Accepted") -> df_wt


## Employment status -------------------------------------------------------

df_wt %>% 
  distinct(EMPLOYMENT_STATUS)

df_wt %>% 
  count(EMPLOYMENT_STATUS)

df_wt %>% 
  count(EMPLOYMENT_STATUS) %>% 
  mutate(prop = (n/sum(n))*100)


## Employment Arrangement --------------------------------------------------

df_wt %>% 
  count(EMPLOYMENT_ARRANGEMENT)


## Occupation --------------------------------------------------------------

df_wt %>% 
  count(OCCUPATION)


## Nature of Occupation ----------------------------------------------------

df_wt %>% 
  count(NATURE_OF_OCCUPATION)


## Industry of Occupation --------------------------------------------------

df_wt %>% 
  count(INDUSTRY_OF_OCCUPATION) %>% View()


## Religion ----------------------------------------------------------------


df_wt %>% 
  count(RELIGION) %>% 
  mutate(prop = (n/sum(n))*100)



## Place of Work -----------------------------------------------------------

df_wt %>% 
  count(PLACE_OF_WORK)


## Education ---------------------------------------------------------------

df_wt %>% 
  distinct(EDU)

df_wt %>% 
  count(DISCIPLINE) %>% View() 

df_wt %>% 
  filter(DISCIPLINE == "Not Applicable") %>% 
  count(EDU)

# Wave 1 ------------------------------------------------------------------


df <- read_csv("data/cmie/cmie_people_raw/people_of_india_20140101_20140430_R.csv")

glimpse(df)


## Employment status -------------------------------------------------------

df %>% 
  distinct(EMPLOYMENT_STATUS)

df_wt %>% 
  count(EMPLOYMENT_STATUS)
