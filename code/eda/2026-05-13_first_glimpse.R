library(tidyverse)

df <- read_csv("data/cmie_income_raw/member_income_2025_12.csv")


glimpse(df)

df %>% count(CASTE_CATEGORY, RELIGION) %>% View()

df %>% 
  filter(CASTE == "Data Not Available") %>% 
  count(RESPONSE_STATUS)

df %>% 
  count(RESPONSE_STATUS)


df %>% 
  filter(CASTE == "Not Applicable") %>% 
  count(RELIGION)



# Accepted Responses Only -------------------------------------------------


df %>% 
  filter(RESPONSE_STATUS == "Accepted") -> df_acc

glimpse(df_acc)

df_acc %>% 
  count(EDU)

df_acc %>% 
  count(DISCIPLINE) %>% View()

df_acc %>% 
  count(NATURE_OF_OCCUPATION) %>% View()

df_acc %>% 
  count(INDUSTRY_OF_OCCUPATION) %>% View()

df_acc %>% 
  count(OCCUPATION) %>% View()


# Nature of Occupation ----------------------------------------------------

df_acc %>% 
  filter(NATURE_OF_OCCUPATION == "Not Applicable") %>% 
  slice_sample(n = 10) %>% View()

df_acc %>% 
  filter(NATURE_OF_OCCUPATION == "Not Applicable") %>% 
  count(R_MEM_WGT_MS)

df_acc %>% 
  filter(R_MEM_WGT_MS == 0) %>% 
  count(MEM_STATUS)

df_acc %>% 
  filter(R_MEM_WGT_MS == 0) %>% 
  count(R_MEM_WGT_MS, R_MEM_WGT_FOR_COUNTRY_MS, R_MEM_WGT_FOR_COUNTRY_MS)

# Remove those with 0 weights ---------------------------------------------


df_acc %>% 
  filter(R_MEM_WGT_MS != 0) -> df_acc_p

dim(df_acc_p)



