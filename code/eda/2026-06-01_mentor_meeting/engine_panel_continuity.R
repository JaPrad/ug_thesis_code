library(tidyverse)


# Extract from data -------------------------------------------------------

source = "data/cmie/cmie_people_raw"
target = "data/eda/2026-06-01_mentor_meeting"

files = list.files(source)

lapply(files, function(file_name){
  
  df <- read_csv(paste0(source, "/", file_name))
  
  df %>% 
    filter(GENDER == "M") %>% 
    filter(R_MEM_WGT_W != 0) %>% 
    filter(RESPONSE_STATUS == "Accepted") -> df # take males who accepted response
  
  df %>% 
    mutate(id = paste0(HH_ID, MEM_ID)) -> df
  
  wave_num <- df$WAVE_NO[1]
  responses <- df$id
  
  return(list(wave_num, responses))
}) -> nested_list

id_list <- setNames(
  lapply(nested_list, `[[`, 2),
  sapply(nested_list, `[[`, 1)
)

saveRDS(id_list, file = paste0(target, "/id_list.Rda"))



# Calculate ---------------------------------------------------------------

target = "data/eda/2026-06-01_mentor_meeting"

id_list <- readRDS(paste0(target, "/id_list.Rda"))


pick_common_num <- function(num_1, num_2){
  index_vec <- num_1:num_2
  
  current_list = list()
  
  for(i in index_vec){
    current_list <- append(current_list, list(id_list[[i]]))
  }
  
  common_vec <- Reduce(intersect, current_list)
  
  return(length(common_vec))
}


cross_join(
  tibble(num_1 = as.numeric(names(id_list))),
  tibble(num_2 = as.numeric(names(id_list)))
) %>% 
  rowwise() %>% 
  mutate(common_num = pick_common_num(num_1, num_2)) -> continuity_table

continuity_table %>% 
  arrange(desc(num_1), desc(num_2)) %>% 
  pivot_wider(names_from = num_2, values_from = common_num) %>% 
  rename(Wave = num_1) %>% 
  saveRDS(file = paste0(target, "/panel_continuity_matrix.Rda"))


