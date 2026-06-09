library(tidyverse)


# Extract from data -------------------------------------------------------

source <- "data/cmie/cmie_people_raw"
target <- "data/eda/2026-06-08_mentor_meeting_02"

files <- sort(list.files(source))

files <- c(files[1:18], files[25:36])

lapply(files, function(file_name){

  cat("Working on", file_name, "\n")
  
  df <- read_csv(paste0(source, "/", file_name), show_col_types = FALSE)
  
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

saveRDS(id_list, file = paste0(target, "/id_list_non_covid.Rda"))


# Calculate --------------------------------------------------------------



## Calculate Full Continuity ----------------------------------------------

target <- "data/eda/2026-06-08_mentor_meeting_02"
id_list <- readRDS(paste0(target, "/id_list_non_covid.Rda"))

pick_common_num <- function(num_1, num_2){

  cat("Working on:", num_1, " - ", num_2, "\n")

  index_vec <- num_1:num_2
  index_vec <- index_vec[!index_vec %in% 19:24]
  
  current_list = list()
  
  for(i in index_vec){
    current_list <- append(current_list, list(id_list[[as.character(i)]]))
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
  saveRDS(file = paste0(target, "/panel_continuity_matrix_non_covid.Rda"))


## Calculate P2P Continuity -----------------------------------------------

# Full list from the previous meeting folder used here

target <- "data/eda/2026-06-01_mentor_meeting"
output_target <- "data/eda/2026-06-08_mentor_meeting_02"
id_list <- readRDS(paste0(target, "/id_list.Rda"))

pick_common_num_P2P <- function(num_1, num_2){

  cat("Working on:", num_1, " - ", num_2, "\n")

  id_vec_1 <- id_list[[as.character(num_1)]]
  id_vec_2 <- id_list[[as.character(num_2)]]
  
  common_vec <- intersect(id_vec_1, id_vec_2)
  
  return(length(common_vec))
}

cross_join(
  tibble(num_1 = as.numeric(names(id_list))),
  tibble(num_2 = as.numeric(names(id_list)))
) %>% 
  rowwise() %>% 
  mutate(common_num = pick_common_num(num_1, num_2)) -> p2p_continuity_table

p2p_continuity_table %>% 
  arrange(desc(num_1), desc(num_2)) %>% 
  pivot_wider(names_from = num_2, values_from = common_num) %>% 
  rename(Wave = num_1) %>% 
  saveRDS(file = paste0(output_target, "/panel_continuity_matrix_p2p.Rda"))


