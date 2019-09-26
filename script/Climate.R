library(tidyverse)

#Q1
BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

BOM_data_separate <- 
  separate(BOM_data, col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/")
BOM_data_separate

View(BOM_data_separate)
BOM_data_separate %>% 
  filter(Temp_min >= 0 | Temp_max >= 0 | Rainfall >= 0) %>% 
  group_by(Station_number) %>% 
  summarise(num_rows = n())

#Q2
BOM_data_tidy <- BOM_data_separate %>% 
  filter(Temp_min >= 0, Temp_max >= 0, Rainfall >= 0)
BOM_data_tidy
# retain the raws which havr values

BOM_data_num <- BOM_data_tidy %>% 
  mutate(
    Temp_max = as.numeric(Temp_max), 
    Temp_min = as.numeric(Temp_min), 
    Rainfall = as.numeric(Rainfall),
    Solar_exposure = as.numeric(Solar_exposure))
BOM_data_num
#transfer chr in to num

Data_with_Temp_diff <- mutate(BOM_data_num, Temp_diff = Temp_max - Temp_min)
# add a new col and calculate the difference 

Data_with_Temp_diff %>% 
  group_by(Month) %>%
  summarise(Mean_temp = mean(Temp_diff)) %>% 
  arrange(Mean_temp)

#Q3:

BOM_stations
view(BOM_stations)

gather()