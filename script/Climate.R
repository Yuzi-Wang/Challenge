library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

BOM_data_separate <- 
  separate(BOM_data, col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/")
BOM_data_separate

BOM_data_separate %>% 
  filter() %>%
  group_by(Station_number) %>% 
  summarise(num_rows = n())
