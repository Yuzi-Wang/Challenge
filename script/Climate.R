library(tidyverse)

#Q1

BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

BOM_data_separate <- 
  separate(BOM_data, col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/")
# split the col into two diff one

BOM_data_separate

View(BOM_data_separate)

BOM_data_separate %>% 
  filter(Temp_min >= 0, Temp_max >= 0, Rainfall >= 0) %>% 
  # retain the raws that have values
  group_by(Station_number) %>% 
  # group by the satation
  summarise(num_rows = n()) # count the nbr of days

#Q2

BOM_data_tidy <- BOM_data_separate %>% 
  filter(Temp_min >= 0, Temp_max >= 0, Rainfall >= 0)
# Save this filtered data frame

BOM_data_tidy

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
  group_by(Month) %>% # group by monthe
  summarise(Mean_temp = mean(Temp_diff)) %>% # caculate the average for each month
  arrange(Mean_temp) # order the average


#Q3:

BOM_station_tidy <- BOM_stations %>% 
  gather(Station_number, number, -info) %>% 
  spread(info, number)
#tidy the data

BOM_station_tidy

BOM_station_num <- BOM_station_tidy %>% 
  mutate(Station_number = as.numeric(Station_number))
#change the chr into num for the next join step

BOM_station_num

joined_BOM <- full_join(BOM_station_num, Data_with_Temp_diff)
#join the two dataset

view(joined_BOM)

joined_BOM %>% 
  group_by(state) %>% # group by state
  summarise(mean_Temp_diff = mean(Temp_diff)) %>% # caculate the average for each state
  arrange(mean_Temp_diff)# order the average

# Q4:

joined_BOM %>% 
  filter(Solar_exposure != "NA") %>% 
  mutate(lon = as.numeric(lon)) %>% 
  filter(lon == max(lon)) %>% 
  summarise(mean_solar = mean(Solar_exposure))

joined_BOM %>% 
  filter(Solar_exposure != "NA") %>% 
  mutate(lon = as.numeric(lon)) %>% 
  filter(lon == min(lon)) %>% 
  summarise(mean_solar = mean(Solar_exposure))

  
