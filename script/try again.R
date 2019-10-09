library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")

BOM_stations <- read_csv("data/BOM_stations.csv")

# Question 1: 
# For each station, how many days have a minimum temperature, a maximum temperature 
# and a rainfall measurement recorded?

# needed to group by station and count the days

BOM_data_separate <- 
  separate(BOM_data, col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/" )

Q1 <- 
  BOM_data_separate %>% 
  filter(Temp_min != "-", Temp_max != "-", Rainfall != "-") %>% 
  group_by(Station_number) %>% 
  summarise(num_days = n()) 

# Question 2: Which month saw the lowest average daily temperature difference?

# calculate temp diff
# calculate the daily temp diff in each month (group by month)
# arrange

BOM_data_tidy <- BOM_data_separate %>% 
  filter(Temp_min != "-", Temp_max != "-", Rainfall != "-")

Q2 <- BOM_data_tidy %>% 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
  group_by(Month) %>% 
  summarise(aver_temp_diff = mean(Temp_diff)) %>% 
  arrange(aver_temp_diff)

# Question 3: Which state saw the lowest average daily temperature difference?

# combine these two dataframes by station number

BOM_stations_tidy <- BOM_stations %>% 
  gather(Station_number, data, -info) %>% 
  spread(info, data) %>% 
  mutate(Station_number = as.numeric(Station_number))

BOM_join <- full_join(BOM_data_tidy, BOM_stations_tidy) %>% 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min))

Q3 <- BOM_join %>% 
  group_by(state) %>% 
  summarise(aver_temp = mean(Temp_diff)) %>% 
  arrange(aver_temp)

# Question 4: Does the westmost (lowest longitude) or eastmost (highest longitude) 
# weather station in our dataset have a higher average solar exposure?

# group by longitude

Q4 <- BOM_join %>% 
  filter(Solar_exposure != "-") %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>% 
  group_by(lon) %>% 
  summarise(aver_solar = mean(Solar_exposure)) %>% 
  arrange(lon)

glimpse(BOM_join)

BOM_join <- BOM_join %>% 
  filter(Solar_exposure != "-") %>% 
  mutate(Temp_min = as.numeric(Temp_min), 
         Temp_max = as.numeric(Temp_max),
         Rainfall = as.numeric(Rainfall),
         Solar_exposure = as.numeric(Solar_exposure),
         elev = as.numeric(elev),
         lat = as.numeric(lat),
         lon = as.numeric(lon)
  )

BOM_join

write_csv(BOM_join, "result/BOM-join.csv")

## 09/10/2019

# Question 1

BOM_join_perth <- filter(BOM_join, Station_number == 9225)

Q1_plot01 <- ggplot(data = BOM_join_perth, 
       mapping = aes(x = Temp_max,
                     y = Temp_min,
                     )) +
  geom_point()
  
Q1_plot02 <- ggplot(data = BOM_join_perth, 
                 mapping = aes(x = Temp_max,
                               y = Rainfall,
                 )) +
  geom_point()

Q1_plot03 <- ggplot(data = BOM_join_perth, 
                    mapping = aes(x = Temp_max,
                                  y = Solar_exposure,
                    )) +
  geom_point()

# Question 2

Q2 <- ggplot(data = BOM_join_perth,
       mapping = aes(x = Temp_max,
                     y = Temp_min,
                     size = Rainfall,
                     color = Solar_exposure)) +
  geom_point() +
  theme(legend.position = c(0.9, 0.3),
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 7),
      legend.key.size = unit(0.15, "cm"))

# Question 3

install.packages("cowplot")

library(cowplot)

Q3 <- plot_grid(Q1_plot01, Q1_plot02, Q1_plot03, Q2) 

ggsave("figures/question_3.png", plot = Q3, width = 10, height = 15)

# Question 4

BOM_data_rainfall <- BOM_data %>% 
  filter(Rainfall != "-") %>% 
  mutate(Rainfall = as.numeric(Rainfall)) 

BOM_aver_rainfall <- BOM_data_rainfall %>%
  group_by(Month, Station_number) %>% 
  summarise(aver_rainfall = mean(Rainfall))

BOM_join_new <- left_join(BOM_aver_rainfall, BOM_station_num)

Q4 <- ggplot(data = BOM_join_new, 
       mapping = aes(x = Month,
                     y = aver_rainfall,
                     group = Station_number,
                     color = Station_number)) +
  geom_line() +
  facet_wrap(~state)

ggsave("figures/question_4.png", plot = Q4)
