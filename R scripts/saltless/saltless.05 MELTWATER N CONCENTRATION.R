
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

library(tidyverse)
source("./R scripts/@_Region file.R")                                       # Define project region 

water <- read.csv("./Data/River_N/Greenland Discharge.csv") %>%             # Import river discharge
  mutate(Day = round(Day)) %>%                                              # Round time steps to nearest day
  group_by(Day) %>%                                                         # Average value per day
  summarise(Discharge = mean(Discharge.m3s.1.))
  
nitrate <- read.csv("./Data/River_N/NO3.csv") %>%                           # Import Nitrate concentrations
  mutate(Day = round(Day)) %>%                                              # Round time steps to nearest day
  group_by(Day) %>%                                                         # Average value per day
  summarise(NO3 = mean(NO3..mumol.))

ammonia <- read.csv("./Data/River_N/NH4.csv") %>%                           # Import Ammonia concentrations 
  mutate(Day = round(Day)) %>%                                              # Round time steps to nearest day
  group_by(Day) %>%                                                         # Average value per day
  summarise(NH4 = mean(NH4..mumol.))

#### align time series ####

all <- full_join(water, nitrate) %>% 
  full_join(ammonia) %>% 
  full_join(data.frame(Day = seq(min(.$Day), max(.$Day), 1))) %>% 
  arrange(Day) %>% 
  mutate(Discharge = zoo::na.approx(Discharge, maxgap = 4, rule = 2),
         NO3 = zoo::na.approx(NO3, maxgap = 4),
         NH4 = zoo::na.approx(NH4, maxgap = 4))

#### Model ####

model.NO3 <- nls(NO3 ~ a * (1-b)^Discharge, data =         #fitting exponential decay function
                   all, start = list(a = 4, 
                                     b = 0.02))                    #provide estimates of start parameters, then model tries to converge

model.NH4 <- nls(NH4 ~ a * (1-b)^Discharge, data =         #fitting exponential decay function
                   all, start = list(a = 1, 
                                     b = 0.02))                    #provide estimates of start parameters, then model tries to converge
summary(model.NO3)
summary(model.NH4)

Predictions <- all %>% 
  mutate(pred_NO3 = predict(model.NO3, all),
         pred_NH4 = predict(model.NH4, all)) %>% 
  drop_na

ggplot(Predictions) +
  geom_point(data = all, aes(x = Discharge, y = NO3), colour = "red") +
  geom_line(aes(x = Discharge, y = pred_NO3), colour= "red") +
  geom_point(data = all, aes(x = Discharge, y = NH4)) +
  geom_line(aes(x = Discharge, y = pred_NH4)) +
  theme_minimal()

#### Adjust flow rates ####

Total <- readRDS("./Objects/daily freshwater total.rds") %>% 
 # filter(Year == 2099) %>% 
  mutate(Discharge = Runoff/21,
         Y = 2) %>% 
  mutate(pred_NO3 = predict(model.NO3, .),
         pred_NH4 = predict(model.NH4, .),
         Date = as.Date(paste0("1/", Month, "/", Year), format = "%d/%m/%Y"))
  

ggplot(Predictions) +
  geom_point(data = all, aes(x = Discharge, y = NO3), colour = "red") +
  geom_line(aes(x = Discharge, y = pred_NO3), colour= "red") +
  geom_point(data = all, aes(x = Discharge, y = NH4)) +
  geom_line(aes(x = Discharge, y = pred_NH4)) +
  geom_point(data = Total, aes(x = Discharge, y = pred_NO3), colour = "blue") +
  geom_point(data = Total, aes(x = Discharge, y = pred_NH4), colour = "blue") +
  theme_minimal()

ggplot(Total) +
  #geom_point(aes(x = Date, y = pred_NO3), colour = "red") +
  geom_line(aes(x = Date, y = pred_NO3), colour= "red") +
  #geom_point(aes(x = Date, y = pred_NH4)) +
  geom_line(aes(x = Date, y = pred_NH4)) +
  theme_minimal()

saveRDS(select(Total, -c(Runoff, Discharge, Y)), "./Objects/River N.rds")
