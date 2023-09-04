
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)
source("./R scripts/@_Region file.R")                                       # Define project region 

#### Clean data ####

NO3 <- readxl::read_excel("./Data/Julia/SBS_rivers_NO3_NH4.xlsx", sheet = "table_NO3") %>% # Get file names
  mutate(NO3 = `With outliers`) %>%                                         # units are mg/L
  mutate(NO3 = milli_to_full(NO3)) %>%                                      # Convert mg to g
  mutate(NO3 = NO3/62.0049 * 1e6) %>%                                       # Convert g to mumol
  mutate(NO3 = NO3 * 1e3) 

NH4 <- readxl::read_excel("./Data/Julia/SBS_rivers_NO3_NH4.xlsx", sheet = "table_NH4") %>% # Get file names
  mutate(NH4 = `With outliers`) %>%                                         # units are mg/L
  mutate(NH4 = milli_to_full(NH4)) %>%                                      # Convert mg to g
  mutate(NH4 = NH4/18.04 * 1e6) %>%                                         # Convert g to mumol
  mutate(NH4 = NH4 * 1e3) 

saveRDS(select(left_join(NO3, NH4, by = "Month"), Month, NO3, NH4), "./Objects/River N.rds")              # Save


