
## Extract chlorophyll and primary production data from remote sensing sources

#### set up ####

library(raster)
library(ncdf4)
library(exactextractr)
library(sf)
library(tidyverse)
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>% 
  st_transform(crs = 4326)

#### Extract primary production ####

series <- map(list.files("../Shared data/CMEMS/PP_data/", full.names = TRUE, pattern = ".nc"), ~{
  
  data <- raster(.x)

  PP <- mutate(domains, PP = exact_extract(data, domains, "mean"),
               Date = as.Date(str_sub(str_extract(.x, "[0-9]+"), start = 1, 8), format = "%Y%m%d")) %>% 
        st_drop_geometry()

}) %>% 
  data.table::rbindlist()

ggplot(series) +
  geom_line(aes(x=Date, y = PP, colour = Shore))

Mike <- mutate(series, month = lubridate::month(Date)) %>% 
  group_by(Shore, month) %>% 
  summarise(PP = mean(PP)) 

write.csv(Mike, str_glue("./Objects/fitting/PP_target_{implementation}.csv"), row.names = FALSE)
  
#### Ectract Chl-a ####

series <- map(list.files("../Shared data/CMEMS/CHL_data/", full.names = TRUE, pattern = ".nc"), ~{

  data <- raster(.x)

  PP <- mutate(domains, CHLa = exact_extract(data, domains, "mean"),
      Date = as.Date(str_sub(str_extract(.x, "[0-9]+"), start = 1, 8), format = "%Y%m%d")) %>%
      st_drop_geometry()

}) %>%
  data.table::rbindlist()

ggplot(series) +
  geom_line(aes(x=Date, y = CHLa, colour = Shore))

Mike <- mutate(series, month = lubridate::month(Date)) %>%
  group_by(Shore, month) %>%
  summarise(CHLa = mean(CHLa))

write.csv(Mike, str_glue("./Objects/fitting/CHLa_target_{implementation}.csv"), row.names = FALSE)
