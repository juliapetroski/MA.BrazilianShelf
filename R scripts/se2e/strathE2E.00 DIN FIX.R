
library(MiMeMo.tools)

source("./R scripts/@_Region file.R")

domain <- readRDS("./Objects/Domains.rds")

New_data <- readxl::read_excel("./Data/Julia/Ocean_nutrient.xlsx", sheet = "BNDO") %>%
  select(Date = `Date-time`, Longitude = `Longitude [deg]`, Latitude = `Latitude [deg]`, `Depth (m)` = `Depth [m]`,
         `NO3 (ug-at/l)` = `Nitrate (NO3) [umol/l]`, `NH4 (ug-at/l)` = `Ammonia (NH3) [umol/l]`) %>%  # Units don't matter because we average proportions
  drop_na(Latitude, `Depth (m)`) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE) %>% # Convert to sf for spatial filter later
  st_transform(crs = st_crs(domain)) %>%  
  group_by(Latitude, Longitude, Date) %>%                                   # Per cast
  arrange(`Depth (m)`, .by_group = TRUE) %>%                                # Order depths ascending
  ungroup() %>% 
  replace_na(list(`NH4 (ug-at/l)` = 0)) %>% 
  mutate(`Depth (m)` = as.numeric(`Depth (m)`),
         `NO3 (ug-at/l)` = as.numeric(str_replace(`NO3 (ug-at/l)`, ",", ".")),
         `NH4 (ug-at/l)` = as.numeric(str_replace(`NH4 (ug-at/l)`, ",", ".")))

Old_data <- readxl::read_excel("./Data/Julia/Ocean_nutrient.xlsx") %>% 
  select(1:10) %>% 
  drop_na(Latitude, `NO3/NH4`, `Depth (m)`) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>% # Convert to sf for spatial filter later
  st_transform(crs = st_crs(domain)) %>%  
  group_by(Station, Date) %>%                                               # Per cast
  arrange(`Depth (m)`, .by_group = TRUE) %>%                                # Order depths ascending
  ungroup() %>% 
  mutate(`NO2 (ug-at/l)` = as.numeric(ifelse(`NO2 (ug-at/l)` == 'nd', 0, `NO2 (ug-at/l)`))) %>% 
  bind_rows(New_data) %>% 
  replace_na(list(`NO2 (ug-at/l)` = 0))
  
#### Calculate proportion ####

shallow_proportion <- Old_data %>% 
  mutate(weights = calculate_depth_share(`Depth (m)`, min_depth = 0, max_depth = SDepth), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

deep_proportion <- Old_data %>% 
  mutate(weights = calculate_depth_share(`Depth (m)`, min_depth = SDepth, max_depth = DDepth), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

final <- bind_rows(shallow_proportion, deep_proportion) %>%                 # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(Station, Date, Depth_layer) %>% 
  mutate(Proportion = `NH4 (ug-at/l)`/(`NH4 (ug-at/l)`+`NO2 (ug-at/l)`+`NO3 (ug-at/l)`)) %>% 
  drop_na(Proportion) %>% 
  summarise(Proportion = weighted.mean(Proportion, weights),                 # Weighted averages
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
#  st_join(domain) %>%                                                       # Check which are in the model domain
#  st_drop_geometry() %>%                                                    # Simplify the output
#  drop_na() %>% 
  group_by(Depth_layer) %>%  #, Month) %>%                                          # Decided not to group by shore because there were few inshore samples   
  summarise(Proportion = weighted.mean(Proportion, Samples),                # Calculate average, weighting by the number of samples
            Casts = n()) %>%                                                # Number of CTD casts contributing to each estimate
  ungroup() 

saveRDS(final, "./Objects/Ammonia to DIN.rds")


ggplot(final) +
  geom_point(aes(x = Depth_layer, y = Casts, colour = Depth_layer)) +
  theme_minimal()

ggplot(final) +
  geom_point(aes(x = Depth_layer, y = Proportion, colour = Depth_layer)) +
  theme_minimal()

ggplot() +
  geom_sf(data = domain) +
  geom_sf(data = Old_data, aes(fill = Station)) +
  theme_minimal()
