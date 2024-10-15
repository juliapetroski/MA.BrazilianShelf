
#### Setup ####
Packages <- c("tidyverse", "tictoc", "dplyr", "bedshear", "future", "future.apply", "lubridate", "data.table", "furrr")                    
lapply(Packages, library, character.only = TRUE) 

data <- readRDS("./Objects/Data_2022.rds")
data[, c("timestep_cmems") := NULL]
setDF(data)

### J: I did per day due to the limitations of my computer
data <- data %>%
  mutate(day = format(ymd_h(timestep), "%Y-%m-%d"))

data_split <- data %>%
  group_by(day) %>%
  group_split()

rm(data)

calc_shear_stress <- function(row) {
  shear_stress(
    bathymetry = row$Bath,
    D50 = row$sed_strath_diameter,
    tidal_velocity = row$uvSpeed,
    tidal_direction = row$uvDirection,
    wave_height = row$swh,
    wave_period = row$wp,
    wave_direction = row$mwd,
    switch = 0
  )
}

process_block <- function(block, batch_num) {
  message(paste("Processing day:", batch_num))
  block <- block %>%
    rowwise() %>%
    mutate(shear_stress_result = list(calc_shear_stress(cur_data()))) %>%
    unnest_wider(shear_stress_result)
  
  saveRDS(block, paste0("stress_day_", batch_num, ".rds"))
  
  return(NULL)
}

process_all_batches <- function(data_split) {
  total_batches <- length(data_split)
  
  for (i in seq_along(data_split)) {
    try({
      process_block(data_split[[i]], i)
      message(paste("Completed", i, "of", total_batches, "days."))
    }, silent = TRUE)
  }
}

plan(multisession, workers = 6)

tic()
process_all_batches(data_split)
toc()

rm(data_split)

result_files <- list.files(pattern = "stress_day_.*\\.rds")
results <- lapply(result_files, readRDS)
results_combined <- bind_rows(results)

rm(results)

saveRDS(results_combined, "stress_results.rds")

results_combined <- stress_results

results_combined <- results_combined %>%
  mutate(month = format(ymd_h(timestep), "%Y-%m"))

#### Calculate 95th percentile per pixel per month #### 
stress_95 <- results_combined %>%
  group_by(x, y, month) %>%
  summarise(stress95 = quantile(shear_mean, 0.95, na.rm = TRUE)) %>%
  ungroup()

ggplot(stress_95) +
  geom_raster(aes(x=x, y=y, fill = stress95)) +
  facet_wrap(vars(month))

#### Calculate disturbance ####
disturbed <- results_combined %>%
  mutate(movement = shields_number > shields_critical) %>%
  group_by(x, y, month, Shore, Habitat) %>%
  summarise(movement = mean(movement))

ggplot(disturbed) +
  geom_raster(aes(x=x, y=y, fill = movement)) +
  facet_wrap(vars(month))

mean(disturbed$movement)                                    # check this value isn't 0

saveRDS(disturbed, "disturbed_2022.rds")

habs <- group_by(disturbed, month, Shore, Habitat) %>% 
  summarise(movement = mean(movement))

#### Jack can convert after here for StrathE2E pipeline ####

# average across habitat types etc etc


