
# Extract Permeability, Porosity, and Nitrogen content by habitat type

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "exactextractr", "raster")                   # List packages
lapply(Packages, library, character.only = TRUE)                           # Load packages
source("./R scripts/@_Region file.R")

habitats <- readRDS("./Objects/Habitats.rds") %>%                          # Import maps of sea bed habitats
  filter(!Habitat %in% c("Rock", "Overhang"))

#### Values provided by case study ####

result <- st_drop_geometry(habitats) %>% 
  mutate(D50 = c(6.452, 0, 0.01288, 0.01606, 0.10964, 0.11423)) 

saveRDS(result, "./Objects/Other habitat parameters.rds")
