### test
# Open the GEBCO netcdf file and limit to a workable size, reshape for tidyverse, and extract contours

#### Set up ####

rm(list=ls())                                                                       # Wipe the brain

packages <- c("tidyverse", "sf", "magick")                                          # List packages
lapply(packages, library, character.only = TRUE)                                    # Load packages

domain <- readRDS("./Objects/Habitats.rds") 

ggplot(data = domain) +
  geom_sf(fill = NA, colour = "white", size = 0.1) +
theme(
  panel.background = element_rect(fill = "black", colour = NA),
  plot.background = element_rect(fill = "black", colour = NA),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "null"),
  panel.spacing = unit(c(0, 0, 0, 0), "null"),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank(),
  legend.position = "none",
  axis.ticks.length = unit(0, "null"),
)

ggsave("./Implementation doc/img/Back.png", width = 10, height = 10, units = "cm", dpi = 500)

image_read("./Implementation doc/img/Back.png") %>% 
  image_crop("1924", gravity = "Center") %>%                    # Remove white border
  image_border("#000000", "250x1530") %>%                       # Extend the black border
  image_crop("x6088", gravity = "South") %>%                    # Shift the map up 
  image_write("./Implementation doc/img/Back.png")
