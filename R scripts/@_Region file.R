
## Set repeated commands specific to the project region
implementation <- "Brazilian_Shelf"

library(sf)

#EPSG <- rgdal::make_EPSG()
#EPSG2 <- filter(EPSG, str_detect(note, "Brazil"))
#Descriptions on epsg.io
crs <- 5530                                                              # Specify the map projection for the project

lims <- c(xmin = 4750000, xmax = 6600000, ymin = 6004799, ymax = 7740000)# Specify limits of plotting window, also used to clip data grids

zoom <- coord_sf(xlim = c(lims[["xmin"]], lims[["xmax"]]), ylim = c(lims[["ymin"]], lims[["ymax"]]), expand = FALSE) # Specify the plotting window for SF maps in this region

ggsave_map <- function(filename, plot) {
  ggsave(filename, plot, scale = 1, width = 12, height = 10, units = "cm", dpi = 500)
  
}                             # Set a new default for saving maps in the correct size
pre <- list(scale = 1, width = 12, height = 10, units = "cm", dpi = 500) # The same settings if you need to pass them to a function in MiMeMo.tools

SDepth <- 50                  # Shallow deep boundary
DDepth <- 500                 # Overhang depth

#### bathymetry.5 MODEL DOMAIN ####

shape <- function(matrix) {
  
shape <-  matrix %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = implementation,.)
  st_crs(shape) <- st_crs(4326)                                        
  shape <- st_transform(shape, crs = crs)
  return(shape)
  
}                      # Convert a matrix of lat-lons to an sf polygon

Region_mask <- matrix(c(-53.5, -33.75,
                        -50, -36,
                        -42.2, -26,
                        -42.2, -22,
                        -50.3, -22,
                        -50.3, -30.3,
                        -51.25, -31.5,
                        -52.5, -32.25,
                        -52.5, -32.75,
                        -52.75, -33.2,
                        
                        -53.05, -33.5,
                        -53.5, -33.75),
                       ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = implementation,.)
st_crs(Region_mask) <- st_crs(4326)                                        
Region_mask <- st_transform(Region_mask, crs = crs)

#### bounds.2 MAKE TRANSECTS ####

## Polygons to mark which transects are along the open ocean-inshore boundary

Inshore_Ocean1 <- matrix(c(-53.5, -50,  -50, -53.5, -53.5,   # Longitudes
                           -33.7, -35.95, -36.05, -33.8, -33.7), ncol = 2, byrow = F) %>% 
  shape()

Inshore_Ocean2 <- matrix(c(-42.15, -42.15, -42.25, -42.25, -42.15,               # Longitudes
                           -26, -22, -22, -26, -26), ncol = 2, byrow = F) %>% 
  shape()


Inshore_ocean_boundaries <- rbind(Inshore_Ocean1, Inshore_Ocean2)

rm(Inshore_Ocean1, Inshore_Ocean2)

#### expand polygon for sampling rivers ####

river_expansion <- matrix(c(-42.2, -22,
                            -42.2, -36,
                            -50, -36,
                            -55, -32.9,
                            -42.2, -22),
                          ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = implementation,.)
st_crs(river_expansion) <- st_crs(4326)                                          
river_expansion <- st_transform(river_expansion, crs = crs)


