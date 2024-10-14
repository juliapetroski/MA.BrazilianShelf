
# Ideally process a month or day o whatever makes sense with the file architecture, then iterate

#### Work on the shared CMEMS grid for all water movements ####

#### Add Grain size data to the grid ####

library(terra)
look <- rast("./Data/Sediment/sed_strath_diameter.tif")
plot(look)

diameter <- data.frame(distinct(depths[,1:2]), extract(look, as.matrix(distinct(depths[,6:5])))) %>% 
  mutate(sed_strath_diameter = 2^(sed_strath_diameter*-1))                      # Converting to phi to mm gets us some movement!

ggplot(data = diameter) +
  geom_raster(aes(x, y, fill = log10(sed_strath_diameter)))

#### tides #####

# We have tides at the surface, Julia found a pdf rom NOC which said you can approximate depth-averaged current as surface / 1.07

# Tides are on a time step of XXX

#### Currents ####

# Currents are on a time step of XXX
# only need depth-averaged

#### Combined currents ####

# We can just sum tidal and current componenets on UV, then convert to direction and speed for waves and shear stress calculations

#### Waves ####

# Waves are on a tide-step of XXX
# Need to interpolate missing coastal cells

