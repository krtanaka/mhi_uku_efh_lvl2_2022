#############################################################################
### Topography, NOAA Coastal Relief Model, 3 arc second, Vol. 10 (Hawaii) ###
### https://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeCrm10.html      ###
### NOAA NGDC   (Dataset ID: usgsCeCrm10)                                 ###
#############################################################################
library(raster)
library(tidyr)
library(dplyr)
topo = raster("G:/GIS/bathymetry/MHI/usgsCeCrm10.nc")
topo = as.data.frame(rasterToPoints(topo))
topo$Topography = ifelse(topo$Topography %in% c(-30:0), topo$Topography, NA)
topo = topo %>% drop_na()

topo %>%
  ggplot(aes(x, y, fill = Topography)) +
  geom_tile(aes(width = 0.005, height = 0.005)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  ggdark::dark_theme_minimal() +
  theme(axis.title = element_blank())

save(topo, file = 'data/Topography_NOAA_CRM_vol10.RData')