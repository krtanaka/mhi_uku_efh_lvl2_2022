################################################
### add sector information to site visit #######
################################################
# MUST CONNECT TO THE N DRIVE 

rm(list = ls())

library(readr)
library(rgdal) # needed for working with shapefiles
library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(colorRamps)

load("outputs/diversity_index_2023-06-15'.Rdata")
df = sdmtmb_output$grid %>% filter(island == "Guam")
plot(df$X, df$Y, pch = ".", col = 2); map(add = T)

# create a spatial object
latlon = df[,c("X", "Y")]*1000
coordinates(latlon) = ~X+Y

shp <- rgdal::readOGR("N:/GIS/Projects/CommonMaps/Sector/gua_base_land_openwater_mpa_finalize.shp")
CRS.new <- CRS("+proj=utm +zone=55 +datum=WGS84 +units=km +no_defs +north")

proj4string(latlon) <- CRS.new
proj4string(shp) <- CRS.new # WARNING MESSAGE OK
area <- over(latlon, shp)

# combine with site data
df = cbind(df, as.data.frame(area))

df = df %>% dplyr::select(names(df)[1:8], SEC_NAME)

df %>%
  ggplot(aes(X , Y, fill = SEC_NAME, color = SEC_NAME)) +
  geom_point(shape = 21, size = 2) + 
  coord_fixed()

df %>% 
  group_by(SEC_NAME) %>%
  mutate(h = mean(h, na.rm = T)) %>%
  ggplot(aes(X, Y, fill = h)) + 
  geom_raster() +
  # coord_fixed() +
  labs(x = "Longitude", y = "Latitude") +
  # facet_wrap(~year, scales = "free") +
  scale_fill_viridis_c(expression(italic("H"))) + 
  theme(legend.position = c(0.1, 0.85),
        legend.background = element_blank())
