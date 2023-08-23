library(raster)
library(dplyr)
library(ggplot2)
library(colorRamps)

rm(list = ls())

# Create an empty data frame to store the results
grid_sizes <- data.frame(lat = numeric(),
                         km2 = numeric())

# Loop through the latitudes from 18N to 23N with a step of 0.01
for (lat in seq(18, 23, 0.01)) {
  
  # Calculate the grid cell size in square kilometers
  grid_size_km2 <- (111.32 * 111.32 * cos(lat * pi / 180)) * 0.01 * 0.01
  
  # Append the latitude and grid cell size to the data frame
  grid_sizes <- rbind(grid_sizes, data.frame(lat = lat,
                                             km2 = grid_size_km2))
}

grid_sizes = grid_sizes %>% 
  mutate(lat = round(lat, 2)) %>% 
  group_by(lat) %>% 
  summarise(km2 = mean(km2)) %>% 
  mutate(lat_g = as.character(lat))

head(grid_sizes)

# https://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeCrm10.html
df = raster("/Users/kisei.tanaka/Desktop/usgsCeCrm10_ff3f_e1be_8d19.nc")

if(min(values(df), na.rm = T) <= 0) {
  
  df[df <= -240] <- NA
  df[df >= 0] <- NA  
  
}

# aggregate to 0.01 dec deg resolution 
df = df %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  mutate(lon = round(x, 2),
         lat = round(y, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(depth = mean(Topography, na.rm = T))

# assign subregional group
df = df %>% 
  as.data.frame() %>% 
  mutate(island_group = case_when(lat > 21.7 & lat < 22.35 & lon > -160.4 & lon < -159.2 ~ "Ni'ihau-Kaua'i",
                                  lat > 21.2 & lat < 21.8 & lon > -158.4 & lon < -157.4 ~ "O'ahu",
                                  lat > 20.4 & lat < 21.4 & lon > -157.9 & lon < -155.9 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                  lat > 18.8 & lat < 20.35 & lon > -156.2 & lon < -154.7 ~ "Hawai'i")) %>% 
  na.omit()

df$island_group <- factor(df$island_group, levels = c("Ni'ihau-Kaua'i", 
                                                      "O'ahu", 
                                                      "Moloka'i-Maui-Lana'i-Kaho'olawe", 
                                                      "Hawai'i"))

df = df %>% mutate(lat_g = as.character(lat))

df = left_join(df, grid_sizes[,c("lat_g", "km2")])

df %>% 
  ggplot(aes(lon, lat, fill = depth)) + 
  geom_tile() + 
  scale_fill_gradientn(colours = matlab.like(100), "Depth (m)") + 
  labs(x = "Longitude", y = "Latitude") + 
  facet_wrap(~island_group, scales = "free")

ggsave(last_plot(), filename = "outputs/depth_0-240m.png", height = 8, width = 10)

efh = df %>% 
  group_by(island_group) %>% 
  summarise(total_efh = sum(km2)) %>% 
  as.data.frame()

efh
