library(raster)
library(dplyr)
library(patchwork)
library(sf)
library(ggplot2)
library(raster)
library(rmapshaper)
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

# read Franklin 2021 Uku EFH lvl1 output, aggregate to 0.01 for faster map plotting
df = raster("/Users/kisei.tanaka/Desktop/efh_sh_dp.tif") %>% 
  rasterToPoints() %>% 
  as.data.frame() %>%
  mutate(lon = round(x, 2),
         lat = round(y, 2),
         est = efh_sh_dp) %>%
  group_by(lon, lat) %>% 
  summarise(est = mean(est))

df %>% 
  ggplot(aes(lon, lat, fill = est)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = matlab.like(100), "")

# derive emprical cumulative distribution function at original resolution
cumulative_dist_df <- raster("/Users/kisei.tanaka/Desktop/efh_sh_dp.tif") %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  na.omit()
cumulative_dist <- ecdf(cumulative_dist_df$efh_sh_dp)
plot(cumulative_dist, bty = "n", xlim = c(0, 1), pch = ".")

for (t in 1:9) {
  
  p95_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.05, type = t)
  p75_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.25, type = t)
  p50_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.5, type = t)
  p25_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.75, type = t)
  
  print(cbind(p95_l1, p75_l1, p50_l1, p25_l1))
  
}

# calculate quantile-based EFH threasholds
p95_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.05)
p75_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.25)
p50_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.5)
p25_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.75)

abline(v = p25_l1, col = 2)
abline(v = p50_l1, col = 2)
abline(v = p75_l1, col = 2)
abline(v = p95_l1, col = 2)

# deifne EFH categories
df$efh = "below threasholds"
df$efh = ifelse(df$est >= p95_l1, "95% EFH", df$efh)
df$efh = ifelse(df$est >= p75_l1, "75% Principal EFH", df$efh)
df$efh = ifelse(df$est >= p50_l1, "50% Core EFH", df$efh)
df$efh = ifelse(df$est >= p25_l1, "25% EFH Hotspot", df$efh)
df$lvl = "EFH level 1"

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

efh_area = data.frame(efh = c("Above 95% EFH", 
                              "Above 75% Principal EFH", 
                              "Above 50% Core EFH", 
                              "Above 25% EFH Hotspot",
                              "Below threashold"))

for (i in 1:length(unique(df$island_group))) {
  
  # i = 2
  
  efh95 = df %>% 
    filter(island_group == unique(df$island_group)[i]) %>% 
    filter(efh %in% c("95% EFH", "75% Principal EFH", "50% Core EFH", "25% EFH Hotspot")) %>% 
    ungroup() %>% 
    summarise(total_area = round(sum(km2), 2)) 
  
  efh75 = df %>% 
    filter(island_group == unique(df$island_group)[i]) %>% 
    filter(efh %in% c("75% Principal EFH", "50% Core EFH", "25% EFH Hotspot")) %>% 
    ungroup() %>% 
    summarise(total_area = round(sum(km2), 2)) 
  
  efh50 = df %>% 
    filter(island_group == unique(df$island_group)[i]) %>% 
    filter(efh %in% c("50% Core EFH", "25% EFH Hotspot")) %>% 
    ungroup() %>% 
    summarise(total_area = round(sum(km2), 2)) 
  
  efh25 = df %>% 
    filter(island_group == unique(df$island_group)[i]) %>% 
    filter(efh %in% c("25% EFH Hotspot")) %>% 
    ungroup() %>% 
    summarise(total_area = round(sum(km2), 2)) 
  
  efhno = df %>% 
    filter(island_group == unique(df$island_group)[i]) %>% 
    filter(efh %in% c("below threasholds")) %>% 
    ungroup() %>% 
    summarise(total_area = round(sum(km2), 2)) 
  
  efh = rbind(efh95, efh75, efh50, efh25, efhno)
  colnames(efh) = unique(df$island_group)[i]
  
  efh_area = cbind(efh_area, efh)
  
}

efh_area

