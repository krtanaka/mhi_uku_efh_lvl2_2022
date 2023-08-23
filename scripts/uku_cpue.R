rm(list = ls())

library(raster)
library(tibble)
library(ggplot2)
library(ggrepel)
library(colorRamps)
library(dplyr)
library(tidyverse)

# From Marc Nadon Uku_EFH repo
load("data/Final.b.Rdata")

r <- raster(ncol=1440, nrow=720)
r <- raster(ncol = 720, nrow = 360)
# r <- raster(ncol = 180, nrow = 90)
crs(r) <- "+proj=longlat +datum=WGS84"
extent(r) <- extent(Final.b)

# convert sf object to raster
uku_cpue <- rasterize(Final.b %>% subset(SEASON == "Winter"), r, field = "CPUE.STD") %>% rasterToPoints() %>% as.data.frame()
uku_cpue <- rasterize(Final.b %>% subset(SEASON == "Summer"), r, field = "CPUE.STD") %>% rasterToPoints() %>% as.data.frame()
uku_cpue <- rasterize(Final.b, r, field = "CPUE.STD") %>% rasterToPoints() %>% as.data.frame()

colnames(uku_cpue) = c("lon", "lat", "cpue_std")

# add percentile column
uku_cpue <- uku_cpue %>% 
  mutate(percentile = percent_rank(cpue_std))

quantiles <- uku_cpue %>% 
  summarize(q10 = quantile(cpue_std, 0.05),
            q50 = quantile(cpue_std, 0.25),
            q75 = quantile(cpue_std, 0.5),
            q90 = quantile(cpue_std, 0.75)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "quantile") %>% 
  rename(cpue_std = V1)

uku_cpue$efh = "below threasholds"
uku_cpue$efh = ifelse(uku_cpue$cpue_std >= quantiles[1,2], "95% EFH", uku_cpue$efh)
uku_cpue$efh = ifelse(uku_cpue$cpue_std >= quantiles[2,2], "75% Principal EFH", uku_cpue$efh)
uku_cpue$efh = ifelse(uku_cpue$cpue_std >= quantiles[3,2], "50% Core EFH", uku_cpue$efh)
uku_cpue$efh = ifelse(uku_cpue$cpue_std >= quantiles[4,2], "25% EFH Hotspot", uku_cpue$efh)

uku_cpue = left_join(uku_cpue, quantiles)

quantile_label = uku_cpue %>% 
  na.omit() %>% 
  group_by(quantile) %>% 
  summarise(lat = median(lat),
            lon = median(lon))

(uku_cpue %>% 
    # group_by(lon, lat) %>% 
    # summarise(cpue_std = mean(cpue_std)) %>% 
    ggplot(aes(lon, lat, fill = cpue_std)) + 
    geom_raster() + 
    scale_fill_gradientn(colours = matlab.like(100)) + 
    labs(x = "Longitude", y = "Latitude") + 
    coord_fixed() + 
    theme(legend.position = c(0.1, 0.2),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA)))

ggsave(last_plot(),filename =  "outputs/cpue_std.png", units = "in", height = 6, width = 8)

(uku_cpue %>% 
    ggplot(aes(lon, lat, fill = efh)) + 
    geom_raster() + 
    scale_fill_viridis_d("", direction = -1) + 
    labs(x = "Longitude", y = "Latitude") + 
    coord_fixed() + 
    theme(legend.position = c(0.1, 0.2),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA)))

ggsave(last_plot(),filename =  "outputs/cpue_efh.png", units = "in", height = 6, width = 8)

(uku_cpue %>% 
    ggplot(aes(lon, lat, fill = percentile)) + 
    geom_raster() + 
    scale_fill_gradientn(colours = matlab.like(100), "Percentile") + 
    labs(x = "Longitude", y = "Latitude") + 
    coord_fixed() + 
    theme(legend.position = c(0.1, 0.2),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA)))

ggsave(last_plot(),filename =  "outputs/cpue_pct.png", units = "in", height = 6, width = 8)

save(uku_cpue, file = "outputs/uku_cpue.RData")
