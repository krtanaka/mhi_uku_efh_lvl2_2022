library(raster)
library(dplyr)
library(patchwork)
library(sf)
library(ggplot2)
library(raster)
library(rmapshaper)
library(colorRamps)

rm(list = ls())

islands = c("Kauai", #1
            "Lehua", #2
            "Niihau", #3
            "Kaula", #4
            "Oahu", #5
            "Molokai", #6
            "Maui", #7
            "Lanai", #8
            "Molokini", #9
            "Kahoolawe", #10
            "Hawaii")

# Franklin 2021 Uku EFH lvl1 output
d1 = raster("/Users/kisei/Desktop/efh_sh_dp.tif") %>% 
  rasterToPoints() %>% 
  as.data.frame() %>%
  mutate(lon = round(x, 2),
         lat = round(y, 2),
         est = efh_sh_dp) %>%
  group_by(lon, lat) %>% 
  summarise(est = mean(est))

d1 %>% ggplot(aes(lon, lat, fill = est)) + geom_raster() + scale_fill_viridis_c(trans = "sqrt")

cumulative_dist_df <- raster("/Users/kisei/Desktop/efh_sh_dp.tif") %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  na.omit()

cumulative_dist <- ecdf(cumulative_dist_df$efh_sh_dp)
plot(cumulative_dist, bty = "n", xlim = c(0, 1), pch = ".")

p95_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.05, type = 7)
p75_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.25, type = 7)
p50_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.5, type = 7)
p25_l1 <- quantile(cumulative_dist_df$efh_sh_dp, 0.75, type = 7)

abline(v = p25_l1, col = 2)
abline(v = p50_l1, col = 2)
abline(v = p75_l1, col = 2)
abline(v = p95_l1, col = 2)

d1$efh = "below threasholds"
d1$efh = ifelse(d1$est >= p95_l1, "95% EFH", d1$efh)
d1$efh = ifelse(d1$est >= p75_l1, "75% Principal EFH", d1$efh)
d1$efh = ifelse(d1$est >= p50_l1, "50% Core EFH", d1$efh)
d1$efh = ifelse(d1$est >= p25_l1, "25% EFH Hotspot", d1$efh)
d1$lvl = "EFH level 1"

xlabs = seq(min(round(d1$lon)), max(round(d1$lon)), 1)
ylabs = seq(min(round(d1$lat)), max(round(d1$lat)), 1)

d1 = d1 %>% 
  as.data.frame() %>% 
  mutate(island_group = case_when(lat > 21.7 & lat < 22.35 & lon > -160.4 & lon < -159.2 ~ "Ni'ihau-Kaua'i",
                                  lat > 21.2 & lat < 21.8 & lon > -158.4 & lon < -157.4 ~ "O'ahu",
                                  lat > 20.4 & lat < 21.4 & lon > -157.9 & lon < -155.9 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                  lat > 18.8 & lat < 20.35 & lon > -156.2 & lon < -154.7 ~ "Hawai'i"))

(l1 = d1 %>% 
    # filter(efh != "below threasholds") %>%
    ggplot(aes(lon, lat, fill = efh)) + 
    geom_raster() + 
    facet_wrap(~island_group, scales = "free") + 
    scale_fill_viridis_d("", direction = -1) + 
    ggtitle("EFH level 1 threasholds") + 
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray20")))


d1a = d1 %>% subset(island_group == "Ni'ihau-Kaua'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d1a$lon)), lon2 = max(ceiling(d1a$lon)), lat1 = min(floor(d1a$lat)), lat2 = max(ceiling(d1a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(1, 3)])),]
world = st_transform(st_as_sf(world))

(p1a <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-3000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d1a, aes(lon, lat, fill = efh), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20", inherit.aes = F) +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-160.4, -159.2)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.7, 22.35)) + 
    ggtitle("Ni'ihau-Kaua'i") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d1a = d1 %>% subset(island_group == "O'ahu")
b = marmap::getNOAA.bathy(lon1 = min(floor(d1a$lon)), lon2 = max(ceiling(d1a$lon)), lat1 = min(floor(d1a$lat)), lat2 = max(ceiling(d1a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(5)])),]
world = st_transform(st_as_sf(world))

(p1b <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d1a, aes(lon, lat, fill = efh), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-158.4, -157.5)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.2, 21.8)) + 
    ggtitle("O'ahu") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d1a = d1 %>% subset(island_group == "Moloka'i-Maui-Lana'i-Kaho'olawe")
b = marmap::getNOAA.bathy(lon1 = min(floor(d1a$lon)), lon2 = max(ceiling(d1a$lon)), lat1 = min(floor(d1a$lat)), lat2 = max(ceiling(d1a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(6:8, 10)])),]
world = st_transform(st_as_sf(world))

(p1c <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = T) +
    geom_raster(data = d1a, aes(lon, lat, fill = efh), show.legend = T) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("EFH\nLevel 1", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-157.8, -155.9)) + 
    scale_y_continuous(expand = c(0,0), limits = c(20.4, 21.35)) + 
    ggtitle("Moloka'i-Maui-Lana'i-Kaho'olawe") + 
    theme(
      legend.position = "bottom",
      legend.justification = c(0, 0),
      legend.key.size = unit(0.7, "cm"),
      panel.background = element_rect(fill = "white"), # bg of the panel
      plot.background = element_rect(fill = "white"), # bg of the plot
      axis.title = element_blank(),
      axis.ticks = element_blank()))

d1a = d1 %>% subset(island_group == "Hawai'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d1a$lon)), lon2 = max(ceiling(d1a$lon)), lat1 = min(floor(d1a$lat)), lat2 = max(ceiling(d1a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(11)])),]
world = st_transform(st_as_sf(world))

(p1d <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d1a, aes(lon, lat, fill = efh), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-156.2, -154.7)) + 
    scale_y_continuous(expand = c(0,0), limits = c(18.8, 20.35)) + 
    ggtitle("Hawai'i") + 
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

(p1 = (p1a + p1b) / (p1c + p1d))

# Tanaka et al. 2022 Uku EFH lvl2 output
d2 = readRDS(file = "outputs/predictions_dynamic_All_500.rds")
d2= d2$data %>% 
  mutate(est = exp(est)) %>% 
  group_by(lon, lat) %>%
  summarise(est = mean(est))

d2 %>% ggplot(aes(lon, lat, fill = est)) + geom_raster() + scale_fill_viridis_c(trans = "sqrt")

cumulative_dist <- ecdf(d2$est)
plot(cumulative_dist, bty = "n", xlim = c(0, 1), pch = ".")

p95_l2 <- quantile(d2$est, 0.05, type = 7)
p75_l2 <- quantile(d2$est, 0.25, type = 7)
p50_l2 <- quantile(d2$est, 0.5, type = 7)
p25_l2 <- quantile(d2$est, 0.75, type = 7)

abline(v = p25_l2, col = 2)
abline(v = p50_l2, col = 2)
abline(v = p75_l2, col = 2)
abline(v = p95_l2, col = 2)

d2$efh = "below threasholds"
d2$efh = ifelse(d2$est >= p95_l2, "95% EFH", d2$efh)
d2$efh = ifelse(d2$est >= p75_l2, "75% Principal EFH", d2$efh)
d2$efh = ifelse(d2$est >= p50_l2, "50% Core EFH", d2$efh)
d2$efh = ifelse(d2$est >= p25_l2, "25% EFH Hotspot", d2$efh)
d2$lvl = "EFH level 2"

d2 = d2 %>% 
  as.data.frame() %>% 
  mutate(island_group = case_when(lat > 21.7 & lat < 22.3 & lon > -160.4 & lon < -159.2 ~ "Ni'ihau-Kaua'i",
                                  lat > 21.2 & lat < 21.8 & lon > -158.4 & lon < -157.4 ~ "O'ahu",
                                  lat > 20.4 & lat < 21.4 & lon > -157.9 & lon < -155.9 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                  lat > 18.5 & lat < 20.5 & lon > -156.2 & lon < -154.5 ~ "Hawai'i"))

(l2 = d2 %>% 
    # filter(efh != "below threasholds") %>% 
    ggplot(aes(lon, lat, fill = efh)) + 
    geom_raster() + 
    facet_wrap(~island_group, scales = "free") + 
    scale_fill_viridis_d("", direction = -1) + 
    ggtitle("EFH level 2 threasholds") + 
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray20")))

d2a = d2 %>% subset(island_group == "Ni'ihau-Kaua'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d2a$lon)), lon2 = max(ceiling(d2a$lon)), lat1 = min(floor(d2a$lat)), lat2 = max(ceiling(d2a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(1, 3)])),]
world = st_transform(st_as_sf(world))

(p1a <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d2a, aes(lon, lat, fill = efh), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-160.4, -159.2)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.7, 22.35)) + 
    ggtitle("Ni'ihau-Kaua'i") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d2a = d2 %>% subset(island_group == "O'ahu")
b = marmap::getNOAA.bathy(lon1 = min(floor(d2a$lon)), lon2 = max(ceiling(d2a$lon)), lat1 = min(floor(d2a$lat)), lat2 = max(ceiling(d2a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(5)])),]
world = st_transform(st_as_sf(world))

(p1b <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d2a, aes(lon, lat, fill = efh), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-158.4, -157.5)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.2, 21.8)) + 
    ggtitle("O'ahu") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d2a = d2 %>% subset(island_group == "Moloka'i-Maui-Lana'i-Kaho'olawe")
b = marmap::getNOAA.bathy(lon1 = min(floor(d2a$lon)), lon2 = max(ceiling(d2a$lon)), lat1 = min(floor(d2a$lat)), lat2 = max(ceiling(d2a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(6:8, 10)])),]
world = st_transform(st_as_sf(world))

(p1c <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = T) +
    geom_raster(data = d2a, aes(lon, lat, fill = efh), show.legend = T) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("EFH\nLevel 2", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-157.8, -155.9)) + 
    scale_y_continuous(expand = c(0,0), limits = c(20.4, 21.35)) + 
    ggtitle("Moloka'i-Maui-Lana'i-Kaho'olawe") + 
    theme(legend.position = "bottom",
          legend.justification = c(0, 0),
          panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          legend.key.size = unit(0.7, "cm"),
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d2a = d2 %>% subset(island_group == "Hawai'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d2a$lon)), lon2 = max(ceiling(d2a$lon)), lat1 = min(floor(d2a$lat)), lat2 = max(ceiling(d2a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(11)])),]
world = st_transform(st_as_sf(world))

(p1d <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d2a, aes(lon, lat, fill = efh), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("EFH\nLevel 2", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-156.2, -154.7)) + 
    scale_y_continuous(expand = c(0,0), limits = c(18.8, 20.35)) + 
    ggtitle("Hawai'i") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

(p2 = (p1a + p1b) / (p1c + p1d) )

# 95% overlap
p95_l1_df = d1 %>% subset(est >= p95_l1) %>% select(lon, lat, lvl) %>% mutate(cell_count = 1)
p95_l2_df = d2 %>% subset(est >= p95_l2) %>% select(lon, lat, lvl) %>% mutate(cell_count = 1)

rbind(p95_l1_df, p95_l2_df) %>% 
  group_by(lon, lat) %>% 
  summarise(cell_count = sum(cell_count)) %>% 
  subset(cell_count > 1) %>%  
  ggplot(aes(lon, lat, fill = "cell_count")) + 
  geom_raster()

d3 = rbind(p95_l1_df, p95_l2_df) %>% 
  as.data.frame() %>% 
  mutate(island_group = case_when(lat > 21.7 & lat < 22.3 & lon > -160.4 & lon < -159.2 ~ "Ni'ihau-Kaua'i",
                                  lat > 21.2 & lat < 21.8 & lon > -158.4 & lon < -157.4 ~ "O'ahu",
                                  lat > 20.4 & lat < 21.4 & lon > -157.9 & lon < -155.9 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                  lat > 18.5 & lat < 20.5 & lon > -156.2 & lon < -154.5 ~ "Hawai'i"))

d3a = d3 %>% subset(island_group == "Ni'ihau-Kaua'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(1, 3)])),]
world = st_transform(st_as_sf(world))

(p1a <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-160.4, -159.2)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.7, 22.35)) + 
    ggtitle("Ni'ihau-Kaua'i, 95% EFH") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d3a = d3 %>% subset(island_group == "O'ahu")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(5)])),]
world = st_transform(st_as_sf(world))

(p1b <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-158.4, -157.5)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.2, 21.8)) + 
    ggtitle("O'ahu, 95% EFH") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d3a = d3 %>% subset(island_group == "Moloka'i-Maui-Lana'i-Kaho'olawe")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(6:8, 10)])),]
world = st_transform(st_as_sf(world))

(p1c <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = T) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = T) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-157.8, -155.9)) + 
    scale_y_continuous(expand = c(0,0), limits = c(20.4, 21.35)) + 
    ggtitle("Moloka'i-Maui-Lana'i-Kaho'olawe, 95% EFH") + 
    theme(legend.position = "bottom",
          legend.justification = c(0, 0),
          panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d3a = d3 %>% subset(island_group == "Hawai'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(11)])),]
world = st_transform(st_as_sf(world))

(p1d <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-156.2, -154.7)) + 
    scale_y_continuous(expand = c(0,0), limits = c(18.8, 20.35)) + 
    ggtitle("Hawai'i, 95% EFH") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

(p3 = (p1a + p1b) / (p1c + p1d) )

# 50% overlap
p50_l1_df = d1 %>% subset(est >= p50_l1) %>% select(lon, lat, lvl) %>% mutate(cell_count = 1)
p50_l2_df = d2 %>% subset(est >= p50_l2) %>% select(lon, lat, lvl) %>% mutate(cell_count = 1)

rbind(p50_l1_df, p50_l2_df) %>% 
  group_by(lon, lat) %>% 
  summarise(cell_count = sum(cell_count)) %>% 
  subset(cell_count > 1) %>%  
  ggplot(aes(lon, lat, fill = "cell_count")) + 
  geom_raster()

d3 = rbind(p50_l1_df, p50_l2_df) %>% 
  as.data.frame() %>% 
  mutate(island_group = case_when(lat > 21.7 & lat < 22.3 & lon > -160.4 & lon < -159.2 ~ "Ni'ihau-Kaua'i",
                                  lat > 21.2 & lat < 21.8 & lon > -158.4 & lon < -157.4 ~ "O'ahu",
                                  lat > 20.4 & lat < 21.4 & lon > -157.9 & lon < -155.9 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                  lat > 18.5 & lat < 20.5 & lon > -156.2 & lon < -154.5 ~ "Hawai'i"))

d3a = d3 %>% subset(island_group == "Ni'ihau-Kaua'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(1, 3)])),]
world = st_transform(st_as_sf(world))

(p1a <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-160.4, -159.2)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.7, 22.35)) + 
    ggtitle("Ni'ihau-Kaua'i, 50% EFH") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d3a = d3 %>% subset(island_group == "O'ahu")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(5)])),]
world = st_transform(st_as_sf(world))

(p1b <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-158.4, -157.5)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.2, 21.8)) + 
    ggtitle("O'ahu, 50% EFH") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d3a = d3 %>% subset(island_group == "Moloka'i-Maui-Lana'i-Kaho'olawe")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(6:8, 10)])),]
world = st_transform(st_as_sf(world))

(p1c <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = T) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = T) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-157.8, -155.9)) + 
    scale_y_continuous(expand = c(0,0), limits = c(20.4, 21.35)) + 
    ggtitle("Moloka'i-Maui-Lana'i-Kaho'olawe, 50% EFH") + 
    theme(legend.position = "bottom",
          legend.justification = c(0, 0),
          panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d3a = d3 %>% subset(island_group == "Hawai'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(11)])),]
world = st_transform(st_as_sf(world))

(p1d <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-156.2, -154.7)) + 
    scale_y_continuous(expand = c(0,0), limits = c(18.8, 20.35)) + 
    ggtitle("Hawai'i, 50% EFH") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

(p4 = (p1a + p1b) / (p1c + p1d) )

# 25% overlap
p25_l1_df = d1 %>% subset(est >= p25_l1) %>% select(lon, lat, lvl) %>% mutate(cell_count = 1)
p25_l2_df = d2 %>% subset(est >= p25_l2) %>% select(lon, lat, lvl) %>% mutate(cell_count = 1)

rbind(p25_l1_df, p25_l2_df) %>% 
  group_by(lon, lat) %>% 
  summarise(cell_count = sum(cell_count)) %>% 
  subset(cell_count > 1) %>%  
  ggplot(aes(lon, lat, fill = "cell_count")) + 
  geom_raster()

d3 = rbind(p25_l1_df, p25_l2_df) %>% 
  as.data.frame() %>% 
  mutate(island_group = case_when(lat > 21.7 & lat < 22.3 & lon > -160.4 & lon < -159.2 ~ "Ni'ihau-Kaua'i",
                                  lat > 21.2 & lat < 21.8 & lon > -158.4 & lon < -157.4 ~ "O'ahu",
                                  lat > 20.4 & lat < 21.4 & lon > -157.9 & lon < -155.9 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                  lat > 18.5 & lat < 20.5 & lon > -156.2 & lon < -154.5 ~ "Hawai'i"))

d3a = d3 %>% subset(island_group == "Ni'ihau-Kaua'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(1, 3)])),]
world = st_transform(st_as_sf(world))

(p1a <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-160.4, -159.2)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.7, 22.35)) + 
    ggtitle("Ni'ihau-Kaua'i, 25% EFH") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d3a = d3 %>% subset(island_group == "O'ahu")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(5)])),]
world = st_transform(st_as_sf(world))

(p1b <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-158.4, -157.5)) + 
    scale_y_continuous(expand = c(0,0), limits = c(21.2, 21.8)) + 
    ggtitle("O'ahu, 25% EFH") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d3a = d3 %>% subset(island_group == "Moloka'i-Maui-Lana'i-Kaho'olawe")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(6:8, 10)])),]
world = st_transform(st_as_sf(world))

(p1c <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = T) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = T) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d("", direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-157.8, -155.9)) + 
    scale_y_continuous(expand = c(0,0), limits = c(20.4, 21.35)) + 
    ggtitle("Moloka'i-Maui-Lana'i-Kaho'olawe, 25% EFH") + 
    theme(legend.position = "bottom",
          legend.justification = c(0, 0),
          panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

d3a = d3 %>% subset(island_group == "Hawai'i")
b = marmap::getNOAA.bathy(lon1 = min(floor(d3a$lon)), lon2 = max(ceiling(d3a$lon)), lat1 = min(floor(d3a$lat)), lat2 = max(ceiling(d3a$lat)), resolution = 1)
b = marmap::fortify.bathy(b)

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[c(11)])),]
world = st_transform(st_as_sf(world))

(p1d <- ggplot() +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-1000, 0, by = 250),
                 size = c(0.1),
                 show.legend = F) +
    geom_raster(data = d3a, aes(lon, lat, fill = lvl), show.legend = F) + 
    geom_sf(data = world, fill = "gray", color = "gray20") +
    scale_colour_continuous("Depth (m)") +
    scale_fill_viridis_d(direction = -1) +
    theme_light() +
    scale_x_continuous(expand = c(0,0), limits = c(-156.2, -154.7)) + 
    scale_y_continuous(expand = c(0,0), limits = c(18.8, 20.35)) + 
    ggtitle("Hawai'i, 25% EFH") + 
    theme(panel.background = element_rect(fill = "white"), # bg of the panel
          plot.background = element_rect(fill = "white"), # bg of the plot
          axis.title = element_blank(),
          axis.ticks = element_blank()))

(p5 = (p1a + p1b) / (p1c + p1d) )

png("outputs/efh_1.png", units = "in", height = 12, width = 15, res = 500)
p1
dev.off()

png("outputs/efh_2.png", units = "in", height = 12, width = 15, res = 500)
p2
dev.off()

png("outputs/efh_3.png", units = "in", height = 12, width = 15, res = 500)
p3
dev.off()

png("outputs/efh_4.png", units = "in", height = 12, width = 15, res = 500)
p4
dev.off()

png("outputs/efh_5.png", units = "in", height = 12, width = 15, res = 500)
p5
dev.off()

save(d1, file = "outputs/uku_efh_l1.Rdata")
save(d2, file = "outputs/uku_efh_l2.Rdata")
