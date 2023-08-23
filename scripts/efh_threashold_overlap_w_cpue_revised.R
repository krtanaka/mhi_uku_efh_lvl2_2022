library(dplyr)
library(ggplot2)
library(colorRamps)
library(raster)
library(sf)
library(patchwork)
library(tidyverse)

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

select = dplyr::select

df_all = NULL

for (l in 1:3) {
  
  # l = 1
  
  if (l == 1) {
    
    # load("outputs/uku_efh_l1.Rdata")
    # df = d1 %>%
    #   mutate(percentile = percent_rank(est)) %>%
    #   select(lon, lat, lvl, percentile)
    
    df = raster("/Users/kisei.tanaka/Desktop/efh_sh_dp.tif") %>% 
      rasterToPoints() %>% 
      as.data.frame() %>%
      mutate(percentile = percent_rank(efh_sh_dp),
             lvl = "EFH level 1") %>% 
      mutate(lon = round(x, 2),
             lat = round(y, 2)) %>%
      group_by(lon, lat, lvl) %>% 
      summarise(percentile = mean(percentile))
    
  } 
  
  if (l == 2) {
    
    # load("outputs/uku_efh_l2.Rdata")
    # df = d2 %>%
    #   mutate(percentile = percent_rank(est)) %>% 
    #   select(lon, lat, lvl, percentile)
    
    # Tanaka et al. 2022 Uku EFH lvl2 output
    df = readRDS(file = "outputs/predictions_dynamic_All_500.rds") 
    df = df$data %>% 
      mutate(est = exp(est)) %>% 
      mutate(percentile = percent_rank(est),
             lvl = "EFH level 2") %>% 
      group_by(lon, lat, lvl) %>% 
      summarise(percentile = mean(percentile))
    
  }
  
  if (l == 3) {
    
    # load("outputs/uku_cpue.RData")
    # df = uku_cpue  %>% 
    #   mutate(lvl = "cpue") %>% 
    #   select(lon, lat, lvl, percentile)
    
    # From Marc Nadon Uku_EFH repo
    load("data/Final.b.Rdata")
    
    r <- raster(ncol = 1440, nrow = 720)
    # r <- raster(ncol = 720, nrow = 360)
    # r <- raster(ncol = 180, nrow = 90)
    crs(r) <- "+proj=longlat +datum=WGS84"
    extent(r) <- extent(Final.b)
    
    # convert sf object to raster
    # uku_cpue <- rasterize(Final.b %>% subset(SEASON == "Winter"), r, field = "CPUE.STD") %>% rasterToPoints() %>% as.data.frame()
    # uku_cpue <- rasterize(Final.b %>% subset(SEASON == "Summer"), r, field = "CPUE.STD") %>% rasterToPoints() %>% as.data.frame()
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

    df = uku_cpue  %>%
      mutate(lvl = "cpue") %>%
      select(lon, lat, lvl, percentile)
    
  }
  
  df = df %>% 
    mutate(island_group = case_when(lat > 21.7 & lat < 22.35 & lon > -160.4 & lon < -159.2 ~ "Ni'ihau-Kaua'i",
                                    lat > 21.2 & lat < 21.8 & lon > -158.4 & lon < -157.4 ~ "O'ahu",
                                    lat > 20.4 & lat < 21.4 & lon > -157.9 & lon < -155.9 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                    lat > 18.8 & lat < 20.35 & lon > -156.2 & lon < -154.7 ~ "Hawai'i")) %>% 
    na.omit()
  
  df %>% ggplot(aes(lon, lat, fill = percentile)) + geom_raster() + scale_fill_viridis_c("", limits = c(0,1))
  
  plot_list <- list()
  
  isl_groups = unique(df$island_group)
  
  for (i in 1:length(isl_groups)) {
    
    # i = 1
    
    d_i = df %>% filter(island_group == isl_groups[i])
    
    b = marmap::getNOAA.bathy(lon1 = min(floor(d_i$lon)), 
                              lon2 = max(ceiling(d_i$lon)), 
                              lat1 = min(floor(d_i$lat)), 
                              lat2 = max(ceiling(d_i$lat)), 
                              resolution = 1) %>% 
      marmap::fortify.bathy()
    
    if (i == 1) {lon_range = c(-160.4, -159.2); lat_range = c(21.7, 22.35)}
    if (i == 2) {lon_range = c(-158.4, -157.4); lat_range = c(21.2, 21.8)}
    if (i == 3) {lon_range = c(-157.9, -155.9); lat_range = c(20.4, 21.4)}
    if (i == 4) {lon_range = c(-156.2, -154.7); lat_range = c(18.8, 20.35)}
    
    if (i == 3) {
      
      p_i <- ggplot() +
        geom_raster(data = d_i, aes(lon, lat, fill = percentile), show.legend = T) +
        # geom_contour(data = b,
        #              aes(x = x, y = y, z = z, colour = stat(level)),
        #              breaks = seq(-240.5, -239.5, by = 1),
        #              col = "gray20",
        #              linetype = 2,
        #              size = c(1),
        #              show.legend = F) +
        # scale_colour_continuous("Depth (m)") +
        scale_fill_gradientn(colours = matlab.like(100), "Pctl.", limits = c(0, 1)) +
        scale_x_continuous(expand = c(0,0), limits = lon_range) +
        scale_y_continuous(expand = c(0,0), limits = lat_range) +
        ggtitle(isl_groups[i]) + 
        theme(legend.position = "bottom")
      
    } else {
      
      p_i <- ggplot() +
        geom_raster(data = d_i, aes(lon, lat, fill = percentile), show.legend = F) +
        # geom_contour(data = b,
        #              aes(x = x, y = y, z = z, colour = stat(level)),
        #              breaks = seq(-250.5, -249.5, by = 1),
        #              col = "gray40",
        #              linetype = 2,
        #              size = c(1),
        #              show.legend = F) +
        # scale_colour_continuous("Depth (m)") +
        scale_fill_gradientn(colours = matlab.like(100), "", limits = c(0, 1)) + 
        scale_x_continuous(expand = c(0,0), limits = lon_range) +
        scale_y_continuous(expand = c(0,0), limits = lat_range) +
        ggtitle(isl_groups[i])
      
    }
    
    plot_list[[i]] <- p_i
    
  }
  
  (plot_list[[1]] + plot_list[[2]]) / (plot_list[[3]] + plot_list[[4]])
  
  if (l == 1) ggsave(last_plot(), filename = "outputs/percentile_efh_lvl_1.png", height = 8, width = 11)
  if (l == 2) ggsave(last_plot(), filename = "outputs/percentile_efh_lvl_2.png", height = 8, width = 11)
  if (l == 3) ggsave(last_plot(), filename = "outputs/percentile_cpue.png", height = 8, width = 11)
  
  df_all = rbind(df, df_all)
  
}

# map mean percentile at 0.01 dec deg resolution
for (i in 1:length(isl_groups)) {
  
  # i = 1
  
  # aggregate to 0.01 dec deg resolution
  d_i = df_all %>% 
    filter(island_group == isl_groups[i]) %>% 
    mutate(lon = round(lon, 2),
           lat = round(lat, 2)) %>%
    group_by(lon, lat, island_group) %>%
    summarise(num_levels = n_distinct(lvl),
              percentile = mean(percentile, na.rm = T)) %>% 
    filter(num_levels > 1)
  
  # b = marmap::getNOAA.bathy(lon1 = min(floor(d_i$lon)), 
  #                           lon2 = max(ceiling(d_i$lon)), 
  #                           lat1 = min(floor(d_i$lat)), 
  #                           lat2 = max(ceiling(d_i$lat)), 
  #                           resolution = 1) %>% 
  #   marmap::fortify.bathy()
  
  if (i == 1) {lon_range = c(-160.4, -159.2); lat_range = c(21.7, 22.35)}
  if (i == 2) {lon_range = c(-158.4, -157.4); lat_range = c(21.2, 21.8)}
  if (i == 3) {lon_range = c(-157.9, -155.9); lat_range = c(20.4, 21.4)}
  if (i == 4) {lon_range = c(-156.2, -154.7); lat_range = c(18.8, 20.35)}
  
  if (i == 3) {
    
    p_i <- ggplot() +
      geom_raster(data = d_i, aes(lon, lat, fill = percentile), show.legend = T) +
      # geom_contour(data = b,
      #              aes(x = x, y = y, z = z, colour = stat(level)),
      #              breaks = seq(-250.5, -249.5, by = 1),
      #              col = "gray40",
      #              linetype = 2,
      #              size = c(1),
      #              show.legend = F) +
      # scale_colour_continuous("Depth (m)") +
      scale_fill_gradientn(colours = matlab.like(100), "Pctl.", limits = c(0, 1)) + 
      scale_x_continuous(expand = c(0,0), limits = lon_range) +
      scale_y_continuous(expand = c(0,0), limits = lat_range) +
      ggtitle(isl_groups[i]) + 
      theme(legend.position = "bottom")
    
  } else {
    
    p_i <- ggplot() +
      geom_raster(data = d_i, aes(lon, lat, fill = percentile), show.legend = F) +
      # geom_contour(data = b,
      #              aes(x = x, y = y, z = z, colour = stat(level)),
      #              breaks = seq(-250.5, -249.5, by = 1),
      #              col = "gray40",
      #              linetype = 2,
      #              size = c(1),
      #              show.legend = F) +
      # scale_colour_continuous("Depth (m)") +
      scale_fill_gradientn(colours = matlab.like(100), "", limits = c(0, 1)) + 
      # theme_light() +
      scale_x_continuous(expand = c(0,0), limits = lon_range) +
      scale_y_continuous(expand = c(0,0), limits = lat_range) +
      ggtitle(isl_groups[i])
    
    
  }
  
  plot_list[[i]] <- p_i
  
}

(plot_list[[1]] + plot_list[[2]]) / (plot_list[[3]] + plot_list[[4]])

ggsave(last_plot(), filename = "outputs/percentile_cpue_efh.png", height = 8, width = 11)

options = c("Op2", "Op5")

efh_area_option = NULL

for (o in 1:length(options)) {
  
  # o = 1
  
  if (o == 1) {
    
    df_i = df_all %>% 
      filter(lvl == "EFH level 1") %>% 
      mutate(lon = round(lon, 2),
             lat = round(lat, 2)) %>%
      group_by(lon, lat, island_group) %>%
      summarise(percentile = mean(percentile, na.rm = T))
    
  }
  
  if (o == 2) {
    
    df_i = df_all %>% 
      mutate(lon = round(lon, 2),
             lat = round(lat, 2)) %>%
      group_by(lon, lat) %>%
      mutate(num_levels = n_distinct(lvl),
             percentile_ave = mean(percentile, na.rm = T)) %>% 
      filter(lvl != "cpue") %>% 
      group_by(lon, lat, island_group) %>%
      summarise(percentile = mean(percentile_ave, na.rm = T))
    
  }
  
  df_i$efh = "below threasholds"
  df_i$efh = ifelse(df_i$percentile >= 0.05, "95% EFH", df_i$efh)
  df_i$efh = ifelse(df_i$percentile >= 0.25, "75% Principal EFH", df_i$efh)
  df_i$efh = ifelse(df_i$percentile >= 0.5, "50% Core EFH", df_i$efh)
  df_i$efh = ifelse(df_i$percentile >= 0.75, "25% EFH Hotspot", df_i$efh)
  
  df_i$island_group <- factor(df_i$island_group, levels = c("Ni'ihau-Kaua'i", 
                                                            "O'ahu", 
                                                            "Moloka'i-Maui-Lana'i-Kaho'olawe", 
                                                            "Hawai'i"))
  
  df_i = df_i %>% mutate(lat_g = as.character(lat))
  
  df_i = left_join(df_i, grid_sizes[,c("lat_g", "km2")])
  
  efh_area = data.frame(efh = c("Above 95% EFH", 
                                "Above 75% Principal EFH", 
                                "Above 50% Core EFH", 
                                "Above 25% EFH Hotspot",
                                "Below threashold"))
  
  p1 = df_i %>% 
    ggplot(aes(lon, lat, fill = percentile)) + 
    geom_raster() + 
    facet_wrap(~island_group, scales = "free") +  
    scale_fill_gradientn(colours = matlab.like(100),
                         limits = c(0,1), "")
  
  p2 = df_i %>% 
    ggplot(aes(lon, lat, fill = efh)) + 
    geom_raster() + 
    facet_wrap(~island_group, scales = "free") +
    scale_fill_viridis_d("", direction = -1)
  
  p1 + p2
  
  if (o == 1) ggsave(last_plot(), filename = "outputs/option_2_percentile_efh.png", height = 8, width = 22)
  if (o == 2) ggsave(last_plot(), filename = "outputs/option_5_percentile_efh.png", height = 8, width = 22)

  for (i in 1:length(unique(df_i$island_group))) {
    
    # i = 2
    
    efh95 = df_i %>% 
      filter(island_group == unique(df_i$island_group)[i]) %>% 
      filter(efh %in% c("95% EFH", "75% Principal EFH", "50% Core EFH", "25% EFH Hotspot")) %>% 
      ungroup() %>% 
      summarise(total_area = round(sum(km2), 2)) 
    
    efh75 = df_i %>% 
      filter(island_group == unique(df_i$island_group)[i]) %>% 
      filter(efh %in% c("75% Principal EFH", "50% Core EFH", "25% EFH Hotspot")) %>% 
      ungroup() %>% 
      summarise(total_area = round(sum(km2), 2)) 
    
    efh50 = df_i %>% 
      filter(island_group == unique(df_i$island_group)[i]) %>% 
      filter(efh %in% c("50% Core EFH", "25% EFH Hotspot")) %>% 
      ungroup() %>% 
      summarise(total_area = round(sum(km2), 2)) 
    
    efh25 = df_i %>% 
      filter(island_group == unique(df_i$island_group)[i]) %>% 
      filter(efh %in% c("25% EFH Hotspot")) %>% 
      ungroup() %>% 
      summarise(total_area = round(sum(km2), 2)) 
    
    efhno = df_i %>% 
      filter(island_group == unique(df_i$island_group)[i]) %>% 
      filter(efh %in% c("below threasholds")) %>% 
      ungroup() %>% 
      summarise(total_area = round(sum(km2), 2)) 
    
    efh = rbind(efh95, efh75, efh50, efh25, efhno)
    colnames(efh) = unique(df_i$island_group)[i]
    
    efh_area = cbind(efh_area, efh)
    
  }
  
  efh_area$Option = options[o]
  
  efh_area_option = rbind(efh_area_option, efh_area)
  
}

readr::write_csv(efh_area_option, "outputs/total_efh_area_km2.csv")



df = df %>% 
  filter(num_levels > 1) %>%
  filter(efh != "below threasholds") %>% 
  filter(island_group == "Moloka'i-Maui-Lana'i-Kaho'olawe")

df = df_all %>% 
  mutate(lon = round(lon, 2),
         lat = round(lat, 2)) %>%
  group_by(lon, lat) %>%
  summarise(num_levels = n_distinct(lvl),
            percentile = mean(percentile, na.rm = T)) %>% 
  mutate(island_group = case_when(lat > 21.7 & lat < 22.35 & lon > -160.4 & lon < -159.2 ~ "Ni'ihau-Kaua'i",
                                  lat > 21.2 & lat < 21.8 & lon > -158.4 & lon < -157.4 ~ "O'ahu",
                                  lat > 20.4 & lat < 21.4 & lon > -157.9 & lon < -155.9 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                  lat > 18.8 & lat < 20.35 & lon > -156.2 & lon < -154.7 ~ "Hawai'i")) %>% 
  na.omit()

df$efh = "below threasholds"
df$efh = ifelse(df$percentile >= 0.05, "95% EFH", df$efh)
df$efh = ifelse(df$percentile >= 0.25, "75% Principal EFH", df$efh)
df$efh = ifelse(df$percentile >= 0.5, "50% Core EFH", df$efh)
df$efh = ifelse(df$percentile >= 0.75, "25% EFH Hotspot", df$efh)

df$island_group <- factor(df$island_group, levels = c("Ni'ihau-Kaua'i", 
                                                      "O'ahu", 
                                                      "Moloka'i-Maui-Lana'i-Kaho'olawe", 
                                                      "Hawai'i"))

df = df %>% 
  filter(efh != "below threasholds")

df %>% 
  filter(num_levels > 1) %>% 
  ggplot(aes(lon, lat, fill = percentile)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = matlab.like(100), "Pctl.") + 
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("Mean Percentile from EFH levels 1 & 2 and Commercial CPUE") + 
  facet_wrap(~island_group, scales = "free") + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.justification = c(0, 0),
        panel.background = element_rect(fill = "white"), # bg of the panel
        plot.background = element_rect(fill = "white"), # bg of the plot
        legend.key.size = unit(0.7, "cm"),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave(last_plot(), filename = "outputs/percentile_efh_cpue_mean.png", height = 8, width = 10)


df %>% 
  filter(num_levels > 1) %>% 
  ggplot(aes(lon, lat, fill = efh)) + 
  geom_raster() + 
  scale_fill_viridis_d("", direction = -1) + 
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("EFH Thresholds based on average percentiles from EFH levels 1 & 2, as well as Commercial CPUE") + 
  facet_wrap(~island_group, scales = "free") + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.justification = c(0, 0),
        panel.background = element_rect(fill = "white"), # bg of the panel
        plot.background = element_rect(fill = "white"), # bg of the plot
        legend.key.size = unit(0.7, "cm"),
        axis.title = element_blank(),
        axis.ticks = element_blank()) 

ggsave(last_plot(), filename = "outputs/efh.png", height = 8, width = 10)
