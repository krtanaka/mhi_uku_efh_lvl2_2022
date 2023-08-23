detach("package:plyr", unload = TRUE)
detach("package:Rmisc", unload = TRUE)

library(sdmTMB)
library(dplyr)
library(ggplot2)
library(rgdal)
library(colorRamps)
library(patchwork)
library(raster)
library(sf)
library(readr)
library(ggpubr)
library(tidyr)
library(rmapshaper)
library(ggrepel)

rm(list = ls())

select = dplyr::select

Ibbox = read.csv("data/Island_Bounding_Boxes.csv", stringsAsFactors = F) # Updated Bounding boxes 2021
islands = c( "Hawaii", "Kahoolawe", "Kauai", "Lanai", "Maui", "Molokai", "Niihau", "Oahu", "All")#[1:8]

n_knots = c(1000, 500, 300, 100)

for (k in 1:length(n_knots)) {
  
  # k = 2
  
  map = NULL
  index_a = NULL
  index_b = NULL
  cog = NULL
  
  for (i in 1:length(islands)) {
    
    # i = 1
    
    # get spatial distribution results
    map_i_static = readRDS(paste0("outputs/predictions_static_", islands[i], "_", n_knots[k], ".rds"))
    map_i_dynamic = readRDS(paste0("outputs/predictions_dynamic_", islands[i], "_", n_knots[k], ".rds"))
    map_i_static = map_i_static$data
    map_i_dynamic = map_i_dynamic$data
    map_i_static$island =  islands[i]
    map_i_dynamic$island =  islands[i]
    map_i_static$model =  "static_model"
    map_i_dynamic$model =  "dynamic_model"
    map_i = rbind(map_i_static, map_i_dynamic)
    map = rbind(map, map_i)
    
    # get first index result - total biomass
    index_i_static = readRDS(paste0("outputs/biomass_bias_corrected_static_", islands[i], "_", n_knots[k], ".rds"))
    index_i_dynamic = readRDS(paste0("outputs/biomass_bias_corrected_dynamic_", islands[i], "_", n_knots[k], ".rds"))
    index_i_static$island =  islands[i]
    index_i_dynamic$island =  islands[i]
    index_i_static$model =  "static_model"
    index_i_dynamic$model =  "dynamic_model"
    index_i_dynamic[sapply(index_i_dynamic, is.nan)] <- NA
    index_i_dynamic[sapply(index_i_dynamic, is.infinite)] <- NA
    index_i_dynamic[index_i_dynamic == 0] <- NA 
    index_i_dynamic$lwr = ifelse(is.na(index_i_dynamic$lwr), index_i_static$lwr, index_i_dynamic$lwr)
    index_i_dynamic$upr = ifelse(is.na(index_i_dynamic$upr), index_i_static$upr, index_i_dynamic$upr)
    index_diff = index_i_dynamic$est - index_i_static$est
    index_i_dynamic$lwr =  index_i_dynamic$lwr + index_diff
    index_i_dynamic$upr = index_i_dynamic$upr + index_diff
    # index_i_static$est_rel = index_i_static$est/max(index_i_static$est)
    # index_i_dynamic$est_rel = index_i_dynamic$est/max(index_i_dynamic$est)
    # index_i_static$lwr_rel = index_i_static$lwr/max(index_i_static$est)
    # index_i_dynamic$lwr_rel = index_i_dynamic$lwr/max(index_i_dynamic$est)
    # index_i_static$upr_rel = index_i_static$upr/max(index_i_static$est)
    # index_i_dynamic$upr_rel = index_i_dynamic$upr/max(index_i_dynamic$est)
    index_i_a = rbind(index_i_static, index_i_dynamic)
    
    # get second index result - mean biomass
    index_i_b = map_i %>%
      mutate(est = exp(est)) %>% 
      group_by(year, model, island) %>%
      summarise(est.mean = mean(est, na.rm = TRUE),
                sd = sd(est, na.rm = TRUE),
                n = n()) %>%
      mutate(se = sd / sqrt(n),
             lwr = est.mean - qt(1 - (0.05 / 2), n - 1) * se,
             upr = est.mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
      mutate(est = est.mean)
    
    index_a = rbind(index_a, index_i_a)
    index_b = rbind(index_b, index_i_b)
    
    # get COG results
    cog_i_static = readRDS(paste0("outputs/COG_bias_corrected_static_", islands[i], "_", n_knots[k], ".rds"))
    cog_i_dynamic = readRDS(paste0("outputs/COG_bias_corrected_dynamic_", islands[i], "_", n_knots[k], ".rds"))
    cog_i_static$island =  islands[i]
    cog_i_dynamic$island =  islands[i]
    cog_i_static$model =  "static_model"
    cog_i_dynamic$model =  "dynamic_model"
    cog_i_dynamic[sapply(cog_i_dynamic, is.nan)] <- NA
    cog_i_dynamic[sapply(cog_i_dynamic, is.infinite)] <- NA
    cog_i_dynamic[cog_i_dynamic == 0] <- NA 
    cog_i_dynamic$lwr = ifelse(is.na(cog_i_dynamic$lwr), cog_i_static$lwr, cog_i_dynamic$lwr)
    cog_i_dynamic$upr = ifelse(is.na(cog_i_dynamic$upr), cog_i_static$upr, cog_i_dynamic$upr)
    cog_i = rbind(cog_i_static, cog_i_dynamic)
    cog = rbind(cog, cog_i)
    
  }
  
  rm(cog_i, cog_i_dynamic, cog_i_static, index_i_a, index_i_b, index_i_dynamic, index_i_static, map_i, map_i_dynamic, map_i_static, index_diff)
  
  map = map %>%
    subset(island == "All") %>%
    subset(model == "dynamic_model")
  
  map = map %>% 
    mutate(island_group = case_when(lat > 21.73143 & lat < 22.29684 & lon > -160.3023 & lon < -159.2321 ~ "Ni'ihau-Kaua'i",
                                    lat > 21.19259 & lat < 21.75448 & lon > -158.3432 & lon < -157.5982 ~ "O'ahu",
                                    lat > 20.45111 & lat < 21.26553 & lon > -157.3642 & lon < -155.9242 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                    lat > 18.86465 & lat < 20.32120 & lon > -156.1109 & lon < -154.7542 ~ "Hawai'i",))
  
  map$island_group = factor(map$island_group, levels=c("Ni'ihau-Kaua'i","O'ahu","Moloka'i-Maui-Lana'i-Kaho'olawe","Hawai'i"))
  
  map %>% 
    mutate(est = exp(est)) %>% 
    group_by(island_group) %>% 
    summarise(max =  round(max(est), 2),
              mean = round(mean(est), 2),
              sd = sd(est),
              n = n(),
              se = sd / sqrt(n))
  
  (fig4a = map %>%
      group_by(lon, lat, island_group) %>%
      summarise(est = mean(est)) %>%
      ggplot(aes_string("lon", "lat", fill = "exp(est)")) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      facet_wrap(~island_group, scales = "free", ncol = 4) + 
      scale_fill_gradientn("", colours = matlab.like(100)) + 
      xlab("Longitude (dec deg)") +
      ylab("Latitude (dec deg)") + 
      theme_minimal() + 
      theme(aspect.ratio = 0.8,
            # legend.justification = c(0, 1), 
            # legend.position = c(0, 1),
            # legend.key = element_rect(colour = NA, fill = NA),
            # legend.text = element_text(color = "white", size = 12),
            legend.key.size = unit(0.5, "cm"),
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = "(a)"))
  
  (fig4aa = map %>%
      subset(island_group == "Ni'ihau-Kaua'i") %>% 
      group_by(lon, lat, island_group) %>%
      summarise(est = mean(est)) %>%
      ggplot(aes_string("lon", "lat", fill = "exp(est)")) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      coord_fixed() +
      scale_fill_gradientn("", colours = matlab.like(100)) + 
      # xlab("Longitude (dec deg)") +
      # ylab("Latitude (dec deg)") + 
      ylab("") + xlab("") + 
      theme_minimal() + 
      ggtitle("Ni'ihau-Kaua'i", subtitle = "Max. estimated density per 100 sq.m = 3.6") +
      theme(legend.justification = c(0, 1), 
            legend.position = c(0, 1.05),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text = element_text(color = "white", size = 12),
            legend.key.size = unit(0.5, "cm"),
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = "(a)"))
  
  (fig4ab = map %>%
      subset(island_group == "O'ahu") %>% 
      group_by(lon, lat, island_group) %>%
      summarise(est = mean(est)) %>%
      ggplot(aes_string("lon", "lat", fill = "exp(est)")) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      coord_fixed() +
      scale_fill_gradientn("", colours = matlab.like(100)) + 
      # xlab("Longitude (dec deg)") +
      # ylab("Latitude (dec deg)") + 
      ylab("") + xlab("") + 
      theme_minimal() + 
      ggtitle("O'ahu", subtitle = "Max. estimated density per 100 sq.m = 1.01") +
      theme(legend.justification = c(0, 0), 
            legend.position = c(0, 0),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text = element_text(color = "white", size = 12),
            legend.key.size = unit(0.5, "cm"),
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = ""))
  
  (fig4ac = map %>%
      subset(island_group == "Moloka'i-Maui-Lana'i-Kaho'olawe") %>% 
      group_by(lon, lat, island_group) %>%
      summarise(est = mean(est)) %>%
      ggplot(aes_string("lon", "lat", fill = "exp(est)")) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      coord_fixed() +
      scale_fill_gradientn("", colours = matlab.like(100)) + 
      # xlab("Longitude (dec deg)") +
      # ylab("Latitude (dec deg)") + 
      ylab("") + xlab("") + 
      theme_minimal() + 
      ggtitle("Moloka'i-Maui-Lana'i-Kaho'olawe", subtitle = "Max. estimated density per 100 sq.m = 7.27") +
      theme(legend.justification = c(0, 0), 
            legend.position = c(0, 0),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text = element_text(color = "white", size = 12),
            legend.key.size = unit(0.5, "cm"),
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = ""))
  
  (fig4ad = map %>%
      subset(island_group == "Hawai'i") %>% 
      group_by(lon, lat, island_group) %>%
      summarise(est = mean(est)) %>%
      ggplot(aes_string("lon", "lat", fill = "exp(est)")) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      coord_fixed() +
      scale_fill_gradientn("", colours = matlab.like(100)) + 
      # xlab("Longitude (dec deg)") +
      # ylab("Latitude (dec deg)") + 
      ylab("") + xlab("") + 
      theme_minimal() + 
      ggtitle("Hawai'i", subtitle = "Max. est. density per 100 sq.m = 2.11") +
      theme(legend.justification = c(1, 1), 
            legend.position = c(1, 1.03),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text = element_text(color = "white", size = 12),
            legend.key.size = unit(0.5, "cm"),
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = ""))
  
  fig4a = fig4aa | fig4ab | fig4ac | fig4ad
  rm(fig4aa, fig4ab, fig4ac, fig4ad)
  
  betaf = function(vec){
    
    n = length(vec)
    
    beta = lm(vec ~ seq(1:n))$coef[2]
    
    # p = summary(lm(vec ~ seq(1:36)))$ coefficients [2,4]
    return(beta) # beta gives you a slope, if you want p-value, change it to p
    # return(p) # beta gives you a slope, if you want p-value, change it to p
    
  }
  
  slope = NULL
  
  for (y in 1:length(unique(map$year))) {
    
    # y = 1
    
    slope_i = map %>% 
      subset(model == "dynamic_model" & year ==  unique(map$year)[y]) %>% 
      group_by(lon, lat) %>% 
      # mutate(est = exp(est)) %>%
      summarise(est = mean(est, na.rm = T)) 
    
    colnames(slope_i)[3] = unique(map$year)[y]
    
    if (y == 1) {
      
      slope = slope_i
      
    } else {
      
      slope = merge(slope, slope_i)
      
    }
    
  }
  
  res = as.data.frame(apply(slope[, 3:length(names(slope))], 1, betaf))
  slope = cbind(slope[,1:2], res)
  colnames(slope)[3] = "b"
  
  island_group = map %>% 
    group_by(lon, lat, island_group) %>% 
    summarise(est = mean(est))
  
  slope = merge(slope, island_group)
  
  (fig4b = slope %>%
      ggplot(aes(lon, lat, fill = b)) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      facet_wrap(.~ island_group, scale = "free", nrow = 1) +
      scale_fill_distiller(palette ="RdBu",
                           direction = -1, "Change rate",
                           limits = c(quantile(slope$b, 0.999)*-1, quantile(slope$b, 0.999))) +
      xlab("Longitude (dec deg)") +
      ylab("Latitude (dec deg)") + 
      theme_minimal() +
      theme(legend.position = "right",
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) +
      ggtitle("Linear trend 2010-2019") +
      labs(tag = "(b)"))
  
  library(scales)
  
  (slope_hist = slope %>%
      ggplot(aes(x = b, fill=..x..)) +
      geom_histogram(aes(y = (..count..)/sum(..count..)), show.legend = F, bins = 30) + 
      scale_fill_gradient2(midpoint = 0,
                           low = "blue",
                           mid = "white",
                           high = "red") +
      geom_vline(xintercept = 0, color = "gray30", linetype="dotted") + 
      facet_wrap(.~ island_group, scale = "free_y", nrow = 4) +
      xlab("") + ylab("Proportion") + 
      theme_half_open())
  
  slope %>% 
    group_by(island_group) %>% 
    summarise(median_b = median(b))
  
  (fig4ba = slope %>%
      subset(island_group == "Ni'ihau-Kaua'i") %>% 
      ggplot(aes(lon, lat, fill = b)) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      coord_fixed() +
      scale_fill_distiller(palette ="RdBu", direction = -1, "") +
      ylab("") + xlab("") + 
      theme_minimal() + 
      ggtitle("Ni'ihau-Kaua'i", subtitle = "Median change rate = -0.054 year-1") +
      theme(legend.justification = c(0, 1), 
            legend.position = c(0, 1.05),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text = element_text(color = "white", size = 12),
            legend.key.size = unit(0.5, "cm"),
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = "(b)"))
  
  (fig4bb = slope %>%
      subset(island_group == "O'ahu") %>% 
      ggplot(aes(lon, lat, fill = b)) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      coord_fixed() +
      scale_fill_distiller(palette ="RdBu", direction = -1, "") +
      ylab("") + xlab("") + 
      theme_minimal() + 
      ggtitle("O'ahu", subtitle = "Median change rate = -0.057 year-1") +
      theme(legend.justification = c(0, 0), 
            legend.position = c(0, 0.01),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text = element_text(color = "white", size = 12),
            legend.key.size = unit(0.5, "cm"),
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = ""))
  
  (fig4bc = slope %>%
      subset(island_group == "Moloka'i-Maui-Lana'i-Kaho'olawe") %>% 
      ggplot(aes(lon, lat, fill = b)) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      coord_fixed() +
      scale_fill_distiller(palette ="RdBu", direction = -1, "") +
      ylab("") + xlab("") + 
      theme_minimal() + 
      ggtitle("Moloka'i-Maui-Lana'i-Kaho'olawe", subtitle = "Median change rate = -0.085 year-1") +
      theme(legend.justification = c(0, 0), 
            legend.position = c(0, 0.01),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text = element_text(color = "white", size = 12),
            legend.key.size = unit(0.5, "cm"),
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = ""))
  
  (fig4bd = slope %>%
      subset(island_group == "Hawai'i") %>% 
      ggplot(aes(lon, lat, fill = b)) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      coord_fixed() +
      scale_fill_distiller(palette ="RdBu", direction = -1, "") +
      ylab("") + xlab("") + 
      theme_minimal() + 
      ggtitle("Hawai'i", subtitle = "Median change rate: -0.073 year-1") +
      theme(legend.justification = c(1,1), 
            legend.position = c(1, 1.04),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text = element_text(color = "white", size = 12),
            legend.key.size = unit(0.5, "cm"),
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = ""))
  
  fig4b = fig4ba | fig4bb | fig4bc | fig4bd
  rm(fig4ba, fig4bb, fig4bc, fig4bd)
  
  png(paste0("outputs/fig4_", n_knots[k], ".png"), height = 8, width = 20, units = "in", res = 500)
  print(fig4a / fig4b) 
  dev.off()
  
  png(paste0("outputs/figs_slope_", n_knots[k], ".png"), height = 8, width = 6, units = "in", res = 500)
  print(slope_hist)
  dev.off()
  
  rm(island_group, res, slope_i, slope, slope_hist)
  
  index = map %>% 
    mutate(year = as.numeric(year),
           est = exp(est)) 
  
  island_group_list = as.character(unique(index$island_group))
  
  index_by_island_group = NULL
  
  for (isl in 1:length(island_group_list)) {
    
    # isl = 4
    
    index_group = index %>% 
      subset(island_group == island_group_list[isl]) %>%
      Rmisc::summarySE(measurevar = "est", groupvars = c("year"))
    
    index_group$group = island_group_list[isl]
    index_by_island_group = rbind(index_by_island_group, index_group)
    
  }
  
  library(plyr)
  
  b_df <- ddply(index_by_island_group, .(group), summarise, slope = round(summary(lm(scale(est)~year))$coefficients[2], 3))
  p_df <- ddply(index_by_island_group, .(group), summarise, p = round(summary(lm(scale(est)~year))$coefficients[8], 3))
  # p_df$p = ifelse(p_df$p < 0.05, "<0.05", paste0("=", p_df$p))
  summary = merge(b_df, p_df)
  summary
  
  detach("package:plyr", unload = TRUE)
  
  est = index_by_island_group %>% group_by(group) %>% summarise(est = mean(est))
  summary = merge(summary, est)
  
  (fig5a = index_by_island_group %>% 
      select(year, group, est, ci) %>% 
      ggplot(aes(year, est, group = group, color = est)) + 
      geom_point(size = 2, show.legend = F) + 
      geom_errorbar(aes(ymin = est-ci, ymax = est+ci), width = 0.01, show.legend = T) + 
      facet_wrap(~group, scales = "free_y", ncol = 2) +
      geom_smooth(method = "lm", se = F, size = 0.5, color = "gray30") + 
      geom_text(data = summary,
                aes(label = paste0("\n β = ", slope, "\n p = ", p)),
                color = "gray20",
                x = -Inf, y = -Inf,
                hjust = -0.1,
                vjust = -0.2,
                size = 4) +
      scale_color_gradientn(colours = matlab.like(10), "") + 
      ylab("Individuals per 100 sq.m") + 
      xlab("") + 
      scale_x_continuous(breaks = unique(index_by_island_group$year)) +
      theme_half_open() + 
      theme(#legend.position = "top",
            #legend.justification = c(0,1),
            aspect.ratio = 1,
            axis.text.x = element_text(angle = 90, vjust = 0.7)) +
      labs(tag = "(a)"))
  
  png(paste0("outputs/fig5a_", n_knots[k], ".png"), height = 7, width = 8, units = "in", res = 500)
  fig5a
  dev.off()
  
  rm(b_df, p_df, island_group_list, index)
  
  gravity = NULL
  
  gravity_year = unique(map$year)
  
  for (i in 1:length(unique(map$year))){
    
    # i = 1
    
    gravity_i = map %>% 
      subset(year == gravity_year[i]) %>% 
      select(lon, lat, depth, est)
    
    X = sum(gravity_i$est * gravity_i$lon) / sum(gravity_i$est) #for Lon
    Y = sum(gravity_i$est * gravity_i$lat) / sum(gravity_i$est) #for Lat
    Z = sum(gravity_i$est * gravity_i$depth) / sum(gravity_i$est) #for Lat
    
    xyz = data.frame(lon = X, lat = Y, depth = Z)
    xyz$year = gravity_year[i]
    
    gravity = rbind(gravity, xyz)
    
  }
  
  gravity$period = ifelse(gravity$year %in% c("2010", "2012", "2013"), "2010-2013", "2015-2019")
  
  st.err <- function(x) {
    sd(x)/sqrt(length(x))
  }
  
  lon_se <- aggregate(lon ~ period, gravity, st.err)
  lat_se <- aggregate(lat ~ period, gravity, st.err)
  se = merge(lon_se, lat_se)
  colnames(se) = c("Period","Lon_se","Lat_se")
  
  load('data/MHI_islands_shp.RData')
  crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ISL_bounds = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
  ISL_bounds <- crop(ISL_bounds, extent(range(pretty(gravity$lon))[1]-0.1, 
                                        range(pretty(gravity$lon))[2]+0.1, 
                                        range(pretty(gravity$lat))[1]-0.1, 
                                        range(pretty(gravity$lat))[2]+0.1))
  
  (fig4b = gravity %>% 
      ggplot() +
      geom_point(aes(lon, lat), size = 2) +
      geom_segment(aes(x = lon, 
                       y = lat,
                       xend = c(tail(lon, n = -1), NA),
                       yend = c(tail(lat, n = -1), NA)),
                   arrow = arrow(length = unit(0.4, "cm")),
                   show.legend = F) +
      geom_polygon(data = ISL_bounds, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile
      # geom_errorbar(aes(x = x_est, xmin = x_lwr, xmax = x_upr), width = 0, position = position_dodge(width = 0.5), show.legend = F)  +
      # geom_errorbar(aes(y = y_est, ymin = y_lwr, ymax = y_upr), width = 0, position = position_dodge(width = 0.5), show.legend = F)  +
      ggrepel::geom_label_repel(data = gravity, aes(lon, lat, label = year, fill = year), 
                                colour = "white", fontface = "bold", position = "identity", show.legend = F) + 
      viridis::scale_color_viridis(discrete = T, begin = 0, end = 0.9) +
      viridis::scale_fill_viridis(discrete = T, begin = 0, end = 0.9) +
      xlab("Eastings (km)") +
      ylab("Northings (km)") + 
      coord_fixed() +
      theme_minimal() + 
      labs(tag = "(b)"))
  
  cog_static_x = cog %>% 
    subset(coord == "X" & model == "static_model") %>% 
    # subset(island != "All") %>%
    subset(island == "All") %>%
    group_by(year, island, model) %>% 
    summarise(x_est = mean(est), 
              x_upr = mean(upr), 
              x_lwr = mean(lwr)) %>% 
    select(year, x_est, x_upr, x_lwr, island, model)
  
  cog_static_y = cog %>% 
    subset(coord == "Y" & model == "static_model") %>% 
    # subset(island != "All") %>%
    subset(island == "All") %>%
    group_by(year, island, model) %>% 
    summarise(y_est = mean(est), 
              y_upr = mean(upr), 
              y_lwr = mean(lwr)) %>% 
    select(year, y_est, y_upr, y_lwr, island, model)
  
  cog_static = merge(cog_static_x, cog_static_y); rm(cog_static_x, cog_static_y)
  
  cog_dynamic_x = cog %>% 
    subset(coord == "X" & model == "dynamic_model") %>% 
    # subset(island != "All") %>%
    subset(island == "All") %>%
    group_by(year, island, model) %>% 
    summarise(x_est = mean(est), 
              x_upr = mean(upr), 
              x_lwr = mean(lwr)) %>% 
    select(year, x_est, x_upr, x_lwr, island, model)
  
  cog_dynamic_y = cog %>% 
    subset(coord == "Y" & model == "dynamic_model") %>% 
    # subset(island != "All") %>%
    subset(island == "All") %>%
    group_by(year, island, model) %>% 
    summarise(y_est = mean(est), 
              y_upr = mean(upr), 
              y_lwr = mean(lwr)) %>% 
    select(year, y_est, y_upr, y_lwr, island, model)
  
  cog_dynamic = merge(cog_dynamic_x, cog_dynamic_y); rm(cog_dynamic_x, cog_dynamic_y)
  
  cogs = rbind(cog_static, cog_dynamic) %>% 
    # subset(model == "static_model") %>%
    subset(model == "dynamic_model")
  
  coordinates(cogs) <- ~x_est + y_est 
  proj4string(cogs) <- CRS("+proj=utm +zone=5 +datum=WGS84 +units=km +ellps=WGS84") 
  cogs <- spTransform(cogs,CRS("+proj=longlat +datum=WGS84"))
  cogs = as.data.frame(cogs)
  
  # zone = 5
  load('data/MHI_islands_shp.RData')
  crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ISL_bounds = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
  ISL_bounds = st_transform(st_as_sf(ISL_bounds))
  ISL_bounds <- ms_simplify(ISL_bounds, keep = 0.001, keep_shapes = F)
  # ISL_bounds = spTransform(ISL_bounds, CRS(paste0("+proj=utm +units=km +zone=", zone)))
  # ISL_bounds <- crop(ISL_bounds, extent(min(cogs$x_est) - 0.2, 
  #                                       max(cogs$x_est) + 0.2, 
  #                                       min(cogs$y_est) - 0.2, 
  #                                       max(cogs$y_est) + 0.2))
  
  load("data/rea/ALL_REA_FISH_RAW_SST.RData")
  
  label = df %>% 
    subset(REGION == "MHI" & ISLAND %in% islands) %>%
    group_by(ISLAND) %>% 
    summarise(lat = mean(LATITUDE),
              lon = mean(LONGITUDE))
  
  (fig5b_inset = ggplot() +
      geom_sf(data = ISL_bounds) +
      coord_sf(crs = st_crs(4135)) + 
      geom_rect(aes(xmin = -158, xmax = -157, ymin = 20.5, ymax = 21.3), fill = "transparent", color = "red", size = 1.5) + 
      geom_label_repel(data = label, 
                       aes(x = lon, y = lat, label = ISLAND), 
                       label.size = NA,
                       fontface = "bold",   
                       label.padding = 0.5, 
                       na.rm = T,
                       fill = alpha(c("white"), 0.8),
                       nudge_x = c(0.2, 0.2, 0.2, 0.2, 0.2),
                       nudge_y = c(0.2, 0.2, 0.2, 0.2, 0.2)) + 
      theme_linedraw() + 
      theme( panel.background = element_rect(fill = alpha('white', 0.9)), # bg of the panel
             plot.background = element_rect(fill = alpha('white', 0.1)), # bg of the plot
             panel.grid.major = element_blank(),
             axis.title = element_blank()))
  
  b = marmap::getNOAA.bathy(lon1 = -158,
                             lon2 = -157,
                             lat1 = 20.5,
                             lat2 = 21.3,
                             resolution = 1)
  
  b = marmap::fortify.bathy(b)
  
  (fig5b = ggplot() +
      geom_point(data = cogs, aes(x_est, y_est)) +
      geom_segment(data = cogs, 
                   aes(x = x_est, 
                       y = y_est,
                       xend = c(tail(x_est, n = -1), NA),
                       yend = c(tail(y_est, n = -1), NA)),
                   size = 0.5,
                   arrow = arrow(length = unit(0.2, "cm")),
                   show.legend = F) +
      geom_contour(data = b,
                   aes(x = x, y = y, z = z, colour = stat(level)),
                   breaks = seq(-5000, 0, by = 100),
                   size = c(0.5)) +
      scale_colour_distiller(palette = "Blues", direction = 1, "Depth (m)") +
      ggnewscale::new_scale_color() +
      ggnewscale::new_scale_fill() +
      # geom_errorbar(aes(x = x_est, xmin = x_lwr, xmax = x_upr), width = 0, position = position_dodge(width = 0.5), show.legend = F)  +
      # geom_errorbar(aes(y = y_est, ymin = y_lwr, ymax = y_upr), width = 0, position = position_dodge(width = 0.5), show.legend = F)  +
      geom_label_repel(data = cogs, 
                       aes(x_est, y_est, label = year),
                       fontface = "bold",   
                       na.rm = T,
                       size = 8,
                       fill = alpha(c("gray20"), 0.8),
                       colour = "white",
                       segment.size = 0.25,
                       segment.color = 'gray60',
                       # direction = "y",
                       nudge_x = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
                       nudge_y = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
                       show.legend = F) + 
      coord_fixed() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_half_open() +
      labs(y = " ", x = "") + 
      theme( legend.position = c(0,1),
             legend.justification = c(-0.2,1.1),
             axis.ticks = element_blank(),
             axis.text = element_blank()) + 
      labs(tag = "(b)"))
  
  fig5b <-
    ggdraw() +
    draw_plot(fig5b) +
    draw_plot(fig5b_inset, x = 0.06, y = 0.02, width = 0.6, height = 0.6)
  
  png( paste0("outputs/fig5b_", n_knots[k], ".png"), height = 8, width = 10, units = "in", res = 300)
  fig5b
  dev.off()
  
}