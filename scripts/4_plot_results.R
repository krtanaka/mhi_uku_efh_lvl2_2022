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

rm(list = ls())

select = dplyr::select

Ibbox = read.csv("data/Island_Bounding_Boxes.csv", stringsAsFactors = F) # Updated Bounding boxes 2021
islands = c( "Hawaii", "Kahoolawe", "Kauai", "Lanai", "Maui", "Molokai", "Niihau", "Oahu", "All")#[1:8]

n_knots = c(1000, 500, 300, 100)

for (k in 1:length(n_knots)) {
  
  k = 2
  
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
    # subset(island != "All") %>%
    subset(model == "dynamic_model")
  
  (map1 = map %>%
      group_by(lon, lat, island) %>%
      summarise(est = mean(est)) %>%
      ggplot(aes_string("lon", "lat", fill = "exp(est)")) +
      geom_tile(aes(height = 0.01, width = 0.01)) +
      # facet_wrap(.~ model + island, scale = "free") +
      facet_wrap(.~ island, scale = "free", nrow = 1) +
      # coord_fixed() + 
      scale_fill_gradientn("Ind. per 100 sq.m", 
                           colours = matlab.like(100),
                           # trans = "sqrt",
                           na.value = "yellow", 
                           limits = c(0, quantile(exp(map$est), 0.99))) + 
      # ggtitle("Mean density 2010-2019 (fixed effects + all random effects)", 
      #         subtitle = paste("maximum estimated density =", 
      #                          round(max(map$est), 2))) + 
      # xlab("Eastings (km)") +
      # ylab("Northings (km)") + 
      xlab("Eastings (km)") +
      ylab("Northings (km)") + 
      theme_minimal() + 
      theme(legend.position = "right",
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
      labs(tag = "(a)"))
  
  slope = NULL
  
  for (y in 1:length(unique(map$year))) {
    
    # y = 1
    
    slope_i = map %>% 
      subset(model == "dynamic_model" & year ==  unique(map$year)[y]) %>% 
      group_by(X, Y) %>% 
      # mutate(est = exp(est)) %>% 
      summarise(est = mean(est, na.rm = T)) 
    
    colnames(slope_i)[3] = unique(map$year)[y]
    
    if (y == 1) {
      
      slope = slope_i
      
    } else {
      
      slope = merge(slope, slope_i)
      
    }
    
  }
  
  betaf = function(vec){
    
    n = length(vec)
    
    beta = lm(vec ~ seq(1:n))$coef[2] #this is for 1982-2011
    
    # p = summary(lm(vec ~ seq(1:36)))$ coefficients [2,4]
    return(beta) # beta gives you a slope, if you want p-value, change it to p
    #   return(p) # beta gives you a slope, if you want p-value, change it to p
    
  }
  
  res = as.data.frame(apply(slope[, 3:length(names(slope))], 1, betaf))
  slope = cbind(slope[,1:2], res)
  colnames(slope)[3] = "b"
  
  island_est = map %>% 
    group_by(X, Y, island) %>% 
    summarise(est = mean(est))
  
  slope = merge(slope, island_est)
  
  (map2 = slope %>%
      ggplot(aes(X, Y, fill = b)) +
      geom_tile(aes(height = 1, width = 1)) +
      facet_wrap(.~ island, scale = "free", nrow = 1) +
      scale_fill_distiller(palette ="RdBu",
                           direction = -1, "Change rate",
                           limits = c(quantile(slope$b, 0.001), quantile(slope$b, 0.999))) +
      xlab("Eastings (km)") +
      ylab("Northings (km)") +
      theme_minimal() +
      theme(legend.position = "right",
            panel.background = element_rect(fill = "gray10", colour = "gray10"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) +
      # ggtitle("Linear trend 2010-2019") + 
      labs(tag = "(b)"))
  
  (slope_hist = slope %>%
      ggplot(aes(b, fill = cut(b, 100))) +
      geom_histogram(show.legend = F, bins = 30) + 
      geom_vline(xintercept = 0, color = "gray30", linetype="dotted") + 
      facet_wrap(.~ island, scale = "free_y", nrow = 2) +
      theme_pubr())
  
  png(paste0("outputs/fig3_", n_knots[k], ".png"), height = 6, width = 18, units = "in", res = 500)
  map1 / map2 
  dev.off()
  
  png(paste0("outputs/slope_hist_", n_knots[k], ".png"), height = 8, width = 16, units = "in", res = 500)
  slope_hist
  dev.off()
  
  rm(island_est, res, slope_i, slope)
  
  index_a %>% 
    # subset(island == "All") %>%
    subset(island != "All") %>%
    ggplot(aes(year, est, color = model, fill = model)) +
    geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(x = year, ymin = lwr, ymax = upr), width = 0, position = position_dodge(width = 0.5), show.legend = F)  +
    # geom_smooth(method = "lm", show.legend = F, se = F) +
    facet_wrap(.~island, scales = "free", nrow = 2) +
    xlab("Year") +
    ylab("Density Estimate") +
    # labs(color = "") +
    theme_minimal() + 
    theme(legend.position = "right",
          panel.background = element_rect(fill = "gray10", colour = "gray10"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20"))
  
  index_b %>% 
    # subset(island == "All") %>%
    subset(island != "All") %>%
    # subset(model == "dynamic_model") %>%
    ggplot(aes(year, est, color = model, fill = model, group = model)) +
    geom_point(size = 1, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(x = year, ymin = lwr, ymax = upr), width = 0, position = position_dodge(width = 0.5), show.legend = F)  +
    geom_smooth(method = "lm", se = F, show.legend = F) + 
    facet_wrap(.~ island, scale = "free", nrow = 2) +
    xlab("Year") +
    ylab("Mean Relative Density Estimate") +
    # labs(color = "") +
    theme_minimal() + 
    theme(legend.position = "right",
          panel.background = element_rect(fill = "gray10", colour = "gray10"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) 
  
  trend = index_b %>% 
    # subset(island == "All") %>%
    subset(island != "All") %>%
    subset(model == "dynamic_model") %>% 
    mutate(year = as.numeric(year))
  
  library(plyr)
  
  b_df <- ddply(trend, .(island), summarise, slope=round(summary(lm(scale(est)~year))$coefficients[2], 3))
  p_df <- ddply(trend, .(island), summarise, p=round(summary(lm(scale(est)~year))$coefficients[8], 3))
  # p_df$p = ifelse(p_df$p < 0.05, "<0.05", paste0("=", p_df$p))
  summary = merge(b_df, p_df)
  summary
  
  (trend_fig = ggplot(trend, aes(year, est)) +
      geom_point(size = 2, aes(color = est), show.legend = F) +
      geom_line(size = 0.1) +
      geom_smooth(method = "lm", se = F, size = 0.5, color = "gray30") +
      geom_errorbar(aes(x = year, ymin = lwr, ymax = upr, color = est), width = 0.05, show.legend = F) +
      scale_color_gradientn(colors = matlab.like(10)) + 
      geom_text(data = summary,
                aes(label = paste0("\n β = ", slope, "\n p = ", p)),
                x = -Inf, y = -Inf,
                hjust = -0.1,
                vjust = -0.2,
                size = 4) +
      ylab("Individuals per 100 sq.m") + 
      xlab("") + 
      scale_x_continuous(breaks = unique(trend$year)) +
      facet_wrap(.~ island, scale = "free_y", nrow = 2) +
      theme_pubr() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) + 
      labs(tag = "(a)"))
  
  detach("package:plyr", unload = TRUE)
  
  rm(b_df, p_df, summary, trend)
  
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
  
  zone = 5
  load('data/MHI_islands_shp.RData')
  crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ISL_bounds = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
  ISL_bounds = spTransform(ISL_bounds, CRS(paste0("+proj=utm +units=km +zone=", zone)))
  ISL_bounds <- crop(ISL_bounds, extent(range(pretty(cogs$x_est))[1]-20, 
                                        range(pretty(cogs$x_est))[2]+20, 
                                        range(pretty(cogs$y_est))[1]-20, 
                                        range(pretty(cogs$y_est))[2]+20))
  
  (cog_mhi = ggplot() +
      geom_point(data = cogs, aes(x_est, y_est), size = 2) +
      geom_segment(data = cogs, 
                   aes(x = x_est, 
                       y = y_est,
                       xend = c(tail(x_est, n = -1), NA),
                       yend = c(tail(y_est, n = -1), NA)),
                   arrow = arrow(length = unit(0.4, "cm")),
                   show.legend = F) +
      geom_polygon(data = ISL_bounds, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile
      # geom_errorbar(aes(x = x_est, xmin = x_lwr, xmax = x_upr), width = 0, position = position_dodge(width = 0.5), show.legend = F)  +
      # geom_errorbar(aes(y = y_est, ymin = y_lwr, ymax = y_upr), width = 0, position = position_dodge(width = 0.5), show.legend = F)  +
      geom_label_repel(data = cogs, aes(x_est, y_est, label = year, fill = year), colour = "white", fontface = "bold", position = "identity", show.legend = F) + 
      viridis::scale_color_viridis(discrete = T, begin = 0, end = 0.9) +
      viridis::scale_fill_viridis(discrete = T, begin = 0, end = 0.9) +
      xlab("Eastings (km)") +
      ylab("Northings (km)") + 
      # coord_fixed() +
      theme_pubr() + 
      labs(tag = "(b)"))
  
  png(paste0("outputs/fig4_", n_knots[k], ".png"), height = 10, width = 10, units = "in", res = 500)
  trend_fig / cog_mhi
  dev.off()

}
