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
library(viridis)

rm(list = ls())

select = dplyr::select

n_knots = c(1000, 500, 300, 100)

for (k in 1:length(n_knots)) {
  
  # k = 2
  
  density_model_static = readRDS(paste0("outputs/density_model_static_", n_knots[k], ".rds"))
  density_model_dynamic = readRDS(paste0("outputs/density_model_dynamic_", n_knots[k], ".rds"))
  density_model_full = readRDS(paste0("outputs/density_model_full_", n_knots[k], ".rds"))
  
  # load("data/grid_env/bathymetry_date_dyamic_layers.RData") # STRM15 with dates, year and environmental variables produced by EDS (see K.Tanaka - EFH Branch)
  SM = readRDS("data/grid_env/grid_env_chla_wind_sst_0.01decdeg.rds")
  # SM = readRDS("data/grid_env/grid_env_chla_wind_sst_15arcsec.rds")
  
  grid = SM; rm(SM)
  
  # grid[grid == "NaN"] <- NA
  
  grid <- grid %>% select(-contains("ALLB4"))
  grid <- grid %>% select(-contains("MO03"))
  grid <- grid %>% select(-contains("YR03"))
  grid <- grid %>% select(-contains("YR05"))
  grid <- grid %>% select(-contains("YR10"))
  grid <- grid %>% select(-contains("YR10YR01"))
  grid <- grid %>% select(-contains("8Day_WK01"))
  grid <- grid %>% select(-contains("_WK01"))
  grid <- grid %>% select(-contains("_YR01"))
  grid <- grid %>% select(-contains("mean_annual_range_"))
  grid <- grid %>% select(-contains("mean_monthly_range_"))
  
  grid$chla_8d = rowMeans(grid[,c("mean_Chlorophyll_A_AquaMODIS_8Day_DY01","mean_Chlorophyll_A_ESAOCCCI_8Day_DY01")], na.rm = T)
  grid = grid[ , !names(grid) %in% c("mean_Chlorophyll_A_AquaMODIS_8Day_DY01","mean_Chlorophyll_A_ESAOCCCI_8Day_DY01")] 
  
  grid$chla_m = rowMeans(grid[,c("mean_Chlorophyll_A_AquaMODIS_8Day_MO01","mean_Chlorophyll_A_ESAOCCCI_8Day_MO01")], na.rm = T)
  grid = grid[ , !names(grid) %in% c("mean_Chlorophyll_A_AquaMODIS_8Day_MO01","mean_Chlorophyll_A_ESAOCCCI_8Day_MO01")] 
  
  # grid$q05_chla_8d = rowMeans(grid[,c("q05_Chlorophyll_A_AquaMODIS_8Day_DY01","q05_Chlorophyll_A_ESAOCCCI_8Day_DY01")], na.rm = T)
  grid = grid[ , !names(grid) %in% c("q05_Chlorophyll_A_AquaMODIS_8Day_DY01","q05_Chlorophyll_A_ESAOCCCI_8Day_DY01")] 
  
  grid$q05_chla_m = rowMeans(grid[,c("q05_Chlorophyll_A_AquaMODIS_8Day_MO01","q05_Chlorophyll_A_ESAOCCCI_8Day_MO01")], na.rm = T)
  grid = grid[ , !names(grid) %in% c("q05_Chlorophyll_A_AquaMODIS_8Day_MO01","q05_Chlorophyll_A_ESAOCCCI_8Day_MO01")] 
  
  # grid$q95_chla_8d = rowMeans(grid[,c("q95_Chlorophyll_A_AquaMODIS_8Day_DY01","q95_Chlorophyll_A_ESAOCCCI_8Day_DY01")], na.rm = T)
  grid = grid[ , !names(grid) %in% c("q95_Chlorophyll_A_AquaMODIS_8Day_DY01","q95_Chlorophyll_A_ESAOCCCI_8Day_DY01")] 
  
  grid$q95_chla_m = rowMeans(grid[,c("q95_Chlorophyll_A_AquaMODIS_8Day_MO01","q95_Chlorophyll_A_ESAOCCCI_8Day_MO01")], na.rm = T)
  grid = grid[ , !names(grid) %in% c("q95_Chlorophyll_A_AquaMODIS_8Day_MO01","q95_Chlorophyll_A_ESAOCCCI_8Day_MO01")] 
  
  # grid$sd_chla_8d = rowMeans(grid[,c("sd_Chlorophyll_A_AquaMODIS_8Day_DY01","sd_Chlorophyll_A_ESAOCCCI_8Day_DY01")], na.rm = T)
  grid = grid[ , !names(grid) %in% c("sd_Chlorophyll_A_AquaMODIS_8Day_DY01","sd_Chlorophyll_A_ESAOCCCI_8Day_DY01")] 
  
  grid$sd_chla_m = rowMeans(grid[,c("sd_Chlorophyll_A_AquaMODIS_8Day_MO01","sd_Chlorophyll_A_ESAOCCCI_8Day_MO01")], na.rm = T)
  grid = grid[ , !names(grid) %in% c("sd_Chlorophyll_A_AquaMODIS_8Day_MO01","sd_Chlorophyll_A_ESAOCCCI_8Day_MO01")] 
  
  names(grid) <- gsub("mean_", "", names(grid)); names(grid)
  names(grid) <- gsub("_DY01", "_d", names(grid)); names(grid)
  names(grid) <- gsub("_MO01", "_m", names(grid)); names(grid)
  
  names(grid) <- gsub("Ocean_Surface_Winds", "wind", names(grid)); names(grid)
  names(grid) <- gsub("SST_CRW_Daily", "sst", names(grid)); names(grid)
  names(grid) <- gsub("Bathymetry", "depth", names(grid)); names(grid)
  
  # remove those bc don't relaly make sense
  grid = grid[ , !names(grid) %in% c("q05_wind_d","q95_wind_d", "sd_wind_d", "q05_sst_d", "q95_sst_d", "sd_sst_d")] ; names(grid)
  
  grid = grid[complete.cases(grid), ]
  names(grid) <- tolower(names(grid))
  
  grid = grid %>% 
    mutate(depth = bathymetry*-1) %>% 
    select(lon, lat, depth, date_r, year, 
           sst_d, chla_8d, wind_d, 
           sst_m, chla_m, wind_m,
           q95_sst_m, q95_chla_m, q95_wind_m,
           q05_sst_m, q05_chla_m, q05_wind_m,
           sd_sst_m, sd_chla_m, sd_wind_m) 
  
  # grid = grid %>% subset(depth < 30)
  
  Ibbox = read.csv("data/Island_Bounding_Boxes.csv", stringsAsFactors = F) # Updated Bounding boxes 2021
  islands = unique(Ibbox$ISLAND.CODE); islands
  islands = c( "Hawaii", "Kahoolawe", "Kauai", "Lanai", "Maui", "Molokai", "Niihau", "Oahu", "All")
  
  for (i in 1:length(islands)) {
    
    # i = 9
    
    if (i %in% 1:8) {
      
      box = Ibbox %>% subset(ISLAND.CODE == islands[i])
      
      grid_i <- grid %>% subset(lon > box$RIGHT_XMAX & 
                                  lon < box$LEFT_XMIN & 
                                  lat > box$TOP_YMAX & 
                                  lat < box$BOTTOM_YMIN)
      grid_i %>% 
        ggplot(aes(lon, lat, fill = sd_chla_m)) + 
        geom_raster() + 
        facet_wrap(.~year) + 
        scale_fill_viridis_c()
      
    } else {
      
      grid_i = grid
      
    }
    
    zone <- (floor((grid_i$lon[1] + 180)/6) %% 60) + 1
    xy_utm = as.data.frame(cbind(utm = project(as.matrix(grid_i[, c("lon", "lat")]),
                                               paste0("+proj=utm +units=km +zone=", zone))))
    
    colnames(xy_utm) = c("X", "Y"); plot(xy_utm, pch = ".")
    
    grid_i = cbind(grid_i, xy_utm)
    
    # set the area argument to 0.0081 km2 since our grid cells are 90 m x 90 m = 0.0081 square kilometers
    # predict to grid to estimate COG, biomass index with CI (default 95%)
    
    p_static <- predict(density_model_static, 
                        newdata = grid_i, 
                        return_tmb_object = T, 
                        area = 0.0081)
    
    p_dynamic <- predict(density_model_dynamic, 
                         newdata = grid_i, 
                         return_tmb_object = T, 
                         area = 0.0081)
    
    p_static$data %>% 
      group_by(X, Y, year) %>% 
      summarise(est = mean(est)) %>% 
      ggplot(aes_string("X", "Y", fill = "exp(est)")) + 
      geom_tile(width = 2, height = 2) + 
      facet_wrap(.~ year) + 
      scale_fill_viridis() + 
      ggdark::dark_theme_minimal()
    
    p_dynamic$data %>% 
      group_by(X, Y, year) %>% 
      summarise(est = mean(est)) %>% 
      ggplot(aes_string("X", "Y", fill = "exp(est)")) + 
      geom_tile(width = 2, height = 2) + 
      facet_wrap(.~ year) + 
      scale_fill_viridis() + 
      ggdark::dark_theme_minimal()
    
    saveRDS(p_static, paste0("outputs/predictions_static_", islands[i], "_", n_knots[k], ".rds"))
    saveRDS(p_dynamic, paste0("outputs/predictions_dynamic_", islands[i], "_", n_knots[k], ".rds"))
    rm(grid, grid_i); gc()
    
    # compute indices
    biomass_estimates_static = get_index(p_static, bias_correct = T); biomass_estimates_static; gc()
    biomass_estimates_dyamic = get_index(p_dynamic, bias_correct = T); biomass_estimates_dyamic; gc()
    
    plot(biomass_estimates_static$est, col = 2, pch = 20)
    points(biomass_estimates_dyamic$est, col = 4, pch = 20)
    
    saveRDS(biomass_estimates_static, paste0("outputs/biomass_bias_corrected_static_", islands[i], "_", n_knots[k], ".rds"))
    saveRDS(biomass_estimates_dyamic, paste0("outputs/biomass_bias_corrected_dynamic_", islands[i], "_", n_knots[k], ".rds"))
    gc()
    
    COG_static = get_cog(p_static, bias_correct = T); gc()
    COG_dynamic = get_cog(p_dynamic, bias_correct = T); gc()
    
    saveRDS(COG_static, paste0("outputs/COG_bias_corrected_static_", islands[i], "_", n_knots[k], ".rds"))
    saveRDS(COG_dynamic, paste0("outputs/COG_bias_corrected_dynamic_", islands[i], "_", n_knots[k], ".rds"))
    gc()
    
  }
  
}
