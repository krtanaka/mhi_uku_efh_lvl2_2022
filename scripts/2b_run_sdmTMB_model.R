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
            "Hawaii")#[7:11]

n_knots = c(1000, 500, 300, 100)

for (k in 1:length(n_knots)) {
  
  # k = 4
  
  load("outputs/clean_df.RData")
  
  zone <- (floor((df$lon[1] + 180)/6) %% 60) + 1
  xy_utm = as.data.frame(cbind(utm = project(as.matrix(df[, c("lon", "lat")]), paste0("+proj=utm +units=km +zone=", zone))))
  colnames(xy_utm) = c("X", "Y")
  df = cbind(df, xy_utm)
  plot(xy_utm, pch = ".", bty = 'n')
  rm(xy_utm)
  
  # Read in Island Boundaries
  load('data/MHI_islands_shp.RData')
  crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
  ISL_this_utm = spTransform(ISL_this,CRS(paste0("+proj=utm +units=km +zone=",zone)))
  ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=",zone))
  
  rea_spde <- make_mesh(df, c("X", "Y"), n_knots = n_knots[k], type = "cutoff_search", seed = 2022) # search
  # rea_spde <- make_mesh(df, c("X", "Y"), cutoff  = 15, type = "cutoff") # predefined
  
  #build barrier to mesh
  rea_spde_coast = add_barrier_mesh(rea_spde , ISL_this_sf)
  
  png(paste0("outputs/SPDE_mesh_field_", n_knots[k], "_knots.png"), units = "in", height = 10, width = 10, res = 500)
  plot(rea_spde_coast$mesh, asp = 1, main = ""); axis(1); axis(2)
  plot(ISL_this_utm, add = TRUE)
  points(rea_spde_coast$loc_xy,col = "green", pch = ".", cex = 5)
  bar_i = rea_spde_coast$barrier_triangles
  norm_i = rea_spde_coast$normal_triangles
  points(rea_spde_coast$spde$mesh$loc[,1], rea_spde_coast$spde$mesh$loc[,2], pch = ".", col = "black")
  points(rea_spde_coast$mesh_sf$V1[bar_i], rea_spde_coast$mesh_sf$V2[bar_i], col = "red", pch = 20, cex = 0.5)
  points(rea_spde_coast$mesh_sf$V1[norm_i], rea_spde_coast$mesh_sf$V2[norm_i], col = "blue", pch = 20, cex = 0.5)
  dev.off()
  
  rm(ISL_bounds, ISL_this, ISL_this_sf, ISL_this_utm)
  
  obs_year = unique(df$year)
  full_year = seq(min(df$year), max(df$year), by = 1)
  missing_year = setdiff(full_year, obs_year)
  missing_year = as.integer(missing_year);missing_year
  
  # only depth
  density_model_static <- sdmTMB(
    
    # using (date) works but results in poor model performances
    
    data = df, 
    
    formula = response ~ as.factor(year) + s(depth, k = 3),
    
    silent = F, 
    
    # extra_time = missing_year,
    
    # reml = T,
    
    time = "year",
    
    # spde = rea_spde,
    mesh = rea_spde_coast,
    
    # anisotropy = T,
    
    family = tweedie(link = "log")
    # spatiotemporal = "IID",
    # control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
    
  ); beepr::beep(2)
  
  density_model_static <- run_extra_optimization(density_model_static, newton_loops = 0); beepr::beep(2)
  density_model_static
  max(density_model_static$gradients)
  AIC(density_model_static)
  saveRDS(density_model_static, paste0("outputs/density_model_static_", n_knots[k], ".rds"))
  
  # VIF parsimonious model with dynamic covariates
  
  source("scripts/vif.R")
  
  vif <- df[, c("depth", 
                "sst_d", "chla_8d", "wind_d",
                "sst_m", "chla_m", "wind_m",
                "q95_sst_m", "q95_chla_m", "q95_wind_m", 
                "q05_sst_m", "q05_chla_m", "q05_wind_m", 
                "sd_sst_m", "sd_chla_m", "sd_wind_m")]
  
  vif_func(in_frame = vif, thresh = 3, trace = T) 
  
  density_model_dynamic <- sdmTMB(
    
    # using (date) works but results in poor model performances
    
    data = df, 
    
    formula = response ~ as.factor(year) + # AIC-based parsimonious model for sdmTMB with 1000 knots (3/23/2022)
      s(depth, k = 5) +
      # s(sst_d, k = 5) +
      # s(wind_d, k = 5) +
      # s(wind_m, k = 5) +
      # s(q05_chla_m, k = 5) +
      # s(sd_sst_m, k = 5) +
      # s(sd_chla_m, k = 5) +
      s(sd_wind_m, k = 5),
    
    silent = F, 
    
    # extra_time = missing_year,
    
    # reml = T,
    
    time = "year",
    
    # spde = rea_spde,
    mesh = rea_spde_coast,
    
    # anisotropy = T,
    
    family = tweedie(link = "log")
    # spatiotemporal = "IID",
    # control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
    
  ); beepr::beep(2)
  
  density_model_dynamic <- run_extra_optimization(density_model_dynamic, newton_loops = 0); beepr::beep(2)
  density_model_dynamic
  AIC(density_model_dynamic)
  max(density_model_dynamic$gradients)
  saveRDS(density_model_dynamic, paste0("outputs/density_model_dynamic_", n_knots[k], ".rds"))
  
  # full model
  density_model_full <- sdmTMB(
    
    # using (date) works but results in poor model performances
    
    data = df, 
    
    formula = response ~ as.factor(year) + 
      s(depth, k=5) +
      s(sst_d, k=5) +
      s(chla_8d, k=5) +
      s(wind_d, k=5) +
      s(sst_m, k=5) + 
      s(chla_m, k=5) + 
      s(wind_m, k=5) +
      s(q95_sst_m, k=5) +
      s(q95_chla_m, k=5) +
      s(q95_wind_m, k=5) +
      s(q05_sst_m, k=5) + 
      s(q05_chla_m, k=5) + 
      s(q05_wind_m, k=5) +
      s(sd_sst_m, k=5)+
      s(sd_chla_m, k=5) +
      s(sd_wind_m, k=5),
    
    silent = F, 
    
    # extra_time = missing_year,
    
    # reml = T,
    
    time = "year",
    
    # spde = rea_spde,
    mesh = rea_spde_coast,
    
    family = tweedie(link = "log")
    # spatiotemporal = "IID",
    # control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
    
  ); beepr::beep(2)
  
  density_model_full <- run_extra_optimization(density_model_full, newton_loops = 0); beepr::beep(2)
  density_model_full
  AIC(density_model_full)
  max(density_model_full$gradients)
  saveRDS(density_model_full, paste0("outputs/density_model_full_", n_knots[k], ".rds"))
  
  # presence absence model
  df$response_p = ifelse(df$response > 0, 1, 0)
  
  density_model_presence <- sdmTMB(
    
    # using (date) works but results in poor model performances
    
    data = df, 
    
    formula = response_p ~ as.factor(year) + 
      s(depth) +
      s(sst_d) +
      s(chla_8d) +
      s(wind_d) +
      s(sst_m) + 
      s(chla_m) + 
      s(wind_m) +
      s(q95_sst_m) +
      s(q95_chla_m) +
      s(q95_wind_m) +
      s(q05_sst_m) + 
      s(q05_chla_m) + 
      s(q05_wind_m) +
      s(sd_sst_m)+
      s(sd_chla_m) +
      s(sd_wind_m),    
    silent = F, 
    
    # extra_time = missing_year,
    
    # reml = T,
    
    time = "year",
    
    # spde = rea_spde,
    mesh = rea_spde_coast,
    
    # anisotropy = T,
    
    family = binomial(link = "logit")
    # spatiotemporal = "IID",
    # control = sdmTMBcontrol(step.min = 0.01, step.max = 1)

  ); beepr::beep(2)
  
  auc <- predict(density_model_presence, df, type="response")
  
  #calculate AUC
  library(pROC)
  auc(auc$response_p, auc$est)
  
  density_model_presence <- run_extra_optimization(density_model_presence, newton_loops = 0); beepr::beep(2)
  density_model_presence
  AIC(density_model_presence)
  max(density_model_presence$gradients)
  saveRDS(density_model_presence, paste0("outputs/density_model_presence_", n_knots[k], ".rds"))
  
  gc()
  
}
