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
ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=", zone))
rm(ISL_this, ISL_this_utm, ISL_bounds)

# rea_spde = make_mesh(df, c("X", "Y"), n_knots = 100, type = "cutoff_search") # search
# rea_spde = make_mesh(df, c("X", "Y"), cutoff = 15, type = "cutoff") # predefined
rea_spde = make_mesh(df, c("X", "Y"), cutoff = 15, type = "cutoff", seed = 2021) # predefined

#build barrier to mesh
rea_spde_coast = add_barrier_mesh(rea_spde , ISL_this_sf)
rm(rea_spde, zone)

# only + depth
m_cv1 <- sdmTMB_cv(
  
  data = df, 
  
  formula = response ~ as.factor(year) + s(depth, k=5),
  
  silent = F, 
  
  time = "year",
  
  spde = rea_spde_coast,
  
  family = tweedie(link = "log"),
  k_folds = 5
  
); beepr::beep(2)

saveRDS(m_cv1, "outputs/fit_depth.rds")

# full model
m_cv2 <- sdmTMB_cv(
  
  data = df, 
  
  formula = response ~ as.factor(year) + s(depth, k=5) + 
    s(sst_d, k=5) + s(chla_8d, k=5) + s(wind_d, k=5) + 
    s(sst_m, k=5) + s(chla_m, k=5) + s(wind_m, k=5) + 
    s(q95_sst_m, k=5) + s(q95_chla_m, k=5) + s(q95_wind_m, k=5) +
    s(q05_sst_m, k=5) + s(q05_chla_m, k=5) + s(q05_wind_m, k=5) + 
    s(sd_sst_m, k=5)+ s(sd_chla_m, k=5) + s(sd_wind_m, k=5),
  
  # control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
  # priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
  
  silent = F, 
  
  time = "year",
  
  spde = rea_spde_coast,
  
  family = tweedie(link = "log"),
  k_folds = 5
  
); beepr::beep(2)

saveRDS(m_cv2, "outputs/fit_full.rds")

source("scripts/vif.R")

vif <- df[, c("sst_d", "chla_8d", "wind_d",
              "sst_m", "chla_m", "wind_m",
              "q95_sst_m", "q95_chla_m", "q95_wind_m", 
              "q05_sst_m", "q05_chla_m", "q05_wind_m", 
              "sd_sst_m", "sd_chla_m", "sd_wind_m")]

vif_func(in_frame = vif, thresh = 3, trace = T) 

# VIF model
m_cv3 <- sdmTMB_cv(
  
  data = df, 
  
  formula = response ~ as.factor(year) + s(depth, k=5) + 
    s(sst_d, k=5) + s(wind_d, k=5) + 
    s(wind_m, k=5) + 
    s(q05_chla_m, k=5) + 
    s(sd_sst_m, k=5)+ s(sd_chla_m, k=5) + s(sd_wind_m, k=5),
  
  # control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
  # priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5)),
  
  silent = F, 
  
  time = "year",
  
  spde = rea_spde_coast,
  
  family = tweedie(link = "log"),
  k_folds = 5
  
); beepr::beep(2)

saveRDS(m_cv3, "outputs/fit_vif.rds")

m_cv1 = readRDS("outputs/fit_depth.rds")
m_cv2 = readRDS("outputs/fit_full.rds")
m_cv3 = readRDS("outputs/fit_vif.rds")

AIC_pot_big = NULL

for (m in 1:3) {
  
  if (m == 1) model = m_cv1
  if (m == 2) model = m_cv2
  if (m == 3) model = m_cv3
  
  AIC_pot_small = NULL
  
  for (mi in 1:5) {
    
    aic = AIC(model$models[[mi]])
    
    AIC_pot_small = rbind(AIC_pot_small, aic)
    
  }
  
  AIC_pot_small = colMeans(AIC_pot_small)
  
  AIC_pot_small$model = paste0("m_cv", m)
  
  AIC_pot_big = rbind(AIC_pot_big, AIC_pot_small)
  
}
  
m_cv1$sum_loglik
m_cv2$sum_loglik
m_cv3$sum_loglik

m_cv1$fold_elpd
m_cv2$fold_elpd
m_cv3$fold_elpd

m_cv1$elpd
m_cv2$elpd
m_cv3$elpd

m_cv1$fold_loglik
m_cv2$fold_loglik
m_cv3$fold_loglik

m_cv1$sum_loglik
m_cv2$sum_loglik
m_cv3$sum_loglik


head(m_cv1$data)
m_cv1$models[[1]]
m_cv1$max_gradients
