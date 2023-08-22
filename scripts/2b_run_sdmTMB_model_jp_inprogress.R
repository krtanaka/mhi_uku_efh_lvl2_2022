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
library(mgcv)
library(viridis)
library(tidyterra)
library(ggrepel)
library(plotrix)

rm(list = ls())

select = dplyr::select

# load clean eds-enhanced ncrmp data. See 1_prep_ncrmp_data.R
load("data/clean_ncrmp_nSPC_abund_biom_GUAM_all_sp_matched_data.RData") # eds-enhanced ncrmp data w/ all species (n = 713)
#load("data/clean_ncrmp_nSPC_abund_size_marian_samoa_all_sp.RData") # eds-enhanced ncrmp data w/ all species (n = 713)
#load("data/clean_ncrmp_marian_select_sp.Rdata") # eds-enhanced ncrmp data w/ priority species (n = 29)

# plot distribution of survey sites
ggplot(data = df) + 
  geom_point(aes(x=lon,y=lat,colour=factor(obs_year))) +
  scale_colour_manual(values = topo.colors(5)) +
  theme_bw() + theme(panel.grid = element_blank()) +
  labs(x="Longitude", y="Latitude",colour="Year")

# calculate area needed to change abund.site to integer data (smallest number multiplied to 1):
min(subset(df, abund > 0)$abund)
# 1 / 0.0007073553 = 1413.717
df$abund_int = round(df$abund*1413.717)
#df$abund_100m = df$abund*100

# subset for updated 12 priority species (downsized list from the original 30- J. Brown)
priority_sp <- c("ACLI","NALI","CAME","CHUB","LUFU","CHFN","EPME","LEOL","SCSC","MOGR","BOMU","CHUN")
#

df <- df %>%
  subset(species %in% priority_sp)

sp_list = unique(df$species)
islands = unique(df$island)

# df %>%
#   ggplot(aes(depth, response)) +
#   geom_hex(bins = 100) +
#   geom_smooth()

# load the 10-m resolution Guam prediction grid from prep.grid.R. Add year 2022.
# source: NOAA/NGDC 10-m Bathymetry: Guam (https://pae-paha.pacioos.hawaii.edu/erddap/griddap/ngdc_bathy_10m_guam.html)
load("data/grid/guam_grid_10m.Rdata")
xy= distinct(grid[,c(1,2)])
#xy = distinct(xy)
load("data/grid/guam_grid_10m_matchedvars.Rdata")
grid = cbind(xy,grid_new)
rm(xy, grid_new)
grid$depth = grid$depth * -1
grid$obs_year = 2017
grid = grid[,-4]
grid = subset(grid, select = -c(rugosity))
grid = distinct(grid)
grid = grid[complete.cases(grid),]
# regrid output to 500m
grid = grid %>% 
  # mutate(X = round(X, 0),
  # Y = round(Y, 0)) %>%
  mutate(X = ceiling(X*2)/2,
         Y = ceiling(Y*2)/2) %>%
  group_by(X, Y, obs_year) %>%
  summarise(depth = mean(depth), chla = mean(chla))


# load MPA designations for prediction grid overlay
# try shape file
#reading in a shapefile with multiple polygons
library(rgdal)
mpa <-readOGR("data/guam_MPAs/gua_base_land_openwater_mpa_finalize.shp")
mpa <- spTransform(mpa, "+proj=utm +zone=55 +datum=WGS84 +units=km +no_defs ") # need to transform lat and long coordinates
mpa = subset(mpa, SEC_NAME != "GUA_WEST_OPEN" & SEC_NAME != "GUA_EAST_OPEN" & SEC_NAME != "GUA_LAND")
ggplot() + geom_polygon(data = mpa, aes(x=long, y=lat, group = group), color = "black", fill = "lightgrey")


for (s in 1:length(sp_list)) {
  # s = 1
  
  # pick the number of knots, more knots = higher resolutions but longer computaion time...
  #n_knots = c(1000, 500, 300, 100)[3]
  
    # subset data
  df_i = df %>% 
    subset(island == "Guam") %>%
    subset(species == sp_list[s]) #%>%
  #  subset(size_10cm != "(0,10]" & size_10cm != "(10,20]" & size_10cm != "(20,30]") %>%
  #  group_by(lat, lon, obs_year) %>% summarise(abund_int2 = mean(abund_int, na.rm=T))
  #df_guam = subset(df_guam, species=="CAME")
  #df_i = left_join(df_i, df_guam)
  #df_i$abund_int = round(df_i$abund*1413.717)
  
  # plot raw observed distributions
  df_i$abund_norm = (df_i$abund_int-min(df_i$abund_int))/(max(df_i$abund_int)-min(df_i$abund_int))
  ggplot() + 
    geom_point(df_i, mapping=aes(x=lon,y=lat,color=abund_norm)) +
    scale_color_viridis(option="D", breaks=c(0.2,0.4,0.6,0.8)) + 
    ggdark::dark_theme_minimal()

  # df_i = df_i[-which(df_i$response > quantile(df_i$response, probs = 0.999)),]
  
  if (nrow(subset(df_i, abund_int > 0)) / nrow(subset(df_i)) < 0.05) next
  
  # adjust depth column
  #df_i = df_i[!is.na(df_i$depth),] %>% 
  #  subset(depth < 0) %>% 
  #  mutate(depth = depth * -1)
  
  # create few more depth covariates (see Barnett et al. 2020)
  #df_i$depth_scaled = scale(log(df_i$depth))
  #df_i$depth_scaled2 = df_i$depth_scaled ^ 2
  #df_i$depth_mean = mean(df_i$depth)
  #df_i$depth_sd = sd(df_i$depth)
  
  # add utm coordinates
  zone <- (floor((df_i$lon[1] + 180)/6) %% 60) + 1
  xy_utm = as.data.frame(cbind(utm = project(as.matrix(df_i[, c("lon", "lat")]), paste0("+proj=utm +units=km +zone=", zone))))
  colnames(xy_utm) = c("X", "Y")
  df_i = cbind(df_i, xy_utm)
  # plot(xy_utm, pch = ".", bty = 'n')
  rm(xy_utm)
  
  # find smallest distance between survey areas in a given year to determine minimum cutoff size
  library(RANN)
  
  zone <- (floor((df$lon[1] + 180)/6) %% 60) + 1
  xy_utm = as.data.frame(cbind(utm = project(as.matrix(df[, c("lon", "lat")]), paste0("+proj=utm +units=km +zone=", zone))))
  colnames(xy_utm) = c("X", "Y")
  df = cbind(df, xy_utm)
  # plot(xy_utm, pch = ".", bty = 'n')
  rm(xy_utm)
  d=subset(df, obs_year == 2009)
  points = d[,c(1,2)]
  for(i in 1:length(points)) {
     point = points[i,]
     others = points[-i,]
     nearest = nn2(point, others)
       }
  
  # Read in Island Boundaries (i.e., land mass)
  load('data/gis/MHI_islands_shp.RData')
  crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
  ISL_this <- ISL_bounds %>%
    subset(ISLAND %in% "GUAM")
  ISL_this_utm = spTransform(ISL_this, CRS(paste0("+proj=utm +units=km +zone=", zone)))
  ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=", zone))
  
  #rea_spde <- make_mesh(df_i, c("X", "Y"), n_knots = n_knots, type = "cutoff_search", seed = 2022); gc() # let INLA figure out
  # rea_spde <- make_mesh(df, c("X", "Y"), cutoff  = 15, type = "cutoff") # predefined cutoff 
  # Use the cutoff value to avoid the long search
  rea_spde <- make_mesh(df_i, c("X", "Y"), cutoff = 0.1, seed = 2023); gc() # let INLA figure out
  # build barrier to mesh
  rea_spde_coast = add_barrier_mesh(rea_spde , ISL_this_sf)
  
  # png(paste0("outputs/SPDE_mesh_field_", n_knots[k], "_knots.png"), units = "in", height = 10, width = 10, res = 500)
   plot(rea_spde_coast$mesh, asp = 1, main = ""); axis(1); axis(2)
   plot(ISL_this_utm, add = TRUE)
   points(rea_spde_coast$loc_xy,col = "green", pch = ".", cex = 5)
   bar_i = rea_spde_coast$barrier_triangles
   norm_i = rea_spde_coast$normal_triangles
   points(rea_spde_coast$spde$mesh$loc[,1], rea_spde_coast$spde$mesh$loc[,2], pch = 20, col = "black")
   points(rea_spde_coast$mesh_sf$V1[bar_i], rea_spde_coast$mesh_sf$V2[bar_i], col = "red", pch = 20)
   points(rea_spde_coast$mesh_sf$V1[norm_i], rea_spde_coast$mesh_sf$V2[norm_i], col = "blue", pch = 20)
  # dev.off()
  
  rm(ISL_bounds, ISL_this, ISL_this_sf, ISL_this_utm, rea_spde, zone)
  
  obs_year = unique(df_i$obs_year)
  full_year = seq(min(df_i$obs_year), max(df_i$obs_year), by = 1)
  missing_year = setdiff(full_year, obs_year)
  
  missing_year = as.integer(missing_year);missing_year
  
  # a basic model for index standardization. 
  # you will likely want to include 0 + as.factor(year) or -1 + as.factor(year) 
  # so that there is a factor predictor that represents the mean estimate for each time slice.
  m <- sdmTMB(
    
    data = df_i, 
    formula = abund_int ~ 0 + as.factor(obs_year) + s(depth, k=3),
    # formula = response ~ as.factor(year) + s(depth, k = 3),
    
    time = NULL, 
    # silent = F,
    mesh = rea_spde_coast, 
    # spatiotemporal = c("iid", "ar1", "rw", "off")[4],
    # spatial = c("on", "off")[2],
    # control = sdmTMBcontrol(step.min = 0.001, step.max = 1),
    family = tweedie(link = "log"))

  # Run extra optimization
  m <- run_extra_optimization(m, newton_loops = 0); beepr::beep(2)
  
  #df_i$PA = ifelse(df_i$response > 0, 1, 0)
  
  # create new dataframe with complete  cases for variables of interest:
  df_i2 = df_i %>% 
    select(lon, lat, X, Y, obs_year, depth, abund_int, est_pop_density, rugosity,
           mean_Wind_Speed_ASCAT_daily_DY01, mean_Chlorophyll_A_ESA_OC_CCI_8days_DY01,
           mean_Kd490_ESA_OC_CCI_8days_DY01, mean_SST_CRW_CoralTemp_daily_DY01) %>%
    #mutate(wave_exp = round(wave_exp)) %>%
    filter(complete.cases(.))

  df_i2 = df_i %>% 
    select(lon, lat, X, Y, obs_year, depth, sec_name, abund_int, mean_Chlorophyll_A_ESA_OC_CCI_8days_DY01) %>%
    #mutate(wave_exp = round(wave_exp)) %>%
    filter(complete.cases(.))
  #df_i2 = df_i2[complete.cases(df_i2),]
  #source("scripts/vif.R")
  #vif <- df_i2 %>% select(obs_year, depth, abund_int, est_pop_density, rugosity, wave_exp, mean_Kd490_ESA_OC_CCI_8days_DY01)
  #vif_func(in_frame = vif, thresh = 3, trace = T) 
  
  # run basic gam() to see which variables are not significant and could be removed
  #m2a = gam(abund_int ~ 0 + as.factor(obs_year) + s(depth, k=3) + s(est_pop_density, k=4) +
  #            te(lon,lat), data = df_i2, family = tw(link = "log"))
  #summary(m2a)
  #plot(m2a)
  #termplot(m2a)
  
  # Read in Island Boundaries (i.e., land mass)
    zone <- (floor((df_i2$lon[1] + 180)/6) %% 60) + 1
    load('data/gis/MHI_islands_shp.RData')
    crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    #ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
    ISL_this <- ISL_bounds %>%
      subset(ISLAND %in% "GUAM")
    ISL_this_utm = spTransform(ISL_this, CRS(paste0("+proj=utm +units=km +zone=", zone)))
    ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=", zone))
    #rea_spde <- make_mesh(df_i2, c("X", "Y"), n_knots = n_knots, type = "cutoff_search", seed = 2022); gc() # let INLA figure out
    rea_spde <- make_mesh(df_i2, c("X", "Y"), cutoff = 0.1, seed = 2023); gc() # let INLA figure out
    rea_spde_coast = add_barrier_mesh(rea_spde , ISL_this_sf)
    rm(ISL_bounds, ISL_this, ISL_this_sf, ISL_this_utm, rea_spde, zone)

  # run sdmTMB model with all variables
  #df_i2$mean_Chlorophyll_A_ESA_OC_CCI_8days_DY01_log = log(df_i2$mean_Chlorophyll_A_ESA_OC_CCI_8days_DY01)
  m2 <- sdmTMB(
    
    data = df_i2, 
    formula = abund_int ~ 0 + as.factor(obs_year) + s(depth, k=3) +
      #s(est_pop_density, k=4) + 
      #s(rugosity, k=4) +
      #s(mean_Wind_Speed_ASCAT_daily_DY01, k=4) +
      s(mean_Chlorophyll_A_ESA_OC_CCI_8days_DY01, k=4),# +
      #s(mean_Kd490_ESA_OC_CCI_8days_DY01, k=4) +
      #s(mean_SST_CRW_CoralTemp_daily_DY01),# +
      #s(wave_exp, k=4),
      #formula = response ~ as.factor(year) + s(depth, k = 3),
    
    #time = "year", 
    # silent = F,
    mesh = rea_spde_coast, 
    # spatiotemporal = c("iid", "ar1", "rw", "off")[4],
    # spatial = c("on", "off")[2],
    # control = sdmTMBcontrol(step.min = 0.001, step.max = 1),
    family = tweedie(link = "log"))
  
   
  # # Spatially varying coefficient of year
  # df_i$year_scaled <- scale(as.numeric(df_i$year))
  # m <- sdmTMB(
  #   response ~ year_scaled + depth_scaled + depth_scaled2,
  #   spatial_varying = ~ 0 + year_scaled, #<
  #   data = df_i,
  #   # silent = F,
  #   mesh = rea_spde_coast,
  #   family = tweedie(),
  #   time = "year"
  # )
  
  # # IID random intercepts by year. Good for strong year effect?
  # df_i$fyear <- as.factor(df_i$year)
  # m <- sdmTMB(
  #   response ~ (1 | fyear) + depth_scaled + depth_scaled2,
  #   data = df_i,
  #   # silent = F,
  #   mesh = rea_spde_coast,
  #   family = tweedie(link = "log")
  # )
  
  # Run extra optimization
  m2 <- run_extra_optimization(m2, newton_loops = 0); beepr::beep(2)
  
  # Visualize effect: (see ?visreg_delta)
  visreg::visreg(m, xvar = "depth", scale = "response")
  visreg::visreg(m2, xvar = "mean_Chlorophyll_A_ESA_OC_CCI_8days_DY01", scale = "response")

  # check gradients and AIC
  max(m$gradients)
  max(m2$gradients)
  AIC(m)
  AIC(m2)
  
  # Extract coefficients:
  tidy(m2, conf.int = TRUE)
  tidy(m, effects = "ran_par", conf.int = TRUE)
  
  # Perform several 'sanity' checks:
  sanity(m)
  
  df_i$resids <- residuals(m) # randomized quantile residuals
  qqnorm(df_i$resids, pch = 20)
  qqline(df_i$resids)
  
  df_i2$resids <- residuals(m2) # randomized quantile residuals
  qqnorm(df_i2$resids, pch = 20)
  qqline(df_i2$resids)
  
  ggplot() + 
    geom_point(df_i, mapping=aes(x=lon,y=lat,color=resids)) +
    scale_color_gradient2(low = "red", mid = "white", high = "blue") + 
    ggdark::dark_theme_minimal()
  
  ggplot() + 
    geom_point(df_i2, mapping=aes(x=lon,y=lat,color=resids)) +
    scale_color_gradient2(low = "red", mid = "white", high = "blue") + 
    ggdark::dark_theme_minimal()
  
  
  # r <- residuals(m, "mle-mcmc", mcmc_warmup = 100, mcmc_iter = 101)
  # qqnorm(r)
  # qqline(r)
  
  # Predict on new data:
  #grid$fyear <- as.factor(grid$year)
  #grid$year_scaled <- scale(as.numeric(grid$year))
  #grid$depth_scaled = scale(log(grid$depth))
  #grid$depth_scaled2 = grid$depth_scaled ^ 2
  
  # predict model over Guam grid, convert back to m-2, and create normalized response (0-1 scale)
  grid$mean_Chlorophyll_A_ESA_OC_CCI_8days_DY01 = grid$chla
  p <-  predict(m2, newdata = grid, type = "response")
  p$est_m2 = p$est / 1413.72
  p$est_norm = (p$est_m2-min(p$est_m2))/(max(p$est_m2)-min(p$est_m2))
  
  # Calculate R^2 for sdmTMB model
  predicted = data.frame(predict(m, df_i[,c(1:13,755:759)], type = "response"))
  #                  data.frame(predict(m2, df_i[,c(755,756,13,6,751,752)], type = "response")))
  #predicted$est = exp(predicted[,10])
  #predicted$est2 = exp(predicted[,20])/1+exp(predicted[,20])
  names(predicted)[19] <- "Predicted"
  #names(predicted)[21] <- "Predicted_PA"
  names(predicted)[15] <- "Observed"
  rsquared = cor(predicted$Predicted, predicted$Observed)^2; rsquared # R-squared
  adjrsquared = 1 - (((1 - rsquared)*(298-1))/(298-3-1)); adjrsquared # Adjusted R-squared
  
  plot(predicted$Observed, predicted$Predicted, pch = 19)#, xlim = c(0,4), ylim = c(0,4), xlab="Observed", ylab="Predicted")
  abline(0,1, lty = "dashed", lwd=1.3, col = "darkgray")
  
  # Calculate R^2 for m2 sdmTMB model
  predicted = data.frame(predict(m2, df_i2, type = "response"))
  names(predicted)[10] <- "Predicted"
  names(predicted)[8] <- "Observed"
  rsquared = cor(predicted$Predicted, predicted$Observed)^2; rsquared # R-squared
  adjrsquared = 1 - (((1 - rsquared)*(312-1))/(312-7-1)); adjrsquared # Adjusted R-squared
  
  plot(predicted$Observed, predicted$Predicted, pch = 19, xlim = c(0,50), ylim = c(0,50), xlab="Observed", ylab="Predicted")
  abline(0,1, lty = "dashed", lwd=1.3, col = "darkgray")
  
  # plot mean abundance by island sector for observed & modelled
  predicted %>%
    group_by(sec_name) %>%
    summarise(abund_mean = mean(Observed, na.rm=T),
              abund_se = std.error(Observed, na.rm=T),
              m_abund_mean = mean(Predicted, na.rm=T),
              m_abund_se = std.error(Predicted, na.rm=T)) %>%
    ggplot(predicted,mapping=aes(x=sec_name, y=abund_mean, colour = "Observed")) + 
    geom_point() + 
    geom_errorbar(mapping=aes(x=sec_name, ymin=abund_mean-abund_se, ymax=abund_mean+abund_se), color="black", width=0.15) +
    geom_point(aes(x=sec_name, y=m_abund_mean, colour = "Predicted"), position=position_nudge(x=0.2)) +
    geom_errorbar(mapping=aes(x=sec_name, ymin=m_abund_mean-m_abund_se, ymax=m_abund_mean+m_abund_se), color="blue", width=0.15, position=position_nudge(x=0.2)) +
    theme_bw() + theme(panel.grid = element_blank()) +
    scale_color_manual(name = "", values = c("Observed" = "black", "Predicted" = "blue")) +
    xlab("") + ylab("Abundance (Ind. per ~1414 m2)") + ggtitle("M. grandoculus") +
    scale_x_discrete(labels = c("Achang","East","Harbor","Pati Point","Piti Bomb","Tumon","West"))
  
  
  png(paste0("outputs/abund_int_map_", sp_list[s], ".png"), height = 8, width = 7, res = 500, units = "in")

  print(p %>% 
          mutate(X = round(X, 1),
                 Y = round(Y, 1)) %>%
          group_by(X, Y, obs_year) %>% 
          summarise(est_norm = mean(est_norm)) %>% 
          ggplot() +
          geom_raster(aes(X, Y, fill = est_norm)) +
          geom_polygon(data = mpa, aes(x=long, y=lat, group = group), linewidth=0.4, color = "white", fill = NA) +
          coord_fixed() +
          scale_fill_viridis(option = "D", "Norm. Abundance \n(ind.m2)", breaks = c(0.2,0.4,0.6,0.8)) +
          labs(x = "Eastings (km)", y = "Northings (km)", title = paste0(unique(df_i$sci), " Prediction"),
                    subtitle = paste0("r-squared = ", round(rsquared,2))) + 
          
               #     subtitle = paste0("Max. estimated abundance = ", round(max(p$est_norm), 2), " ind./m2 \n r-squared = ", round(rsquared,2))) + 
          #facet_wrap(~obs_year) +
          #ggtitle(paste0(unique(df_i$sci), ": Prediction (fixed \n effects  + all random effects)"),
          #        subtitle = paste0("Maximum estimated biomass density = \n ", round(max(exp(p$est)), 2), " g/m2; r-squared = ", round(rsquared,2))
          #) + 
          ggdark::dark_theme_minimal())
  
  dev.off()
  
  saveRDS(m, paste0("outputs/abund_int_index_model_", n_knots, "_knots_", sp_list[s], ".rds"))
  
  ## extract biomass index
  # p <-  predict(m, newdata = grid, return_tmb_object = T)
  # index <- get_index(p, area = rep(4, nrow(grid)))
  # 
  # ggplot(index, aes(year, est, group = 1)) +
  #   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
  #   geom_line() +
  #   geom_point(aes(fill = est), shape = 21, size = 5) +
  #   labs(x = "Year", y = "Biomass (g)") +
  #   scale_fill_gradientn(
  #     colors = matlab.like(100),"") +
  #   ggdark::dark_theme_minimal()
  # 
  # cog <- get_cog(p, format = "wide")
  # ggplot(cog, aes(est_x, est_y, colour = year)) +
  #   geom_pointrange(aes(xmin = lwr_x, xmax = upr_x)) +
  #   geom_pointrange(aes(ymin = lwr_y, ymax = upr_y)) +
  #   scale_colour_viridis_d()
  # 
  # saveRDS(p, paste0("outputs/biomass_index_prediction_", n_knots, "_knots_", sp_list[s], ".rds"))

  gc()
  
}
