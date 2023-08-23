library(sdmTMB)
library(dplyr)
library(ggplot2)
library(rgdal)
library(colorRamps)
library(patchwork)
library(raster)
library(sf)

rm(list = ls())

# for Uku: 
# Total numerical density estimates (individuals per 100 m2) were obtained by dividing fish counts in each survey by the survey area (353 m2 from two 15-m diameter survey cylinders) and multiplying by 100. - Nadon et al. 2020

region = "MHI"
uku_or_not = T

# ## top 5 taxa by abundance or biomass
# load("data/rea/ALL_REA_FISH_RAW.rdata")
# df %>%
#   subset(REGION == region) %>%
#   subset(OBS_YEAR >= 2010) %>% 
#   group_by(TAXONNAME) %>%
#   # summarise(n = sum(BIOMASS_G_M2, na.rm = T)) %>%
#   summarise(n = sum(COUNT, na.rm = T)) %>%
#   mutate(freq = n/sum(n)) %>%
#   arrange(desc(freq)) %>%
#   top_n(5)

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
            "Hawaii")#[1]

response_variable = "fish_count"; sp = ifelse(uku_or_not == T, "Aprion virescens", "Chromis vanderbilti")

if (response_variable == "fish_count") {
  
  load("data/rea/ALL_REA_FISH_RAW_SST.RData")
  df[df == -9991] <- NA
  
  df = df %>% 
    subset(REGION == region & ISLAND %in% islands) %>%
    subset(OBS_YEAR >= 2010) %>% 
    subset(TRAINING_YN == 0) %>%
    # mutate(response = ifelse(TAXONNAME == sp, COUNT*100, 0)) %>%
    mutate(response = ifelse(TAXONNAME == sp, COUNT, 0)) %>%
    group_by(LONGITUDE, LATITUDE, ISLAND, OBS_YEAR) %>% 
    summarise(response = sum(response, na.rm = T),
              
              sst_d_mean = mean(mean_SST_CRW_Daily_DY01, na.rm = T),
              sst_w_mean = mean(mean_SST_CRW_Daily_WK01, na.rm = T),
              sst_m_mean = mean(mean_SST_CRW_Daily_MO01, na.rm = T),
              
              sst_w_sd = mean(sd_SST_CRW_Daily_WK01, na.rm = T),
              sst_m_sd = mean(sd_SST_CRW_Daily_MO01, na.rm = T),
              
              sst_w_q05 = mean(q05_SST_CRW_Daily_WK01, na.rm = T),
              sst_m_q05 = mean(q05_SST_CRW_Daily_MO01, na.rm = T),
              
              sst_w_q95 = mean(q95_SST_CRW_Daily_WK01, na.rm = T),
              sst_m_q95 = mean(q95_SST_CRW_Daily_MO01, na.rm = T),
              
              depth = mean(DEPTH, na.rm = T)) %>% 
    subset(response < quantile(response, prob = 0.999))
  
  df %>% ggplot(aes(response)) + geom_histogram() + 
    df %>% group_by(OBS_YEAR) %>% summarise(n = mean(response)) %>% ggplot(aes(OBS_YEAR, n)) + geom_line()
  
  (survey_map = df %>% 
      group_by(LONGITUDE, LATITUDE) %>% 
      summarise(n = mean(response)) %>% 
      ggplot(aes(LONGITUDE, LATITUDE, color = n)) + 
      geom_point(size = 3, alpha = 0.5) + 
      coord_fixed() + 
      scale_color_viridis_b() + 
      ggpubr::theme_pubr() + 
      ggtitle("Survey_based trend"))
  
  (survey_trend = df %>% 
      group_by(OBS_YEAR) %>% 
      summarise(n = mean(response)) %>% 
      ggplot(aes(OBS_YEAR, n)) + 
      geom_point(size = 2) + 
      ylab("Mean abundance (n) per sq.m") + 
      xlab("Year") + 
      geom_line() + 
      ggpubr::theme_pubr() + 
      ggtitle("Survey_based trend"))
  
}

zone <- (floor((df$LONGITUDE[1] + 180)/6) %% 60) + 1
xy_utm = as.data.frame(cbind(utm = project(as.matrix(df[, c("LONGITUDE", "LATITUDE")]), paste0("+proj=utm +units=km +zone=", zone))))
colnames(xy_utm) = c("X", "Y")
df = cbind(df, xy_utm)

# Read in Island Boundaries
load('data/MHI_islands_shp.RData')
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
ISL_this_utm = spTransform(ISL_this,CRS(paste0("+proj=utm +units=km +zone=",zone)))
ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=",zone))

n_knots = 500
n_knots = 300 
n_knots = 100 

rea_spde <- make_mesh(df, c("X", "Y"), n_knots = n_knots, type = "cutoff_search") # search
# rea_spde <- make_mesh(df, c("X", "Y"), cutoff  = n_knots, type = "cutoff") # predefined

#build barrier to mesh
rea_spde_coast = add_barrier_mesh(rea_spde , ISL_this_sf)

# png(paste0("outputs/SPDE_mesh_field_", n_knots, ".png"), units = "in", height = 8, width = 8, res = 500)
# par(mfrow = c(1,2), pty = 's')
# plot(xy_utm, pch = ".", bty = 'n')
plot(rea_spde_coast$mesh, asp = 1, pch = ".", main = ""); axis(1); axis(2)
plot(ISL_this_utm, add = TRUE)
points(rea_spde_coast$loc_xy,col = "green", pch = ".", cex = 5)
bar_i = rea_spde_coast$barrier_triangles
norm_i = rea_spde_coast$normal_triangles
points(rea_spde_coast$spde$mesh$loc[,1], rea_spde_coast$spde$mesh$loc[,2], pch = ".", col = "black")
points(rea_spde_coast$mesh_sf$V1[bar_i], rea_spde_coast$mesh_sf$V2[bar_i], col = "red", pch = 4)
points(rea_spde_coast$mesh_sf$V1[norm_i], rea_spde_coast$mesh_sf$V2[norm_i], col = "blue", pch = 4)
# dev.off()

df$year = df$OBS_YEAR
df$depth_scaled = scale(log(df$depth))
df$depth_scaled2 = df$depth_scaled ^ 2

plot(df$depth, df$response, pch = 20, bty = "n")
plot(df$sst_d_mean, df$response, pch = 20, bty = "n")

obs_year = unique(df$year)
full_year = seq(min(df$year), max(df$year), by = 1)
missing_year = setdiff(full_year, obs_year)
missing_year = as.integer(missing_year);missing_year

density_model <- sdmTMB(
  
  data = df, 
  
  # formula = response ~ as.factor(year) + depth_scaled + depth_scaled2,
  # formula = response ~ as.factor(year) + s(depth, k = 5),
  # formula = response ~ as.factor(year) + s(depth, k=5) + s(temp, k=5),
  formula = response ~ as.factor(year) + 
    
    s(sst_d_mean, k=5) + 
    s(sst_w_mean, k=5) + 
    s(sst_m_mean, k=5) + 
    
    s(sst_w_sd, k=5) + 
    s(sst_m_sd, k=5) + 
    
    s(sst_w_q05, k=5) + 
    s(sst_m_q05, k=5) + 
    
    s(sst_w_q95, k=5) + 
    s(sst_m_q95, k=5) + 
    s(depth, k=5),
  
  silent = F, 
  # extra_time = missing_year,
  spatial_trend = T, 
  spatial_only = F, 
  time = "year", 
  spde = rea_spde, 
  anisotropy = T,
  family = tweedie(link = "log"),
  # family = poisson(link = "log"),
  # family = binomial(link = "logit"), weights = n,
  # family = nbinom2(link = "log"),
  # family = Beta(link = "logit"),
  
  control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
  
); beepr::beep(2)

density_model <- run_extra_optimization(density_model); beepr::beep(2)

# look at gradients
max(density_model$gradients)

df$residuals <- residuals(density_model)
par(pty = "s")
png(paste0('outputs/qq_', sp, '.png'), width = 5, height = 5.5, units = "in", res = 500)
qqnorm(df$residuals, ylim = c(-5, 5), xlim = c(-5, 5), bty = "n", pch = 20, main = sp); abline(a = 0, b = 1)
dev.off()

m_p <- predict(density_model); m_p = m_p[,c("response", "est")]
m_p$back_abs_res = abs(df$residuals)

(p1 = ggplot(df, aes_string("X", "Y", color = "residuals")) +
    geom_point(alpha = 0.8, size = round(abs(df$residuals), digits = 0)) + 
    # facet_wrap(.~ISLAND, scales = "free", ncol = 3) +
    xlab("Eastings (km)") +
    ylab("Northings (km)") + 
    coord_fixed() +
    scale_color_gradient2() + 
    theme_minimal())

(p2 = m_p  %>% 
    ggplot(aes(response, exp(est))) + 
    geom_point(alpha = 0.2, aes(size = back_abs_res), show.legend = F) + 
    coord_fixed(ratio = 1) +
    ylab("Prediction") + 
    xlab("Observation") + 
    geom_abline(intercept = 0, slope = 1) +
    geom_smooth(method = "lm", se = T) + 
    ggpubr::theme_pubr())

png(paste0('outputs/residuals_', sp, '.png'), width = 8, height = 8, units = "in", res = 500)
print(p1)
dev.off()

png(paste0('outputs/pred_obs_', sp, '.png'), width = 5, height = 5, units = "in", res = 500)
print(p2)
dev.off()

#  extract some parameter estimates
sd <- as.data.frame(summary(TMB::sdreport(density_model$tmb_obj)))
r <- density_model$tmb_obj$report()
r

# prediction onto new data grid
load("data/crm/Topography_NOAA_CRM_vol10.RData") # bathymetry 
load("data/crm/Topography_NOAA_CRM_vol10_SST_CRW_Monthly.RData") # bathymetry with monthly SST
# topo$x = ifelse(topo$x > 180, topo$x - 360, topo$x)

grid = topo
grid <- topo %>% subset(x > range(pretty(df$LONGITUDE))[1] 
                        & x < range(pretty(df$LONGITUDE))[2] 
                        & y > range(pretty(df$LATITUDE))[1] 
                        & y < range(pretty(df$LATITUDE))[2])
res = 2
grid$longitude = round(grid$x, digits = res)
grid$latitude = round(grid$y, digits = res)

grid$longitude = grid$x
grid$latitude = grid$y

zone <- (floor((grid$longitude[1] + 180)/6) %% 60) + 1
xy_utm = as.data.frame(cbind(utm = project(as.matrix(grid[, c("longitude", "latitude")]),
                                           paste0("+proj=utm +units=km +zone=", zone))))

colnames(xy_utm) = c("X", "Y"); plot(xy_utm, pch = ".")

grid = cbind(grid, xy_utm)

grid_year = NULL

years = sort(as.vector(unique(df$year)))

# aggregating SST annually
# for (y in 1:length(years)) {
#   
#   # y = 1
#   
#   grid_year_sst = grid %>% select(contains(as.character(years[[y]])))
#   
#   grid_y = NULL
#   
#   grid_depth = grid[,3]
#   grid_xy = grid[,442:443]
#   
#   for (m in 1:12) {
#     
#     grid_m = cbind(grid_xy, grid_depth, grid_year_sst[,m])
#     colnames(grid_m)[4] = "temp"
#     grid_y = rbind(grid_y, grid_m)
#     
#   }
#   
#   grid_y = grid_y %>% 
#     group_by(X, Y) %>% 
#     summarise(temp = mean(temp), 
#               depth = mean(grid_depth)*-1)
#   
#   grid_y$year = years[[y]]
#   
#   grid_year = rbind(grid_year, grid_y)
#   
#   rm(grid_depth, grid_xy, grid_y, grid_year_sst, grid_m)
#   
# }

# # aggregating SST at each month-year time step, e.g., 03/1985 - 03/2021
# for (t in 1:435) {
# 
#   # t = 1
# 
#   grid_t = grid[,c(442, 443, t+3)]
#   grid_t$time = colnames(grid_t)[3]
#   colnames(grid_t)[3] = "temp"
# 
# 
#   grid_year = rbind(grid_year, grid_t)
#   print(t/435)
# 
# }
# 
# grid_year$year = substr(grid_year$time, 1, 4)
# grid_year = grid_year %>% subset(year %in% years)

# only depth covariate
for (y in 1:length(years)) {
  
  # y = 1
  
  grid_y = grid[,c("X", "Y", "Topography")]
  colnames(grid_y)[3] = "depth"
  grid_y$depth = grid_y$depth *-1
  grid_y$year = years[[y]]
  grid_year = rbind(grid_year, grid_y)
  
}

grid_year$depth_scaled = scale(log(grid_year$depth+0.0001))
grid_year$depth_scaled2 = grid_year$depth_scaled ^ 2

grid_year_missing = NULL

for (y in 1:length(missing_year)) {
  
  # y = 1
  
  # Add missing years to our grid:
  grid_from_missing_yr <- grid_year[grid_year$year ==  missing_year[[y]]-1, ]
  grid_from_missing_yr$year <-  missing_year[[y]] # `L` because `year` is an integer in the data
  grid_year_missing <- rbind(grid_year_missing, grid_from_missing_yr)
  
}

# grid_year = rbind(grid_year, grid_year_missing)

# set the area argument to 0.0081 km2 since our grid cells are 90 m x 90 m = 0.0081 square kilometers
p <- predict(density_model, 
             newdata = grid_year, 
             return_tmb_object = T, area = 0.0081)

p$data$sp = sp
sdm_output = p$data

save(sdm_output, file = paste0("outputs/density_results_", sp, "_", response_variable, "_", n_knots, "_", region, ".RData"))
save(paste0("outputs/density_results_", sp, "_", response_variable, "_", n_knots, "_", region, ".RData"))

plot_map_raster <- function(dat, column = "est") {
  
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_tile(aes(height = 0.5, width = 0.5), alpha = 0.8) +
    # geom_raster() +
    facet_wrap(~year) +
    coord_fixed() +
    xlab("Eastings (km)") +
    ylab("Northings (km)") + 
    scale_fill_gradientn(colours = matlab.like(100), "") +
    ggdark::dark_theme_minimal()
  
}

# pick out a single year to plot since they should all be the same for the slopes. Note that these are in log space.
plot_map_raster(filter(p$data, year == unique(p$data$year)[1]), "zeta_s")
plot_map_raster(p$data, "est") + ggtitle("Predicted density (g/sq.m) (fixed effects + all random effects)")
plot_map_raster(p$data, "exp(est_non_rf)") + ggtitle("Prediction (fixed effects only)")
plot_map_raster(p$data, "omega_s") + ggtitle("Spatial random effects only")
plot_map_raster(p$data, "epsilon_st") + ggtitle("Spatiotemporal random effects only")

# look at just the spatiotemporal random effects:
plot_map_raster(p$data, "est_rf") + scale_fill_gradient2()

(trend = plot_map_raster(filter(p$data, year == unique(p$data$year)[1]), "zeta_s") + ggtitle("Linear trend"))

(density_map_year = p$data %>% 
    # group_by(X, Y) %>% 
    # summarise(est = mean(est)) %>% 
    ggplot(aes_string("X", "Y", fill = "exp(est)")) +
    geom_tile(aes(height = 1, width = 1)) +
    # geom_point(alpha = 0.5) +
    facet_wrap(~year) +
    coord_fixed() +
    xlab("Eastings (km)") +
    ylab("Northings (km)") + 
    # scale_fill_gradientn(colours = matlab.like(100), "g / m^2") +
    scale_fill_gradientn(colours = matlab.like(100), "n/sq.m") +
    # scale_color_gradientn(colours = matlab.like(100), "# per 353 m^2") +
    ggtitle(paste0(sp, " predicted density 2015-2019")) + 
    ggdark::dark_theme_minimal() +
    theme(legend.position = "right"))

png(paste0('outputs/', sp, '_density_year.png'), width = 8, height = 8, units = "in", res = 500)
print(density_map_year)
dev.off()

(density_map_trend = p$data %>% 
    group_by(X, Y) %>%
    summarise(zeta_s = mean(zeta_s)) %>%
    ggplot(aes_string("X", "Y", fill = "zeta_s")) +
    geom_tile(aes(height = 1, width = 1)) +
    coord_fixed() +
    xlab("Eastings (km)") +
    ylab("Northings (km)") + 
    scale_fill_gradientn(colours = matlab.like(100), "linear trend") +
    # scale_color_gradientn(colours = matlab.like(100), "# per 353 m^2") +
    ggtitle(paste0(sp, " localized trend 2015-2019")) + 
    ggdark::dark_theme_minimal() +
    theme(legend.position = c(0.1, 0.2)))

png(paste0('outputs/', sp, '_density_trend.png'), width = 8, height = 5.2, units = "in", res = 500)
print(density_map_trend)
dev.off()

(density_map_mean = p$data %>%
    group_by(X, Y) %>%
    summarise(est = mean(est)) %>%
    ggplot(aes_string("X", "Y", fill = "exp(est)")) +
    geom_tile(aes(height = 1, width = 1)) +
    # geom_point(alpha = 0.5) +
    # facet_wrap(~year) +
    coord_fixed() +
    xlab("Eastings (km)") +
    ylab("Northings (km)") + 
    # scale_fill_gradientn(colours = matlab.like(100), "g / m^2") +
    scale_fill_gradientn(colours = matlab.like(100), "n/sq.m") +
    # scale_color_gradientn(colours = matlab.like(100), "# per 353 m^2") +
    ggtitle(paste0(sp, " mean predicted density 2015-2019")) + 
    ggdark::dark_theme_minimal() +
    theme(legend.position = c(0.1, 0.2)))

png(paste0('outputs/', sp, '_density_mean.png'), width = 8, height = 5.2, units = "in", res = 500)
print(density_map_mean)
dev.off()

index <- get_index(p, bias_correct = F)

ggdark::invert_geom_defaults()

load("data/rea/ALL_REA_FISH_RAW_SST.RData")
df[df == -9991] <- NA

(survey_trend = df %>% 
    group_by(OBS_YEAR) %>% 
    summarise(n = mean(response)) %>% 
    ggplot(aes(OBS_YEAR, n)) + 
    geom_point(size = 2) + 
    ylab("Mean abundance (n) per sq.m") + 
    xlab("Year") + 
    geom_line() + 
    ggpubr::theme_pubr() + 
    ggtitle("Survey_based trend"))

(relative_biomass = index %>%
    ggplot(aes(year, est)) + 
    geom_line() +
    geom_point(size = 3) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
    xlab('Year') + 
    # ylab('biomass') +
    # ylab('metric tonnes') +
    ylab('Total count (n)') +
    # ggtitle("Biomass estimate") +
    ggtitle("Abundance_estimate") +
    # ggdark::dark_theme_minimal())
    ggpubr::theme_pubr())

survey_trend + relative_biomass

png(paste0('outputs/', sp, '_density_mean.png'), width = 10, height = 5, units = "in", res = 500)
print(survey_trend + relative_biomass)
dev.off()

index %>% 
  mutate(cv = sqrt(exp(se^2) - 1)) %>% 
  dplyr::select(-log_est, -max_gradient, -bad_eig, -se) %>%
  knitr::kable(format = "pandoc", digits = c(0, 0, 0, 0, 2))

# Calculate centre of gravity for latitude and longitude

cog <- get_cog(p) # calculate centre of gravity for each data point
cog$utm = ifelse(cog$coord == "X", "Eastings", "Northings")

(density_cog = ggplot(cog, aes(year, est, ymin = lwr, ymax = upr)) +
    geom_ribbon(alpha = 0.2) +
    geom_line() + 
    geom_point(size = 3) +
    facet_wrap(~utm, scales = "free_y") + 
    ggtitle("Center of gravity") + 
    ggdark::dark_theme_minimal())
# theme_pubr()

# table of COG by latitude
plot(data.frame(Y = p$data$Y, est = exp(p$data$est), year = p$data$year) %>%
       group_by(year) %>% summarize(cog = sum(Y * est) / sum(est)), type = "b", bty = "l", ylab = "Northing")

png("/Users/kisei/Desktop/sdmTMB.png", height = 8, width = 12, units = "in", res = 100)
(density_map + trend )/ (relative_biomass+density_cog)
dev.off()

