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
ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=",zone))

n_knots = 1000
n_knots = 500
n_knots = 300 
n_knots = 100 

rea_spde <- make_mesh(df, c("X", "Y"), n_knots = n_knots, type = "cutoff_search", seed = 2022) # search
# rea_spde <- make_mesh(df, c("X", "Y"), cutoff  = 15, type = "cutoff") # predefined

#build barrier to mesh
rea_spde_coast = add_barrier_mesh(rea_spde , ISL_this_sf)

png(paste0("outputs/SPDE_mesh_field_", n_knots, "_knots.png"), units = "in", height = 10, width = 10, res = 500)
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

model_type = c("static", "dynamic")[1]

density_model <- sdmTMB(
  
  # using (date) works but results in poor model performances
  
  data = df, 
  
  formula = response ~ as.factor(year) + s(depth, k=5),
  
  # formula = response ~ as.factor(year) + s(depth, k=5) +
  #   s(sst_d, k=5) + s(chla_8d, k=5) + s(wind_d, k=5) +
  #   s(sst_m, k=5) + s(chla_m, k=5) + s(wind_m, k=5) +
  #   s(q95_sst_m, k=5) + s(q95_chla_m, k=5) + s(q95_wind_m, k=5) +
  #   s(q05_sst_m, k=5) + s(q05_chla_m, k=5) + s(q05_wind_m, k=5) +
  #   s(sd_sst_m, k=5)+ s(sd_chla_m, k=5) + s(sd_wind_m, k=5),
  # 
  # formula = response ~ as.factor(year) + s(depth, k=5) +
  #   s(sst_d, k=5) + s(wind_d, k=5) +
  #   s(wind_m, k=5) +
  #   s(q05_chla_m, k=5) +
  #   s(sd_sst_m, k=5)+ s(sd_chla_m, k=5) + s(sd_wind_m, k=5),
  
  silent = F, 
  
  # extra_time = missing_year,
  
  # spatial_only = TRUE,
  # reml = T,
  
  # time = "date",
  time = "year",
  
  # spde = rea_spde,
  mesh = rea_spde_coast,

  # anisotropy = T,
  
  family = tweedie(link = "log"),
  # family = poisson(link = "log"),
  # family = binomial(link = "logit"), weights = n,
  # family = nbinom2(link = "log"),
  # family = Beta(link = "logit"),
  
  spatiotemporal = "IID",
  
  control = sdmTMBcontrol(step.min = 0.01, step.max = 1)
  # control = sdmTMBcontrol(nlminb_loops = 2, newton_loops = 1),
  # priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 75, sigma_lt = 5))
  
); beepr::beep(2)

density_model <- run_extra_optimization(density_model, newton_loops = 0); beepr::beep(2)

# look at gradients
max(density_model$gradients)

saveRDS(density_model, paste0("outputs/density_model_", n_knots, ".rds"))
saveRDS(density_model, paste0("outputs/density_model_", n_knots, ".rds"))

density_model = read_rds("outputs/density_model_1000.rds")

# Get DHARMa residuals
# The `simulated_response` argument is first so the output from
# simulate() can be piped to dharma_residuals():
# simulate(fit, nsim = 500) %>% dharma_residuals(fit)
s <- simulate(density_model, nsim = 500)
dharma_residuals(s, density_model)
r <- dharma_residuals(s, density_model, plot = FALSE)
head(r)
par(pty = "s")
plot(r$expected, r$observed, pch = ".", bty = "n")
abline(a = 0, b = 1)

df$residuals <- residuals(density_model)
par(pty = "s")
png('outputs/qq.png', width = 5, height = 5.5, units = "in", res = 500)
qqnorm(df$residuals, ylim = c(-5, 5), xlim = c(-5, 5), bty = "n", pch = 20); abline(a = 0, b = 1)
dev.off()

m_p <- predict(density_model); m_p = m_p[,c("response", "est")]
m_p$back_abs_res = abs(df$residuals)

(p1 = ggplot(df, aes_string("X", "Y", color = "residuals")) +
    geom_point(alpha = 0.8, size = round(abs(df$residuals), digits = 0)) + 
    xlab("Eastings (km)") +
    ylab("Northings (km)") + 
    # coord_fixed() +
    facet_wrap(~year) + 
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

# confidence intervals on the fixed effects
tidy(density_model, conf.int = TRUE)

# And similarly for the random effect parameters:
tidy(density_model, "ran_pars", conf.int = TRUE)

png(paste0('outputs/residuals_', "Uku", '.png'), width = 8, height = 8, units = "in", res = 100)
print(p1)
dev.off()

png(paste0('outputs/pred_obs_', "Uku", '.png'), width = 5, height = 5, units = "in", res = 100)
print(p2)
dev.off()

rm(p1, p2, m_p)

# #  extract some parameter estimates
# sd <- as.data.frame(summary(TMB::sdreport(density_model$tmb_obj)))
# r <- density_model$tmb_obj$report()
# r

######################################################################################
### prediction onto new data grid, only works wtih model with no dynamic covaraite ###
######################################################################################

load("data/crm/Topography_NOAA_CRM_vol10.RData") # bathymetry 
# topo$x = ifelse(topo$x > 180, topo$x - 360, topo$x)

grid = topo
grid <- topo %>% subset(x > range(pretty(df$lon))[1] 
                        & x < range(pretty(df$lon))[2] 
                        & y > range(pretty(df$lat))[1] 
                        & y < range(pretty(df$lat))[2])
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

# only depth covariate
for (y in 1:length(years)) {
  
  # y = 1
  
  grid_y = grid[,c("X", "Y", "Topography")]
  colnames(grid_y)[3] = "depth"
  grid_y$depth = grid_y$depth *-1
  # grid_y$depth_scaled = scale(log(grid_y$depth))
  # grid_y$depth_scaled2 = grid_y$depth_scaled ^ 2
  # grid_y$depth_mean = mean(grid_y$depth)
  # grid_y$depth_sd = sd(grid_y$depth)
  grid_y$year = years[[y]]
  grid_year = rbind(grid_year, grid_y)
  
}

grid_year_missing = NULL

for (y in 1:length(missing_year)) {
  
  # y = 4
  
  # Add missing years to our grid:
  # grid_from_missing_yr <- grid_year[grid_year$year ==  missing_year[[y]]-1, ]
  grid_from_missing_yr <- grid_year[grid_year$year ==  2010, ]
  grid_from_missing_yr$year <-  missing_year[[y]] # `L` because `year` is an integer in the data
  grid_year_missing <- rbind(grid_year_missing, grid_from_missing_yr)
  
}

# grid_year = rbind(grid_year, grid_year_missing)

rm(xy_utm, grid, grid_from_missing_yr, grid_y, grid_year_missing, topo)
