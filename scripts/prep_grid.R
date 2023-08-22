library(terra)
library(dplyr)
library(rgdal)
library(raster)

# # http://www.soest.hawaii.edu/pibhmc/cms/data-by-location/cnmi-guam/guam-island/bathymetry/
# nd = raster("G:/GIS/bathymetry/MARI/gua_mb_ld.asc"); res(nd) # Multibeam & Lidar Integrated Bathymetry
# 
# nd[nd <= -30] <- NA
# nd[nd >= 0] <- NA  
# 
# nd <- aggregate(nd, fact = 100/res(nd))
# res(nd)
# 
# plot(nd)
# 
# nd = rasterToPoints(nd) %>% as.data.frame()
# 
# nd = nd %>% 
#   mutate(X = x/1000,
#          Y = y/1000,
#          Depth = gua_mb_ld*-1) %>% 
#   select(X, Y)
# 
# grid = NULL
# 
# obs_year = c("2011", "2014", "2017")
# 
# for (y in 1:length(obs_year)) {
#   
#   nd$year = obs_year[y]
#   
#   grid = rbind(grid, nd)
#   
# }
# 
# save(grid, file = "data/guam_grid_100m.Rdata")
# 
# # NOAA/PIBHMC 60-m Bathymetry: Guam
# nd = raster("G:/GIS/bathymetry/MARI/Guam_60m.asc"); res(nd)
# 
# nd[nd <= -30] <- NA
# nd[nd >= 0] <- NA  
# 
# nd <- aggregate(nd, fact = 100/res(nd))
# res(nd)
# 
# plot(nd)
# 
# nd = rasterToPoints(nd) %>% as.data.frame()
# 
# nd = nd %>% 
#   mutate(X = x/1000,
#          Y = y/1000,
#          Depth = Guam_60m*-1) %>% 
#   select(X, Y)
# 
# grid = NULL
# 
# obs_year = c("2011", "2014", "2017")
# 
# for (y in 1:length(obs_year)) {
#   
#   nd$year = obs_year[y]
#   
#   grid = rbind(grid, nd)
#   
# }
# 
# save(grid, file = "data/guam_grid_60m.Rdata")
# 
# 
# # Mariana Trench 6 arc-second Bathymetric Digital Elevation Model
# nd = rast("G:/GIS/bathymetry/MARI/mariana_trench_6_msl_2012.nc"); res(nd) 
# 
# r <- ext(144.5, 146, 13, 21); plot(r, axes = F); maps::map(add = T)
# nd <- crop(nd, r)
# 
# nd = raster(nd); res(nd)
# 
# # nd <- aggregate(nd, fact = 100/res(nd))
# # res(nd)
# 
# nd[nd <= -31] <- NA
# nd[nd >= 1] <- NA  
# 
# plot(nd)
# 
# nd = rasterToPoints(nd) %>% as.data.frame()
# 
# zone <- (floor((nd$x[1] + 180)/6) %% 60) + 1
# xy_utm = as.data.frame(cbind(utm = project(as.matrix(nd[, c("x", "y")]), paste0("+proj=utm +units=km +zone=", zone))))
# colnames(xy_utm) = c("X", "Y")
# nd = cbind(nd, xy_utm)
# plot(xy_utm, pch = ".", bty = 'n')
# rm(xy_utm)
# 
# nd = nd %>% 
#   # mutate(X = x/1000,
#   #        Y = y/1000,
#   #        Depth = Band1*-1) %>% 
#   select(X, Y)
# 
# grid = NULL
# 
# obs_year = c("2011", "2014", "2017")
# 
# for (y in 1:length(obs_year)) {
#   
#   nd$year = obs_year[y]
#   
#   grid = rbind(grid, nd)
#   
# }
# 
# save(grid, file = "data/mariana_grid_6sec.Rdata")
# 
# # NOAA/NGDC 10-m Bathymetry: Guam (https://pae-paha.pacioos.hawaii.edu/erddap/griddap/ngdc_bathy_10m_guam.html)
# nd = raster("G:/GIS/bathymetry/MARI/ngdc_bathy_10m_guam_9fca_12ce_5623.nc"); res(nd) 
# 
# nd[nd <= -30] <- NA
# nd[nd >= 1] <- NA  
# 
# plot(nd)
# 
# nd = rasterToPoints(nd) %>% as.data.frame()
# 
# zone <- (floor((nd$x[1] + 180)/6) %% 60) + 1
# xy_utm = as.data.frame(cbind(utm = project(as.matrix(nd[, c("x", "y")]), paste0("+proj=utm +units=km +zone=", zone))))
# colnames(xy_utm) = c("X", "Y")
# nd = cbind(nd, xy_utm)
# plot(xy_utm, pch = ".", bty = 'n')
# rm(xy_utm)
# 
# nd = nd %>% 
#   mutate(depth = elevation) %>%
#   dplyr::select(X, Y, depth)
# 
# grid = NULL
# 
# obs_year = c("2009", "2011", "2014", "2017", "2022")
# 
# for (y in 1:length(obs_year)) {
#   
#   nd$year = obs_year[y]
#   
#   grid = rbind(grid, nd)
#   
# }
# 
# grid$depth = grid$depth * -1
# grid = grid %>% 
#   # mutate(X = round(X, 0),
#   # Y = round(Y, 0)) %>%
#   mutate(X = ceiling(X*2)/2,
#          Y = ceiling(Y*2)/2) %>%
#   group_by(X, Y, year) %>%
#   summarise(depth = mean(depth)) 
# 
# save(grid, file = "data/grid/guam_grid_0.5km.Rdata")


rm(list = ls())

load("data/ncrmp_eds_marian_samoa_all_sp.Rdata") # eds-enhanced ncrmp data w/ all species (n = 727)
df = df %>% filter(region == "MARIAN")

# Create list of climatology raster files
rasterlist = list.files(c(
  #"M:/Environmental_Data_Summary/Data_Download/Bathymetry_SRTM15/",
  #"M:/Environmental_Data_Summary/Data_Download/Bathymetry_DEM6_Mariana_Trench/",
  "M:/Environmental_Data_Summary/Data_Download/Bathymetry_PIBHMC_Mariana_Samoa/"), 
  recursive = T, 
  pattern = "_AllIslands.nc", 
  full.names = T)

islands =  c("Guam", 
             "Rota", 
             "Aguijan",
             "Tinian",
             "Saipan",
             "Sarigan", 
             "Pagan",
             "Asuncion",
             "Pajaros", # Farallon_de_Pajaros
             "Maug",
             "Agrihan", 
             "Alamagan",
             "Guguan")

# obs_year = unique(df$year) #year
obs_year = unique(substr(df$date, 1, 7)) #year-month

grid = NULL

for (r in 1:length(islands)) {
  
  # r = 1
  
  ri = raster(grep(tolower(islands)[r], rasterlist, value = T)) %>%  rasterToPoints() %>% as.data.frame()
  colnames(ri) = c("lon", "lat", "depth")
  
  ri = ri %>% 
    subset(depth > -33) %>% 
    group_by(lon, lat) %>% 
    summarise(depth = mean(depth, na.rm = T))
  
  if (r == 1) ri = ri %>% subset(lon > 144.6)
  if (r == 4) ri = ri %>% subset(lon < 145.68)
  
  # ri %>%
  #   group_by(lon, lat) %>%
  #   summarise(depth = mean(depth)) %>%
  #   ggplot(aes(lon, lat, fill = depth)) +
  #   geom_raster() +
  #   scale_fill_viridis_c()
  
  zone <- (floor((ri$lon[1] + 180)/6) %% 60) + 1
  xy_utm = as.data.frame(cbind(utm = project(as.matrix(ri[, c("lon", "lat")]), paste0("+proj=utm +units=km +zone=", zone))))
  colnames(xy_utm) = c("X", "Y")
  ri = cbind(xy_utm, ri)
  # plot(xy_utm, pch = 20, bty = 'n')
  rm(xy_utm)
  
  ri$Island = islands[r]
  
  grid_i = NULL
  
  for (y in 1:length(obs_year)) {
    
    ri$Year = obs_year[y]
    
    grid_i = rbind(grid_i, ri)
    
  }
  
  grid = rbind(grid_i, grid)
  
}

# aggregate to 0.5km or 0.05km res
grid = grid %>% 
  mutate(
    # X = ceiling(X*2)/2,
    # Y = ceiling(Y*2)/2,
    X = plyr::round_any(X, 0.05),
    Y = plyr::round_any(Y, 0.05),
    Depth = depth * -1) %>%
  group_by(X, Y, Year, Island) %>%
  summarise(Depth = mean(Depth)) 

grid %>% 
  ggplot(aes(X, Y, fill = Depth)) + 
  geom_raster() + 
  scale_fill_viridis_c() + 
  # coord_fixed() + 
  facet_wrap(~Island, scales = "free")

save(grid, file = "data/grid/marian_grid_0.5km.Rdata")
save(grid, file = "data/grid/marian_grid_0.05km.Rdata")
