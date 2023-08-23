# save this for threashold modeling 
load("outputs/clean_df.RData")

load("data/crm/Topography_NOAA_CRM_vol10.RData") # bathymetry 
# topo$x = ifelse(topo$x > 180, topo$x - 360, topo$x)

islands = unique(df$island)

for (i in 1:length(islands)) {
  
  # i = 2
  
  df_i = df %>% subset(island == islands[i])
  
  zone <- (floor((df_i$lon[1] + 180)/6) %% 60) + 1
  xy_utm = as.data.frame(cbind(utm = project(as.matrix(df_i[, c("lon", "lat")]), paste0("+proj=utm +units=km +zone=", zone))))
  colnames(xy_utm) = c("X", "Y")
  df_i = cbind(df_i, xy_utm)
  plot(xy_utm, pch = ".", bty = 'n')
  rm(xy_utm)
  
  # Read in Island Boundaries
  load('data/MHI_islands_shp.RData')
  crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
  ISL_this_utm = spTransform(ISL_this,CRS(paste0("+proj=utm +units=km +zone=",zone)))
  ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=",zone))
  
  n_knots = 500
  
  rea_spde <- make_mesh(df_i, c("X", "Y"), n_knots = n_knots, type = "cutoff_search", seed = 2022) # search
  # rea_spde <- make_mesh(df, c("X", "Y"), cutoff  = n_knots, type = "cutoff") # predefined
  
  #build barrier to mesh
  rea_spde_coast = add_barrier_mesh(rea_spde , ISL_this_sf)
  
  png(paste0("outputs/SPDE_mesh_field_", islands[i], "_1000knots.png"), units = "in", height = 7, width = 7, res = 500)
  plot(rea_spde_coast$mesh, asp = 1, main = ""); axis(1); axis(2)
  plot(ISL_this_utm, add = TRUE)
  points(rea_spde_coast$loc_xy,col = "green", pch = ".", cex = 5)
  bar_i = rea_spde_coast$barrier_triangles
  norm_i = rea_spde_coast$normal_triangles
  points(rea_spde_coast$spde$mesh$loc[,1], rea_spde_coast$spde$mesh$loc[,2], pch = ".", col = "black")
  points(rea_spde_coast$mesh_sf$V1[bar_i], rea_spde_coast$mesh_sf$V2[bar_i], col = "red", pch = 20, cex = 0.5)
  points(rea_spde_coast$mesh_sf$V1[norm_i], rea_spde_coast$mesh_sf$V2[norm_i], col = "blue", pch = 20, cex = 0.5)
  dev.off()
  
  grid <- topo %>% subset(x > range(pretty(df_i$lon))[1] 
                          & x < range(pretty(df_i$lon))[2] 
                          & y > range(pretty(df_i$lat))[1] 
                          & y < range(pretty(df_i$lat))[2])
  
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
    
    grid_y = grid[,c("X", "Y", "x", "y", "Topography")]
    colnames(grid_y)[5] = "depth"
    grid_y$depth = grid_y$depth *-1
    # grid_y$depth_scaled = scale(log(grid_y$depth))
    # grid_y$depth_scaled2 = grid_y$depth_scaled ^ 2
    # grid_y$depth_mean = mean(grid_y$depth)
    # grid_y$depth_sd = sd(grid_y$depth)
    grid_y$year = years[[y]]
    grid_y$island = islands[i]
    grid_year = rbind(grid_year, grid_y)
    
  }
  
  save(grid_year, file = paste0('outputs/bathymetry_year_', islands[i], ".Rdata"))
  
}
