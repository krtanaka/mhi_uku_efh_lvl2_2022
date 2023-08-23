library(raster)
library(dplyr)

rm(list = ls())

islands = c("Hawaii", "Kahoolawe", "Kauai", "Kaula", "Lanai", "Lehua", "Maui", "Molokai", "Molokini", "Niihau", "Oahu")

df = NULL

for (i in 1:length(islands)) {
  
  i = 2
  
  # d = stack(paste0('~/Desktop/EDS/DataDownload/Bathymetry_SRTM15/Island_By_Blocks_Level_Data/', islands[i], '_Bathymetry_SRTM15_Bathy_M.nc'))
  d = stack(paste0('/Users/Kisei/Desktop/EDS/DataDownload/NOAA Coastal Relief Model/Island_By_Blocks_Level_Data/', islands[i], '_NOAA Coastal Relief Model_Bathy_M.nc'))
  
  d[d > 0] <- NA
  d[d < -30] <- NA
  
  s = stack(paste0('/Users/Kisei/Desktop/EDS/DataDownload/SST_CRW_Monthly/Island_Level_Data/', islands[i], '_SST_CRW_Monthly_1985-01-31_2021-03-31.nc'))
  
  # s = raster::rotate(s)
  
  s = resample(s, d, method = "bilinear") 
  
  ds = stack(d, s)
  
  ds = as.data.frame(rasterToPoints(ds))
  
  ds = ds %>% na.omit()
  ds$isl = islands[i]
  
  df = rbind(df, ds)
  
}

colnames(df) = gsub("X", "", colnames(df))
colnames(df) = gsub(".12.00.00", "", colnames(df))

topo = df
save(topo, file = 'data/crm/Topography_NOAA_CRM_vol10_SST_CRW_Monthly.RData')