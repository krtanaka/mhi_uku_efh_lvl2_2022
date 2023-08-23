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
library(cowplot)
library(tidyr)

rm(list = ls())

select = dplyr::select

# load dynamic covariates, just keep lat lon unix time and covariates
load('data/rea/NCRMP_fish_site_20102019_MHI_dynamic.RData')
df$DATE_ = as.character(df$DATE_)
df$DATE_ = substr(df$DATE_, 1, 10)
df$DATE = as.numeric(as.POSIXct(df$DATE_))
cov_dynamic = df %>% select(LON, LAT, DATE, names(df)[15:254]); rm(df)

#load static covariates, just keep lat lon, depth
load('data/rea/NCRMP_fish_site_20102019_MHI_static.RData')
df_clim$Bathymetry = ifelse(is.na(df_clim$Bathymetry_CRM_Bathy_M), df_clim$Bathymetry_SRTM15_Bathy_M, df_clim$Bathymetry_CRM_Bathy_M)
cov_static = df_clim %>% 
  group_by(LON, LAT) %>% 
  summarise(Bathymetry = mean(Bathymetry, na.rm = T)) %>% 
  dplyr::select(LON, LAT, Bathymetry)
rm(df_clim)

# load survey data, adjust columns, create id
df = readRDS("data/rea/NCRMP_fish_site_20102019.rds")

df$ISLAND = gsub(" ", "_", df$ISLAND)
df$DATE_ = as.character(df$DATE_)
df$DATE_ = substr(df$DATE_, 1, 10)

colnames(df)[14] = "LAT"
colnames(df)[15] = "LON"

colnames(df)[7:9] = c("Year", "Month", "Day")
df$Year = substr(df$DATE_, 1, 4)
df$Month = substr(df$DATE_, 6, 7)
df$Day = substr(df$DATE_, 9, 10)

df$DATE = paste0(df$Month, "-", df$Day, "-", df$Year)
df$DATE_R = lubridate::mdy(df$DATE)
df$id = paste(df$LON, df$LAT, df$DATE_R, df$ISLAND, sep = "-")

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

# filter by islands, region
# choose abundance as response variable 
# filter by Uku (APVI)

df = df %>% 
  subset(ISLAND %in% islands) %>% 
  subset(REGION == "MHI") %>%
  subset(Response == "Abund_m2") %>% 
  select(id, names(df[,1:18]), names(select(df, contains("APVI"))))

# look at size distributions
islands = unique(df$ISLAND)

big_size_bins = NULL

for (i in 1:length(islands)) {
  
  df_i = df %>% subset(ISLAND == islands[i])
  
  size_bins = df_i[,c(21:dim(df_i)[2])] %>% 
    gather() %>% 
    mutate(key = gsub("APVI_", "", key)) %>% 
    group_by(key) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    mutate(freq = value/sum(value)) 
  
  size_bins$island = islands[i]
  
  big_size_bins = rbind(big_size_bins, size_bins)
  
}

big_size_bins$key <- factor(big_size_bins$key, levels = c("[0,10]",
                                                          "(10,20]", 
                                                          "(20,30]", 
                                                          "(30,40]",
                                                          "(40,50]", 
                                                          "(50,60]", 
                                                          "(60,70]", 
                                                          "(70,80]", 
                                                          "(80,90]", 
                                                          "(90,100]", 
                                                          "(100,Inf]"))
(f1a = big_size_bins %>% 
    ggplot(aes(x = key, y = freq, fill = island)) +
    geom_bar(stat = "identity", show.legend = T) + 
    xlab("Size_bins (cm)") + ylab("Proportion (%)") + 
    theme_half_open() + 
    scale_fill_manual("", values = matlab.like(8)) +
    theme(legend.position = c(0, 1),
          legend.justification = c(-0.1, 0.9),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10)) + 
    labs(tag = "(a)"))

rm(size_bins)

# for Uku: 
# Total numerical density estimates (individuals per 100 m2) were obtained by dividing fish counts in each survey by the survey area (353 m2 from two 15-m diameter survey cylinders) and multiplying by 100. - Nadon et al. 2020

df = df %>%
  select(names(df[,1:20])) %>% 
  mutate(DATE = as.numeric(as.POSIXct(df$DATE_)),
         YEAR = substr(DATE_, 1, 4)) %>% 
  group_by(LON, LAT, DATE, YEAR, ISLAND) %>%
  summarise(response = sum(APVI, na.rm = T)*100) %>%
  # subset(response < quantile(response, prob = 0.999)) %>%
  na.omit()

(f1b = df %>%
    group_by(YEAR, ISLAND) %>%
    summarise(sd = sd(response, na.rm = T),
              response = mean(response, na.rm = T)) %>% 
    ggplot(aes(YEAR, response)) +
    geom_pointrange(
      aes(ymin = ifelse(response - sd < 0, 0, response - sd), 
          ymax = response+sd, color = ISLAND),
      position = position_dodge(0.5)) + 
    # scale_color_viridis_d("", end = 0.8) +
    scale_color_manual(values = matlab.like(8), "") + 
    # ylab("Individuals per 100 sq.m") + xlab("") + 
    labs(y = expression("Individuals per 100" ~ m^2~""), x = "Year") +
    theme_half_open() + 
    theme(legend.position = c(1, 1),
          legend.justification = c(1,1),
          legend.direction = "horizontal") + 
    labs(tag = "(b)"))

f1a + f1b

df %>%
  group_by(LON, LAT, DATE, YEAR, ISLAND) %>% 
  ggplot(aes(response)) +
  geom_histogram(bins = 10)

scale_x_longitude <- function(xmin = -180, xmax = 180, step = 0.2, ...) {
  ewbrks <- seq(xmin,xmax,step)
  ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(x, "W"), ifelse(x > 0, paste(x, "E"),x))))
  return(scale_x_continuous("", breaks = ewbrks, labels = ewlbls, expand = c(0, 0), ...))
}
scale_y_latitude <- function(ymin = -90, ymax = 90, step = 0.2, ...) {
  nsbrks <- seq(ymin,ymax,step)
  nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(x, "S"), ifelse(x > 0, paste(x, "N"),x))))
  return(scale_y_continuous("", breaks = nsbrks, labels = nslbls, expand = c(0, 0), ...))
}

(f1c = df %>% 
    mutate(lon = round(LON, 2),
           lat = round(LAT, 2)) %>% 
    group_by(lon, lat, ISLAND) %>% 
    summarise(n = mean(response, na.rm = T)) %>% 
    ggplot(aes(lon, lat)) + 
    geom_point(aes(size = n, fill = n, color = n), shape = 21, alpha = 0.7) +
    # coord_fixed() +
    # scale_fill_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    # scale_color_viridis_c(guide = "legend", begin = 0, end = 0.8) +
    scale_color_gradientn(colours = matlab.like(100), guide = "legend") +
    scale_fill_gradientn(colours = matlab.like(100), guide = "legend") +
    theme_half_open() +
    facet_wrap(.~ISLAND, scales = "free", ncol = 4) +
    # scale_y_latitude() +
    # scale_x_longitude() +
    # ylab("Latitude (dec deg)") + xlab("Longitude (dec deg)") +
    labs(x = expression(paste("Longitude ", degree, "W", sep = "")),
         y = expression(paste("Latitude ", degree, "N", sep = ""))) +
    guides(color = guide_legend(expression("Individuals per 100" ~ m^2~"")), 
           fill = guide_legend(expression("Individuals per 100" ~ m^2~"")),
           size = guide_legend(expression("Individuals per 100" ~ m^2~""))) + 
    theme(legend.position = "bottom") +
    labs(tag = "(c)"))

(f1a + f1b) / f1c

png("outputs/fig1.png", units = "in", height = 12, width = 15, res = 500)
(f1a + f1b) / f1c
dev.off()

rm(f1a, f1b, f1c, scale_x_longitude, scale_y_latitude)

# merge static covariate 
df = merge(df, cov_static); rm(cov_static)

# merge dynamic covariate
df = merge(df, cov_dynamic); rm(cov_dynamic)

df = df[!duplicated(df[, 1:7]), ]

df[df == "NaN"] <- NA

df <- df %>% select(-contains("ALLB4"))
df <- df %>% select(-contains("MO03"))
df <- df %>% select(-contains("YR03"))
df <- df %>% select(-contains("YR05"))
df <- df %>% select(-contains("YR10"))
df <- df %>% select(-contains("YR10YR01"))
df <- df %>% select(-contains("8Day_WK01"))
df <- df %>% select(-contains("_WK01"))
df <- df %>% select(-contains("_YR01"))
df <- df %>% select(-contains("mean_annual_range_"))
df <- df %>% select(-contains("mean_monthly_range_"))

df$chla_8d = rowMeans(df[,c("mean_Chlorophyll_A_AquaMODIS_8Day_DY01","mean_Chlorophyll_A_ESAOCCCI_8Day_DY01")], na.rm = T)
df = df[ , !names(df) %in% c("mean_Chlorophyll_A_AquaMODIS_8Day_DY01","mean_Chlorophyll_A_ESAOCCCI_8Day_DY01")] 

df$chla_m = rowMeans(df[,c("mean_Chlorophyll_A_AquaMODIS_8Day_MO01","mean_Chlorophyll_A_ESAOCCCI_8Day_MO01")], na.rm = T)
df = df[ , !names(df) %in% c("mean_Chlorophyll_A_AquaMODIS_8Day_MO01","mean_Chlorophyll_A_ESAOCCCI_8Day_MO01")] 

# df$q05_chla_8d = rowMeans(df[,c("q05_Chlorophyll_A_AquaMODIS_8Day_DY01","q05_Chlorophyll_A_ESAOCCCI_8Day_DY01")], na.rm = T)
df = df[ , !names(df) %in% c("q05_Chlorophyll_A_AquaMODIS_8Day_DY01","q05_Chlorophyll_A_ESAOCCCI_8Day_DY01")] 

df$q05_chla_m = rowMeans(df[,c("q05_Chlorophyll_A_AquaMODIS_8Day_MO01","q05_Chlorophyll_A_ESAOCCCI_8Day_MO01")], na.rm = T)
df = df[ , !names(df) %in% c("q05_Chlorophyll_A_AquaMODIS_8Day_MO01","q05_Chlorophyll_A_ESAOCCCI_8Day_MO01")] 

# df$q95_chla_8d = rowMeans(df[,c("q95_Chlorophyll_A_AquaMODIS_8Day_DY01","q95_Chlorophyll_A_ESAOCCCI_8Day_DY01")], na.rm = T)
df = df[ , !names(df) %in% c("q95_Chlorophyll_A_AquaMODIS_8Day_DY01","q95_Chlorophyll_A_ESAOCCCI_8Day_DY01")] 

df$q95_chla_m = rowMeans(df[,c("q95_Chlorophyll_A_AquaMODIS_8Day_MO01","q95_Chlorophyll_A_ESAOCCCI_8Day_MO01")], na.rm = T)
df = df[ , !names(df) %in% c("q95_Chlorophyll_A_AquaMODIS_8Day_MO01","q95_Chlorophyll_A_ESAOCCCI_8Day_MO01")] 

# df$sd_chla_8d = rowMeans(df[,c("sd_Chlorophyll_A_AquaMODIS_8Day_DY01","sd_Chlorophyll_A_ESAOCCCI_8Day_DY01")], na.rm = T)
df = df[ , !names(df) %in% c("sd_Chlorophyll_A_AquaMODIS_8Day_DY01","sd_Chlorophyll_A_ESAOCCCI_8Day_DY01")] 

df$sd_chla_m = rowMeans(df[,c("sd_Chlorophyll_A_AquaMODIS_8Day_MO01","sd_Chlorophyll_A_ESAOCCCI_8Day_MO01")], na.rm = T)
df = df[ , !names(df) %in% c("sd_Chlorophyll_A_AquaMODIS_8Day_MO01","sd_Chlorophyll_A_ESAOCCCI_8Day_MO01")] 

names(df) <- gsub("mean_", "", names(df)); names(df)
names(df) <- gsub("_DY01", "_d", names(df)); names(df)
names(df) <- gsub("_MO01", "_m", names(df)); names(df)

names(df) <- gsub("Ocean_Surface_Winds", "wind", names(df)); names(df)
names(df) <- gsub("SST_CRW_Daily", "sst", names(df)); names(df)
names(df) <- gsub("Bathymetry", "depth", names(df)); names(df)

# remove those bc don't relaly make sense
df = df[ , !names(df) %in% c("q05_wind_d","q95_wind_d", "sd_wind_d", "q05_sst_d", "q95_sst_d", "sd_sst_d")] ; names(df)

df = df %>% 
  select(names(df)[1:7], 
         sst_d, chla_8d, wind_d, 
         sst_m, chla_m, wind_m,
         q95_sst_m, q95_chla_m, q95_wind_m,
         q05_sst_m, q05_chla_m, q05_wind_m,
         sd_sst_m, sd_chla_m, sd_wind_m) %>% 
  mutate(depth = depth*-1)  

names(df) <- tolower(names(df))
df = df[complete.cases(df), ]

# turn this off because the original data were collected between 0-30m ?
df = df %>% subset(depth <= 30)
df = df %>% subset(depth > 0)
df$depth_scaled = scale(log(df$depth))
df$depth_scaled2 = df$depth_scaled ^ 2
df$depth_mean = mean(df$depth)
df$depth_sd = sd(df$depth)

# save this for threashold modeling 
save(df, file = "outputs/clean_df.RData")

(depth = df %>% 
    # subset(response > 0) %>%
    ggplot() + 
    stat_smooth(aes(depth, log(response+1), colour = "Depth", fill = "Depth"), method = "loess", span = 1, alpha = 0.4, show.legend = F) + 
    scale_colour_manual("",
                        breaks = c("Depth"),
                        values = c("#0000FF")) +
    scale_fill_manual("",
                      breaks = c("Depth"),
                      values = c("#0000FF")) +
    # scale_fill_viridis_d("", begin = 0.1, option = "plasma") +
    # scale_color_viridis_d("", begin = 0.1, option = "plasma") +
    theme_half_open(I(20)) + 
    labs(y = expression("log(individuals per 100" ~ m^2~")"), x = "Depth (m)"))

(sst = df %>%
    # subset(response > 0) %>%
    ggplot() + 
    stat_smooth(aes(sst_d, log(response+1), colour = "Daily_SST", fill = "Daily_SST"), method = "loess", span = 1, alpha = 0.4) + 
    stat_smooth(aes(sst_m, log(response+1), colour = "Monthly_SST", fill = "Monthly_SST"), method = "loess", span = 1, alpha = 0.4) + 
    stat_smooth(aes(q95_sst_m, log(response+1), colour = "Monthly_SST_q95", fill = "Monthly_SST_q95"), method = "loess", span = 1, alpha = 0.4) + 
    stat_smooth(aes(q05_sst_m, log(response+1), colour = "Monthly_SST_q05", fill = "Monthly_SST_q05"), method = "loess", span = 1, alpha = 0.4) + 
    scale_colour_manual("",
                        breaks = c("Daily_SST", "Monthly_SST", "Monthly_SST_q95", "Monthly_SST_q05"),
                        values = c("#0000FF", "#00FFFF", "#FFFF00", "#FF0000")) +
    scale_fill_manual("",
                      breaks = c("Daily_SST", "Monthly_SST", "Monthly_SST_q95", "Monthly_SST_q05"),
                      values = c("#0000FF", "#00FFFF", "#FFFF00", "#FF0000")) +
    # scale_fill_viridis_d("", end = 0.8, option = "plasma") +
    # scale_color_viridis_d("", end = 0.8, option = "plasma") +
    theme_half_open(I(20)) + 
    labs(y = expression("log(individuals per 100" ~ m^2~")"), x = "SST (°C)") +
    theme(legend.justification = c(1, -0.1), legend.position = c(1, 0)) +
    guides(color = guide_legend(override.aes = list(fill = NA))))

(sst_sd = df %>% 
    # subset(response > 0) %>%
    ggplot() + 
    stat_smooth(aes(sd_sst_m, log(response+1), colour = "Monthly_SST_SD", fill = "Monthly_SST_SD"), method = "loess", span = 1, alpha = 0.4, show.legend = F) + 
    scale_colour_manual("",
                        breaks = c("Monthly_SST_SD"),
                        values = c("#FF0000")) +
    scale_fill_manual("",
                      breaks = c("Monthly_SST_SD"),
                      values = c("#FF0000")) +
    # scale_fill_viridis_d("", begin = 0.8, option = "plasma") +
    # scale_color_viridis_d("", begin = 0.8, option = "plasma") +
    theme_half_open(I(20)) + 
    labs(y = expression("log(individuals per 100" ~ m^2~")"), x = "Monthly SST SD (°C)") +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1)))

(chla = df %>% 
    # subset(response > 0) %>%
    ggplot() + 
    stat_smooth(aes(chla_8d, log(response+1), colour = "8Days_CHLA", fill = "8Days_CHLA"), method = "loess", span = 1, alpha = 0.4) + 
    stat_smooth(aes(chla_m, log(response+1), colour = "Monthly_CHLA", fill = "Monthly_CHLA"), method = "loess", span = 1, alpha = 0.4) + 
    stat_smooth(aes(q95_chla_m, log(response+1), colour = "Monthly_CHLA_q95", fill = "Monthly_CHLA_q95"), method = "loess", span = 1, alpha = 0.4) + 
    stat_smooth(aes(q05_chla_m, log(response+1), colour = "Monthly_CHLA_q05", fill = "Monthly_CHLA_q05"), method = "loess", span = 1, alpha = 0.4) + 
    scale_colour_manual("",
                        breaks = c("8Days_CHLA", "Monthly_CHLA", "Monthly_CHLA_q95", "Monthly_CHLA_q05"),
                        values = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")) +
    scale_fill_manual("",
                      breaks = c("8Days_CHLA", "Monthly_CHLA", "Monthly_CHLA_q95", "Monthly_CHLA_q05"),
                      values = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")) +
    # scale_fill_viridis_d("", end = 0.8, option = "plasma") +
    # scale_color_viridis_d("", end = 0.8, option = "plasma") +
    theme_half_open(I(20)) + 
    labs(y = expression("log(individuals per 100" ~ m^2~")"), x = expression("Chl_a (mg " ~ m^-3~")")) +
    theme(legend.justification = c(-0.1, -0.1), legend.position = c(0, 0),
          legend.background = element_rect(fill = alpha('white', 0.1))) +
    guides(color = guide_legend(override.aes = list(fill = NA))))

(chla_sd = df %>% 
    # subset(response > 0) %>%
    ggplot() + 
    stat_smooth(aes(sd_chla_m, log(response+1), colour = "Monthly_CHLA_SD", fill = "Monthly_CHLA_SD"), method = "loess", span = 1, alpha = 0.4, show.legend = F) + 
    scale_colour_manual("",
                        breaks = c("Monthly_CHLA_SD"),
                        values = c("#440154FF")) +
    scale_fill_manual("",
                      breaks = c("Monthly_CHLA_SD"),
                      values = c("#440154FF")) +
    # scale_fill_viridis_d("", begin = 0.5, option = "plasma") +
    # scale_color_viridis_d("", begin = 0.5, option = "plasma") +
    theme_half_open(I(20)) + 
    labs(y = expression("log(individuals per 100" ~ m^2~")"), x = expression("Monthly Chl_a SD (mg " ~ m^-3~")")) +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1),
          legend.background = element_rect(fill = alpha('white', 0.1))) +
    guides(color = guide_legend(override.aes = list(fill = NA))))

(wind = df %>% 
    # subset(response > 0) %>%
    ggplot() + 
    stat_smooth(aes(wind_d, log(response+1), colour = "Daily_Wind_Speed", fill = "Daily_Wind_Speed"), method = "loess", span = 1, alpha = 0.4) + 
    stat_smooth(aes(wind_m, log(response+1), colour = "Monthly_Wind_Speed", fill = "Monthly_Wind_Speed"), method = "loess", span = 1, alpha = 0.4) + 
    stat_smooth(aes(q95_wind_m, log(response+1), colour = "Monthly_Wind_Speed_q95", fill = "Monthly_Wind_Speed_q95"), method = "loess", span = 1, alpha = 0.4) + 
    stat_smooth(aes(q05_wind_m, log(response+1), colour = "Monthly_Wind_Speed_q05", fill = "Monthly_Wind_Speed_q05"), method = "loess", span = 1, alpha = 0.4) + 
    scale_colour_manual("",
                        breaks = c("Daily_Wind_Speed", "Monthly_Wind_Speed", "Monthly_Wind_Speed_q95", "Monthly_Wind_Speed_q05"),
                        values = c("#4C00FF", "#00E5FF", "#00FF4D", "#FFFF00")) +
    scale_fill_manual("",
                      breaks = c("Daily_Wind_Speed", "Monthly_Wind_Speed", "Monthly_Wind_Speed_q95", "Monthly_Wind_Speed_q05"),
                      values = c("#4C00FF", "#00E5FF", "#00FF4D", "#FFFF00")) +
    # scale_fill_viridis_d("", end = 0.8, option = "plasma") +
    # scale_color_viridis_d("", end = 0.8, option = "plasma") +
    theme_half_open(I(20)) + 
    labs(y = expression("log(individuals per 100" ~ m^2~")"), x = expression("Wind Speed (m" ~ s^-1~")")) +
    theme(legend.justification = c(1, 0), legend.position = c(1, 0),
          legend.background = element_rect(fill = alpha('white', 0.1))) +
    guides(color = guide_legend(override.aes = list(fill = NA))))

(wind_sd = df %>% 
    # subset(response > 0) %>%
    ggplot() + 
    stat_smooth(aes(sd_wind_m, log(response+1), colour = "Monthly_Wind_Speed_SD", fill = "Monthly_Wind_Speed_SD"), method = "loess", span = 1, alpha = 0.4, show.legend = F) + 
    scale_colour_manual("",
                        breaks = c("Monthly_Wind_Speed_SD"),
                        values = c("#00E5FF")) +
    scale_fill_manual("",
                      breaks = c("Monthly_Wind_Speed_SD"),
                      values = c("#00E5FF")) +
    # scale_fill_viridis_d("", begin = 0.8, option = "C") +
    # scale_color_viridis_d("", begin = 0.8, option = "C") +
    theme_half_open(I(20)) + 
    labs(y = expression("log(individuals per 100" ~ m^2~")"), x = expression("Monthly Wind Speed SD (m" ~ s^-1~")")))


png("outputs/fig2.png", height = 25, width = 20, units = "in", res = 500)
((sst + sst_sd) / (chla + chla_sd) / (wind + wind_sd))/depth
dev.off()
