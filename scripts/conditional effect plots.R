library(sdmTMB)
library(dplyr)
library(ggplot2)
library(rgdal)
library(colorRamps)
library(patchwork)
library(raster)
library(sf)
library(readr)
library(tidyr)
library(mgcv)
library(visreg)
library(maps)
library(sp)

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
fish_info = read_csv("data/rea/NCRMP_fish_taxainfo.csv")

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

df = df %>% 
  subset(ISLAND %in% islands) %>% 
  subset(REGION == "MHI") %>%
  subset(Response == "Abund_m2") %>% 
  select(id, names(df[,1:18]), names(select(df, contains("APVI"))))

# for Uku: 
# Total numerical density estimates (individuals per 100 m2) were obtained by dividing fish counts in each survey by the survey area (353 m2 from two 15-m diameter survey cylinders) and multiplying by 100. - Nadon et al. 2020

# select response group = all size bins
df = df %>% 
  select(names(df[,1:20])) %>% 
  mutate(DATE = as.numeric(as.POSIXct(df$DATE_)),
         YEAR = substr(DATE_, 1, 4)) %>% 
  group_by(LON, LAT, DATE, YEAR) %>% 
  summarise(response = sum(APVI, na.rm = T)*100) %>%
  # subset(response < quantile(response, prob = 0.999)) %>%
  na.omit()

# merge static covariate 
df = merge(df, cov_static); rm(cov_static)

# merge dynamic covariate
df = merge(df, cov_dynamic); rm(cov_dynamic)

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
  select(names(df)[1:6], 
         sst_d, chla_8d, wind_d, 
         sst_m, chla_m, wind_m,
         q95_sst_m, q95_chla_m, q95_wind_m,
         q05_sst_m, q05_chla_m, q05_wind_m,
         sd_sst_m, sd_chla_m, sd_wind_m) %>% 
  mutate(depth = depth*-1)  

names(df) <- tolower(names(df))
df = df[complete.cases(df), ]

df = df %>% subset(depth < 30)
df = df %>% subset(depth > 0)
df$depth_scaled = scale(log(df$depth))
df$depth_scaled2 = df$depth_scaled ^ 2
df$depth_mean = mean(df$depth)
df$depth_sd = sd(df$depth)

ukugam = gam(response ~ 
               
             #   + s(sst_d, k = 5)
             # + s(sst_m, k = 5)
             # + s(q95_sst_m, k = 5)
             # + s(q05_sst_m, k = 5)
             # + s(sd_sst_m, k = 5)
             # 
             # + s(chla_8d, k = 5)
             # + s(chla_m, k = 5)
             # + s(q95_chla_m, k = 5)
             # + s(q05_chla_m, k = 5)
             # + s(sd_chla_m, k = 5)
             # 
             # + s(wind_d, k = 5)
             # + s(wind_m, k = 5)
             # + s(q95_wind_m, k = 5)
             # + s(q05_wind_m, k = 5)
             + s(sd_wind_m, k = 3)
             
             + s(depth, k = 3),
             
             family = "tw(theta = NULL, link = 'log',a=1.01,b=1.99)", 
             data = df, 
             gamma = 1.4)

summary(ukugam)
plot(ukugam, pages = 1)

#visreg on response scale
type = "conditional"
scale = "response"

visreg(ukugam, "sst_d", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "sst_d")

visreg(ukugam, "sst_m", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "sst_m")

visreg(ukugam, "q95_sst_m", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "q95_sst_m")

visreg(ukugam, "sd_sst_m", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "sd_sst_m")

visreg(ukugam, "chla_8d", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "chla_8d")

visreg(ukugam, "wind_d", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "wind_d")

visreg(ukugam, "q95_wind_m", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "q95_wind_m")

visreg(ukugam, "q05_wind_m", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "q05_wind_m")

visreg(ukugam, "sd_wind_m", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "sd_wind_m")

visreg(ukugam, "depth", partial = F, scale = scale, line = list(col="black"),
       ylab = "", xlab = "depth")

(wind_sd = visreg(ukugam, "sd_wind_m", partial = F, scale = scale, line = list(col = "black"), 
                  rug = F,
                  ylab = expression("Individuals per 100" ~ m^2~""),
                  # ylab = "",
                  xlab = expression("Monthly Wind Speed SD (m" ~ s^-1~")"), 
                  gg = TRUE) + 
    coord_fixed(ratio = 40) + 
    theme_half_open())

(depth = visreg(ukugam, "depth", partial = F, scale = scale, line = list(col="black"),
        rug = F,
        ylab = expression("Individuals per 100" ~ m^2~""),  
        xlab = "Depth (m)", 
        gg = TRUE) + 
    coord_fixed(ratio = 200) + 
    theme_half_open())

depth + wind_sd

png("outputs/fig3.png", height = 5, width = 7, units = "in", res = 500)
(depth + wind_sd)
dev.off()

###########################
### Conditional effects ###
###########################

# We can visualize the conditional effect of any covariates by feeding simplified dataframes to the predict function:

nd <- data.frame(
  
  depth = seq(min(df$depth) + 0.2, 
              max(df$depth) - 0.2, 
              length.out = 100), 
  
  depth_scaled = seq(min(df$depth_scaled) + 0.2, 
                     max(df$depth_scaled) - 0.2, 
                     length.out = 100), 
  
  sst_d = seq(min(df$sst_d) + 0.2, 
              max(df$sst_d) - 0.2, 
              length.out = 100), 
  
  year = "2010" # a chosen year
  
)

nd$depth_scaled2 <- nd$depth_scaled^2

p <- predict(density_model, newdata = nd, se_fit = TRUE, re_form = NA)

ggplot(p, aes(sst_d, exp(est), 
              ymin = exp(est - 1.96 * est_se), 
              ymax = exp(est + 1.96 * est_se))) +
  geom_line() + 
  geom_ribbon(alpha = 0.4)

ggplot(p, aes(depth, exp(est), 
              ymin = exp(est - 1.96 * est_se), 
              ymax = exp(est + 1.96 * est_se))) +
  geom_line() + geom_ribbon(alpha = 0.4) +
  # scale_x_continuous(labels = function(x) round(exp(x*df$depth_sd[1] + df$depth_mean[1]))) +
  coord_cartesian(expand = F) +
  labs(x="Depth (m)", y="Biomass density (kg/km2)") 

############################
### Time-varying effects ###
############################

# We could also let the effect of depth vary through time. To do this it helps to give each year a separate intercept.

m4 <- sdmTMB(
  data = df,
  formula = response ~ 0 + as.factor(year),
  time_varying = ~ 0 + depth + depth_scaled + depth_scaled2,
  spde = rea_spde_coast,
  family = tweedie(link = "log"),
  spatial_trend = T,
  time = "year",
  spatiotemporal = "IID"
)
m4
AIC(m4)

nd <- expand.grid(
  
  depth = seq(min(df$depth) + 0.2, 
              max(df$depth) - 0.2, 
              length.out = 50), 
  
  depth_scaled = seq(min(df$depth_scaled) + 0.2, 
                     max(df$depth_scaled) - 0.2, 
                     length.out = 50), 
  
  year = unique(df$year) # all years
)

nd$year = as.character(nd$year)
nd$depth_scaled2 <- nd$depth_scaled^2

p <- predict(m4, newdata = nd, se_fit = TRUE, re_form = NA)

ggplot(p, aes(depth_scaled, exp(est), 
              ymin = exp(est - 1.96 * est_se),
              ymax = exp(est + 1.96 * est_se),
              group = as.factor(year))) +
  geom_line(aes(colour = year), lwd= 1) + 
  geom_ribbon(aes(fill = year), alpha = 0.1) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_continuous(labels = function(x) round(exp(x*df$depth_sd[1] + df$depth_mean[1]))) +
  coord_cartesian(expand = F) +
  labs(x="Depth (m)", y="Biomass density (kg/km2)") 