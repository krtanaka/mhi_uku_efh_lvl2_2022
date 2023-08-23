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

# filter by islands, region
# choose abundance as response variable 
# filter by Uku (APVI)

df = df %>% 
  # subset(ISLAND %in% islands) %>% 
  # subset(REGION == "MHI") %>%
  subset(REGION %in% c("MHI", "NWHI")) %>%
  subset(Response == "Abund_m2") %>% 
  select(id, names(df[,1:18]), names(select(df, contains("APVI"))))

df_MHI = df %>% subset(REGION == "MHI")
df_NWHI = df %>% subset(REGION == "NWHI")

# look at size distributions
size_bins_MHI = df_MHI[,c(21:31)] %>% 
  gather() %>% 
  mutate(key = gsub("APVI_", "", key)) %>% 
  group_by(key) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  mutate(freq = value/sum(value),
         region = "MHI") 

size_bins_NWHI = df_NWHI[,c(21:31)] %>% 
  gather() %>% 
  mutate(key = gsub("APVI_", "", key)) %>% 
  group_by(key) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  mutate(freq = value/sum(value),
         region = "NWHI") 

size_bins = rbind(size_bins_MHI, size_bins_NWHI)

size_bins$key <- factor(size_bins$key, levels = c("[0,10]",
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
(f1a = size_bins %>% 
    ggplot(aes(x = key, y = freq, fill = region)) +
    geom_bar(stat = "identity", position=position_dodge(), show.legend = T) + 
    xlab("Size_bins (cm)") + ylab("Proportion (%)") + 
    theme_classic() + 
    # scale_fill_gradientn("", colours = matlab.like(100)) +
    # scale_fill_viridis_c("") + 
    scale_fill_discrete("") + 
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10)) + 
    labs(tag = "(a)"))


df = df %>%
  select(names(df[,1:20])) %>% 
  mutate(DATE = as.numeric(as.POSIXct(df$DATE_)),
         YEAR = substr(DATE_, 1, 4)) %>% 
  group_by(LON, LAT, DATE, YEAR, REGION) %>%
  summarise(response = sum(APVI, na.rm = T)*100) %>%
  # subset(response < quantile(response, prob = 0.999)) %>%
  na.omit()

(f1b = df %>%
    group_by(YEAR, REGION) %>%
    summarise(sd = sd(response, na.rm = T),
              response = mean(response, na.rm = T)) %>% 
    ggplot(aes(YEAR, response)) +
    geom_pointrange(
      aes(ymin = ifelse(response - sd < 0, 0, response - sd), 
          ymax = response+sd, color = REGION),
      position = position_dodge(0.3)) + 
    # scale_color_viridis_d("", end = 0.8) +
    # scale_color_manual(values = matlab.like(18), "") + 
    scale_color_discrete("") + 
    ylab("Mean individuals per 100 sq.m") + xlab("") + 
    theme_classic() + 
    labs(tag = "(b)"))

f1a + f1b

png("outputs/uku_mhi_nwhi.png", units = "in", height = 5, width = 15, res = 500)
(f1a + f1b)
dev.off()

(df %>% 
    ggplot(aes(LON, LAT)) + 
    geom_point(aes(size = response, fill = response, color = response), shape = 21, alpha = 0.7) +
    scale_color_gradientn(colours = matlab.like(100), guide = "legend") +
    scale_fill_gradientn(colours = matlab.like(100), guide = "legend") +
    theme_half_open() +
    facet_wrap(.~REGION, scales = "free", ncol = 4) +
    labs(x = expression(paste("Longitude ", degree, "W", sep = "")),
         y = expression(paste("Latitude ", degree, "N", sep = ""))) +
    guides(color = guide_legend(expression("Individuals per 100" ~ m^2~"")), 
           fill = guide_legend(expression("Individuals per 100" ~ m^2~"")),
           size = guide_legend(expression("Individuals per 100" ~ m^2~""))) + 
    theme(legend.position = "bottom") +
    labs(tag = "(c)"))
