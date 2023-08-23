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

# top 5 taxa by abundance (Abund_m2) or biomass (Biom_gm2)
taxa_abund = df %>% 
  subset(ISLAND %in% islands) %>% 
  subset(REGION == "MHI") %>%
  subset(Response == "Abund_m2") 
taxa_abund = taxa_abund[,19:731]
taxa_abund = as.data.frame(t(taxa_abund))
data.table::setDT(taxa_abund, keep.rownames = TRUE)[]
taxa_abund$sum = rowSums(taxa_abund[,2:2969], na.rm = T)
taxa_abund$mean = rowMeans(taxa_abund[,2:2969], na.rm = T)
taxa_abund_sum = taxa_abund[,c("rn","sum")]
taxa_abund_mean = taxa_abund[,c("rn","mean")]
colnames(taxa_abund_sum) = c("SPECIES", "SUM")
colnames(taxa_abund_mean) = c("SPECIES", "MEAN")
taxa = merge(taxa_abund_sum, taxa_abund_mean)

taxa = merge(taxa, fish_info[,2:9])

taxa %>% 
  group_by(COMMONNAME) %>%
  summarise(n = sum(SUM, na.rm = T)) %>%
  # summarise(n = sum(MEAN, na.rm = T)) %>%
  mutate(freq = n/sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(5)

library(dplyr)

options(scipen = 999)

TROPHIC_group = c("PISCIVORE", "PLANKTIVORE", "PRIMARY", "SECONDARY")

TROPHIC_group_table = NULL

for (t in 1:length(TROPHIC_group)) {
  
  # t = 1
  
  df_t = taxa %>% 
    subset(TROPHIC_MONREP == TROPHIC_group[t]) %>% 
    group_by(COMMONNAME) %>% 
    summarise(n = sum(CPUE, na.rm = T),
              Mean_CPUE = mean(CPUE, na.rm = T)) %>% 
    mutate(freq = n/sum(n)) %>% 
    mutate(Cumulative_sum = cumsum(freq),
           Total_CPUE = n/1000,
           Mean_CPUE = round(Mean_CPUE/1000, 4),
           Proportion = freq, 
           Functional_group = TROPHIC_group[t])
  
  TROPHIC_group_table = rbind(TROPHIC_group_table, df_t)
  
}

png("/Users/kisei.tanaka/Desktop/functional_group.png", height = 10, width = 20, units = "in", res = 300)
(TROPHIC_group_table %>% 
    subset(Proportion > 0.02) %>%
    ggplot(aes(Proportion, reorder(TAXONNAME, Proportion), fill = Proportion)) +  
    geom_bar(stat = "identity") + 
    guides(color = guide_legend(), size = guide_legend()) + 
    scale_fill_gradientn(colours = matlab.like(10)) + 
    facet_wrap(.~Functional_group, ncol = 4, scales = "free") + 
    ylab("") + 
    theme_half_open() + 
    theme(axis.text.y = element_text(face = "italic")))
dev.off()

