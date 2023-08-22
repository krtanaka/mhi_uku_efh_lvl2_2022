library(ggplot2)
library(rgdal)
library(colorRamps)
library(patchwork)
library(raster)
library(sf)
library(readr)
library(ggpubr)
library(tidyr)
library(tibble)
library(vegan)
library(cowplot)
library(dplyr, warn.conflicts = F)

options(dplyr.summarise.inform = F)

rm(list = ls())

select = dplyr::select

# load clean eds-enhanced ncrmp data. See 1_prep_ncrmp_data.R
load("data/ncrmp_eds_marian_samoa_all_sp.Rdata") 
df = df %>% subset(region == "MARIAN")

sp_list = unique(df$species)

div = NULL

for (s in 1:length(sp_list)) {
  
  # s = 557
  
  df_i = df %>% 
    mutate(response = ifelse(species == sp_list[s], abund.site, 0)) %>% 
    mutate(response = ifelse(is.na(response), 0, response)) %>% 
    group_by(lon, lat, date, year, depth, island) %>%
    summarise(response = sum(response, na.rm = T)) %>%
    # mutate(species == sp_list[s]) %>%
    na.omit()
  
  colnames(df_i)[7] = sp_list[s]
  
  if (s == 1) {
    
    div = cbind(div, df_i)
    
  } else {
    
    div = cbind(div, df_i[,7])
    
  }
  
  print(sp_list[s])
  
}

div[is.na(div)] <- 0
div$h = diversity(div[,c(7:dim(div)[2])])

save(div, file = paste0("outputs/diversity_index_raw_", Sys.Date(), ".Rdata"))
load("outputs/diversity_index_raw_2023-06-15.Rdata")

div %>% 
  filter(island == "Guam") %>%
  mutate(X = round(lon, 2),
         Y = round(lat, 2)) %>%
  group_by(X, Y, island) %>%
  # group_by(X, Y, island, year) %>%
  summarise(h = mean(h, na.rm = T)) %>%
  ggplot(aes(X, Y, fill = h)) + 
  geom_raster() +
  # coord_fixed() +
  theme_cowplot() + 
  labs(x = "Longitude", y = "Latitude") +
  # facet_wrap(~year, scales = "free") +
  facet_wrap(~island, scales = "free") +
  scale_fill_gradientn(colours = matlab.like(100), expression(italic("H")), trans = "sqrt") + 
  theme(legend.position = c(0.1, 0.85),
        legend.background = element_blank())

ggsave(last_plot(), filename = "outputs/diversity_map_raw.png", height = 7, width = 6)

div %>% 
  filter(island == "Guam") %>%
  mutate(
    # depth = round(depth, 0)
    depth = plyr::round_any(depth, 0.5, f = round)
  ) %>%
  group_by(depth) %>% 
  mutate(median_h = mean(h)) %>% 
  ggplot(aes(x = factor(depth), y = h, fill = median_h)) +
  gg.layers::geom_boxplot2(width = 0.8, width.errorbar = 0, show.legend = F) + 
  scale_fill_gradientn(colours = matlab.like(100), "H", trans = "sqrt") +
  # scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
  labs(x = "Depth (m)", y = "H per 100 sq.m") 

div %>% 
  filter(island == "Guam") %>%
  mutate(
    # depth = round(depth, 0)
    depth = plyr::round_any(depth, 0.5, f = round)
  ) %>%
  group_by(depth, island) %>%
  summarise(h = mean(h, na.rm = T)) %>%
  ggplot(aes(depth, h, fill = h)) + 
  geom_smooth(method = "loess", span = 1, color = "gray50", show.legend = F) + 
  geom_point(shape = 21, show.legend = F, size = 5) +
  scale_fill_gradientn(colours = matlab.like(100), expression(italic("H")), trans = "sqrt") + 
  theme_cowplot() + 
  facet_wrap(~island, scales = "free") +
  labs(x = "Depth (m)", y = bquote(italic("H") ~ "per 100 sq.m"))

ggsave(last_plot(), filename = "outputs/diversity_depth_raw.png", height = 7, width = 6)
