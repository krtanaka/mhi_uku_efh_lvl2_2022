library(dplyr)

rm(list = ls())

load("outputs/uku_efh_l1.Rdata")

d1 = d1 %>%
  mutate(percentile = percent_rank(est))

load("outputs/uku_efh_l2.Rdata")

d2 = d2 %>%
  mutate(percentile = percent_rank(est))

load("outputs/uku_cpue.RData")

select = dplyr::select

qs = c(95, 75, 25)

for (p in 1:length(qs)) {
  
  # p = 1
  
  if (qs[p] == 95) q = 0.05
  if (qs[p] == 75) q = 0.25
  if (qs[p] == 25) q = 0.75
  
  l1 = d1 %>% 
    subset(est >= quantile(d1$est, probs = q)) %>% 
    select(lon, lat) %>% 
    mutate(cell_count = 1,
           class = "EFH Lvl 1")
  
  l2 = d2 %>% 
    subset(est >= quantile(d2$est, probs = q)) %>% 
    select(lon, lat) %>%
    mutate(cell_count = 1,
           class = "EFH Lvl 2")
  
  cpue = uku_cpue %>% 
    mutate(lon = round(lon, 2),
           lat = round(lat, 2)) %>%
    group_by(lon, lat) %>%
    summarise(cpue_std = mean(cpue_std)) %>%
    subset(cpue_std >= quantile(uku_cpue$cpue_std , probs = q)) %>% 
    select(lon, lat) %>% 
    mutate(cell_count = 1,
           class = "CPUE")

  p1 = rbind(cpue, l1, l2) %>% 
    ggplot(aes(lon, lat, fill = class)) + 
    geom_raster() + 
    scale_fill_viridis_d("") + 
    labs(x = "Longitude", y = "Latitude", 
         title = paste0("Area circumscribing top ", qs[p] , "% of the mode-based EFH (level 1 & 2) and standadized CPUE")) + 
    coord_fixed() + 
    xlim(-161.00, -154.46) + 
    ylim(18.58,22.99) + 
    theme(legend.position = c(0.1, 0.2),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA))
  
  p2 = rbind(cpue, l1, l2) %>% 
    group_by(lon, lat) %>% 
    summarise(cell_count = sum(cell_count)) %>% 
    subset(cell_count >= 3) %>%
    ggplot(aes(lon, lat, fill = "cell_count")) + 
    geom_raster(show.legend = F) + 
    scale_fill_viridis_d("") + 
    labs(x = "Longitude", y = "Latitude", 
         title = paste0("Area containing top ", qs[p] , "% of the mode-based EFH (level 1 & 2) and standadized CPUE")) + 
    coord_fixed() + 
    xlim(-161.00, -154.46) + 
    ylim(18.58,22.99) + 
    theme(legend.position = c(0.1, 0.2),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA))
  
  p1 + p2
  ggsave(last_plot(), filename =  paste0("outputs/efh_cpue_overlap_", qs[p], ".png"), units = "in", height = 6, width = 17)
  
}
