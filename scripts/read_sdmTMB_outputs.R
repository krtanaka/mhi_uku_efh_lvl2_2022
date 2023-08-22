library(sdmTMB)
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
library(ggdark)
library(cowplot)
library(ggjoy)
library(forcats)
library(dplyr, warn.conflicts = F)

options(dplyr.summarise.inform = F)

rm(list = ls())

select = dplyr::select

files = list.files("G:/sdmtmb_outputs/outputs/species/0.05/", full.names = T, pattern = ".rds")

islands = c("Guam", "Rota", "Aguijan", "Tinian", "Saipan", "Sarigan", "Pagan", "Asuncion", "Farallon_de_Pajaros", "Maug", "Agrihan", "Alamagan", "Guguan")

islands = "All"

coef = NULL
sanity = NULL
residual = NULL
obs_pred = NULL
depth = NULL
grid = NULL

for (isl in 1:length(islands)) {
  
  # isl = 1
  
  files_i <- files[grepl( islands[isl], files)]
  
  coef_i = NULL
  sanity_i = NULL
  residual_i = NULL
  obs_pred_i = NULL
  depth_i = NULL
  grid_i = NULL
  
  for (s in seq_along(files_i)) {
    
    # s = 1
    
    df_i_coef <- readRDS(files_i[s])$coef; df_i_coef$species = strsplit(files_i[s], "_")[[1]][2]
    df_i_sanity <- readRDS(files_i[s])$sanity
    df_i_residual <- readRDS(files_i[s])$residuals; df_i_residual$species = strsplit(files_i[s], "_")[[1]][2]
    df_i_obs_pred <- readRDS(files_i[s])$obs_pred
    df_i_depth <- readRDS(files_i[s])$depth; df_i_depth$species = strsplit(files_i[s], "_")[[1]][2]
    df_i_grid <- readRDS(files_i[s])$grid
    
    coef_i = rbind(coef_i, df_i_coef)
    depth_i = rbind(depth_i, df_i_depth)
    residual_i = rbind(residual_i, df_i_residual)
    obs_pred_i = rbind(obs_pred_i, df_i_obs_pred)
    
    if (s == 1) {
      
      sanity_i = rbind(sanity_i, df_i_sanity)
      grid_i = rbind(grid_i, df_i_grid)
      
    } else {
      
      sanity_i = cbind(sanity_i, df_i_sanity)
      grid_i = left_join(grid_i, df_i_grid)
      
    }
    
    print(s)
    
  }
  
  coef_i$island = islands[isl]
  depth_i$island = islands[isl]
  residual_i$island = islands[isl]
  obs_pred_i$island = islands[isl]
  sanity_i$island = islands[isl]
  
  grid_i[is.na(grid_i)] <- 0
  grid_i$h = diversity(grid_i[,c(6:dim(grid_i)[2])])
  grid_i$n = length(grid_i[,6:dim(grid_i)[2]])
  grid_i$r = specnumber(grid_i[,6:dim(grid_i)[2]])
  
  grid_i = grid_i %>% select(X, Y, year, depth, island, h, r, n)
  
  coef = rbind(coef, coef_i)
  sanity = rbind(sanity, sanity_i)
  residual = rbind(residual, residual_i)
  obs_pred = rbind(obs_pred, obs_pred_i)
  depth = rbind(depth, depth_i)
  grid = rbind(grid, grid_i)
  
}

sdmtmb_output = list(coef = coef, 
                     sanity = sanity, 
                     residual = residual,
                     obs_pred = obs_pred,
                     depth = depth, 
                     grid = grid)

save(sdmtmb_output, file = paste0("outputs/diversity_index_", Sys.Date(), "'.Rdata"))
load(paste0("outputs/diversity_index_", Sys.Date(), "'.Rdata"))

# map
sdmtmb_output$grid %>% 
  filter(island == "Guam") %>%
  mutate(X = round(X, 0),
         Y = round(Y, 0)) %>%
  group_by(X, Y, island) %>%
  # group_by(X, Y, island, year) %>%
  summarise(h = mean(h, na.rm = T)) %>%
  ggplot(aes(X, Y, fill = h, size = h)) + 
  geom_raster() +
  # coord_fixed() +
  theme_cowplot() + 
  labs(x = "Longitude", y = "Latitude") +
  # facet_wrap(~year, scales = "free") +
  facet_wrap(~island, scales = "free") +
  scale_fill_gradientn(colours = matlab.like(100), expression(italic("H")), trans = "sqrt") + 
  theme(legend.position = c(0.1, 0.85),
        legend.background = element_blank())

ggsave(last_plot(), filename = "outputs/diversity_map.png", height = 7, width = 6)

# depth
sdmtmb_output$grid %>% 
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

sdmtmb_output$grid %>% 
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

ggsave(last_plot(), filename = "outputs/diversity_depth.png", height = 7, width = 6)

# depth - island
sdmtmb_output$grid %>% 
  mutate(
    depth = round(depth, 0)
    # depth = plyr::round_any(depth, 0.5, f = round)
  ) %>%
  group_by(depth, island) %>%
  summarise(h = mean(h, na.rm = T)) %>%
  ggplot(aes(depth, h, fill = h)) + 
  geom_smooth(method = "loess", span = 1, color = "gray50", show.legend = F) + 
  geom_point(size = 3, shape = 21, alpha = 0.8, show.legend = F) +
  scale_fill_gradientn(colours = matlab.like(100), expression(italic("H")), trans = "sqrt") + 
  theme_cowplot() + 
  facet_wrap(~island, scales = "free") +
  labs(x = "Depth (m)", y = bquote(italic("H") ~ "per 100 sq.m"))

# depth - all islands
sdmtmb_output$grid %>% 
  mutate(
    depth = round(depth, 1)
    # depth = plyr::round_any(depth, 0.5, f = round)
  ) %>%
  group_by(depth) %>%
  summarise(h = mean(h, na.rm = T)) %>%
  ggplot(aes(depth, h, fill = h)) + 
  geom_smooth(method = "loess", span = 1, color = "gray50", show.legend = F) + 
  geom_point(size = 5, shape = 21, alpha = 0.8, show.legend = F) +
  scale_fill_gradientn(colours = matlab.like(100), expression(italic("H")), trans = "sqrt") + 
  theme_cowplot() + 
  labs(x = "Depth (m)", y = bquote(italic("H") ~ "per 100 sq.m"))

# depth - guam vs. CNMI
sdmtmb_output$grid %>% 
  mutate(
    depth = round(depth, 1),
    # depth = plyr::round_any(depth, 0.5, f = round),
    guam_or_not = ifelse(island == "Guam", "Guam", "CNMI")) %>% 
  group_by(depth, guam_or_not) %>%
  summarise(mean = mean(h, na.rm = TRUE),
            sd = sd(h, na.rm = TRUE),
            n = n()) %>%
  ggplot(aes(depth, mean, fill = guam_or_not, color = guam_or_not)) + 
  geom_smooth(method = "loess", span = 1, show.legend = F) + 
  # geom_errorbar(aes(ymin = mean - sd/sqrt(n),
  #                   ymax = mean + sd/sqrt(n)),
  #               position = position_dodge(width = 0.2),
  #               width = 0, size = 0,
  #               show.legend = F) +
  geom_point(position = position_dodge(width = 0.2), size = 5, shape = 21, alpha = 0.2) +
  scale_color_manual("", values = c("blue", "red")) + 
  scale_fill_manual("", values = c("blue", "red")) + 
  theme_cowplot() + 
  labs(x = "Depth (m)", y = bquote(italic("H") ~ "per 100 sq.m"))

# islands boxplots
sdmtmb_output$grid %>% 
  group_by(island) %>%
  mutate(med_h = median(h)) %>% 
  ggplot(aes(x = reorder(island, h, FUN = median), y = h)) +
  gg.layers::geom_boxplot2(aes(fill = med_h), show.legend = F, width.errorbar = 0) +
  # stat_summary(fun.y = mean, colour = "#39FF14", geom = "point", size = 2) +
  labs(x = "", y = bquote(italic("H") ~ "per 100 sq.m")) + 
  theme_cowplot() + 
  coord_flip() + 
  scale_fill_gradientn(colours = matlab.like(13)) + 
  scale_color_gradientn(colours = matlab.like(13))

# islands joyplot
sdmtmb_output$grid %>%
  group_by(island) %>%
  mutate(h = mean(h)) %>%
  ggplot(aes(x = h, y = fct_reorder(island, h) , fill = h)) +
  geom_joy(scale = 3, alpha = 0.8, size = 0.5, bandwidth = 0.05, show.legend = F) +
  labs(y = "", x = bquote(italic("H") ~ "per 100 sq.m")) + 
  coord_fixed(ratio = 0.1) +
  theme_cowplot() + 
  scale_fill_gradientn(colours = matlab.like(13)) + 
  scale_color_gradientn(colours = matlab.like(13)) 

# lat - all islands
sdmtmb_output$grid %>% 
  mutate(
    # Y = round(Y, 0)
    Y = plyr::round_any(Y, 0.5, f = round)
  ) %>%
  group_by(Y) %>%
  summarise(h = mean(h, na.rm = T)) %>%
  ggplot(aes(Y, h, fill = h)) + 
  geom_smooth(method = "lm", span = 1, color = "gray50", show.legend = F) + 
  geom_point(size = 5, shape = 21, alpha = 0.8, show.legend = F) +
  scale_fill_gradientn(colours = matlab.like(100), expression(italic("H")), trans = "sqrt") + 
  theme_cowplot() + 
  labs(x = "Northings (km)", y = bquote(italic("H") ~ "per 100 sq.m"))

sdmtmb_output$residual %>% 
  sample_frac(0.1) %>% 
  ggplot(aes(sample = resids)) +
  stat_qq(shape = 21, show.legend = F) +
  stat_qq_line() +
  xlim(c(-5, 5)) + 
  ylim(c(-5, 5)) + 
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme(aspect.ratio = 1)

sdmtmb_output$residual %>%
  ggplot(aes(x = resids, y = ..count.., fill = ..x..)) +
  geom_histogram(show.legend = F, bins = 100) +
  # facet_wrap(~species) + 
  xlim(c(-8, 8)) + 
  scale_fill_gradient2(trans = 'reverse', "")

sdmtmb_output$residual %>%
  mutate(X = round(X, 0),
         Y = round(Y, 0)) %>% 
  group_by(X, Y) %>% 
  summarise(resids = mean(resids)) %>% 
  # sample_frac(0.1) %>% 
  ggplot(aes(X, Y, fill = resids, size = resids)) +
  geom_point(shape = 21, alpha = 0.8,  show.legend = F) +
  coord_fixed() + 
  scale_fill_gradient2(trans = 'reverse', "") +
  scale_color_gradient2(trans = 'reverse', "")

sdmtmb_output$obs_pred %>% 
  # sample_frac(0.5) %>%
  ggplot(aes(response, exp(est), 
             color = exp(est),
             fill = exp(est))) + 
  geom_smooth(method = "lm", se = F, color = "gray10", size = 0.5) +
  # geom_point(shape = 21, alpha = 0.5, size = 3, show.legend = F) +
  geom_scattermore(pointsize  = 5, alpha = 0.5) + 
  scale_x_continuous(limits = range(sdmtmb_output$obs_pred$response)) +
  scale_y_continuous(limits = range(sdmtmb_output$obs_pred$response)) +
  scale_fill_gradientn(colours = matlab.like(10), trans = "sqrt") +
  scale_color_gradientn(colours = matlab.like(10), trans = "sqrt") +
  labs(x = "Observed (n/100 sq.m)", y = "Predicted (n/100 sq.m)") +
  geom_abline(intercept = 0, slope = 1, color = "gray20", linetype = "dotted") +
  theme(aspect.ratio = 1)
