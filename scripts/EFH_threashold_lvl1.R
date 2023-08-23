library(raster)
library(dplyr)
library(ggplot2)
library(patchwork)

rm(list = ls())

# Franklin 2021 Uku EFH lvl1 output
r = raster("/Users/kisei.tanaka/Desktop/efh_sh_dp.tif")
# load("data/erik.Rdata")

df = rasterToPoints(r) %>% 
  as.data.frame() %>%
  # sample_frac(0.01) %>%
  na.omit()

### EFH threshold based on Alaska Fisheries Science Center ###
# EFH identified as the upper 50th percentile of the cumulative distribution of predicted habitat-related occurrence
cumulative_dist <- ecdf(df$efh_sh_dp)
plot(cumulative_dist, bty = "n", xlim = c(0, 1), pch = ".")
alaska_efh_threashold <- quantile(df$efh_sh_dp, 0.5, type = 7)
abline(v = alaska_efh_threashold, col = 2)

### EFH threshold based on Tom's efficiency frontier analysis ###
### doesn't really make sense with presence-absence data ###
N = 100
Ex = df$efh_sh_dp;  hist(Ex)
sd_1 <- mean(Ex) + sd(Ex)
sd_2 <- mean(Ex) + sd(Ex)*2
BEtot = sum(Ex, na.rm = T); BEtot
Qex = quantile(x = Ex, seq(0, 1, length.out = N)); Qex
SEQex = seq(min(Ex), max(Ex), length.out = N); SEQex
DF = data.frame(Threshold_E = Qex, PEb = NA)#,Threshold_X=Qx,Pb=NA)
for (i in 1:N){
  
  #  DF$Pb[i]=sum(X[X<=DF$Threshold_E[i]])/Btot
  
  DF$PEb[i] = sum(Ex[Ex >= DF$Threshold_E[i]])/BEtot
  
  if(i%%10 == 0) print(i)
}

vec <- DF$PEb
mean_value <- mean(vec)
sd_value <- sd(vec)
sd_1 <-  mean(vec) + sd(vec)
sd_2 <-  mean(vec) + 2*sd(vec)
percentile_1 <- pnorm(sd_1, mean = mean_value, sd = sd_value)
percentile_2 <- pnorm(sd_2, mean = mean_value, sd = sd_value)

tom_efh_thresholds = c(DF[which.min(abs(DF$PEb-percentile_2)), "Threshold_E"],
                       DF[which.min(abs(DF$PEb-percentile_1)), "Threshold_E"])

# map EFH thresholds
map = df %>% 
  mutate(lon = x,
         lat = y,
         est = efh_sh_dp) %>% 
  mutate(island_group = case_when(lat > 21.73143 & lat < 22.29684 & lon > -161 & lon < -159.2321 ~ "Ni'ihau-Kaua'i",
                                  lat > 21.2 & lat < 22 & lon > -158.4 & lon < -157.5 ~ "O'ahu",
                                  lat > 20.45111 & lat < 21.35 & lon > -158.3432 & lon < -155.9242 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                  lat > 18.86465 & lat < 20.32120 & lon > -156.1109 & lon < -154.7542 ~ "Hawai'i",))

map$island_group = factor(map$island_group, levels=c("Ni'ihau-Kaua'i","O'ahu","Moloka'i-Maui-Lana'i-Kaho'olawe","Hawai'i"))

map = map %>% 
  mutate(lon = round(lon, 2),
         lat = round(lat, 2)) %>%
  group_by(lon, lat, island_group) %>% 
  summarise(est = mean(est))

map %>% 
  subset(island_group %in% c("Ni'ihau-Kaua'i","O'ahu","Moloka'i-Maui-Lana'i-Kaho'olawe","Hawai'i")) %>% 
  ggplot(aes_string("lon", "lat", fill = "est")) +
  geom_raster() 

map$alaska_efh = "below threasholds"
map$alaska_efh = ifelse(map$est > alaska_efh_threashold, "EFH above 50th %tile", map$alaska_efh)
map$tom_efh = "below threasholds"
map$tom_efh = ifelse(map$est > tom_efh_thresholds[1], "EFH above 1sd", map$tom_efh)
map$tom_efh = ifelse(map$est > tom_efh_thresholds[2], "EFH above 2sd", map$tom_efh)

cdf = data.frame(x = df$efh_sh_dp)

p1 = ggplot(cdf, aes(x)) + 
  stat_ecdf() + 
  geom_vline(xintercept = alaska_efh_threashold, color = c("red")) +
  theme_minimal() + 
  xlab("Probability of Occurance") + 
  ylab("Cumulative proportion") +
  labs(title = "EFH Threshold Setting",
       subtitle = paste0("Upper 50th percentile of the cumulative distribution of predicted occurance in MHI\nSuggested Thresholds: ", paste(round((alaska_efh_threashold), 3), collapse = ", ")))

p2 = map %>% 
  subset(island_group %in% c("Ni'ihau-Kaua'i","O'ahu","Moloka'i-Maui-Lana'i-Kaho'olawe","Hawai'i")) %>% 
  ggplot(aes_string("lon", "lat", fill = "alaska_efh")) +
  geom_raster() +
  facet_wrap(~island_group, scales = "free", ncol = 2) +
  scale_fill_discrete("", breaks = c('EFH above 50th %tile', 'below threasholds')) +
  xlab("Longitude (dec deg)") +
  ylab("Latitude (dec deg)") + 
  theme_minimal() + 
  theme(aspect.ratio = 0.8,
        legend.key.size = unit(0.5, "cm"),
        panel.background = element_rect(fill = "gray10", colour = "gray10"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20"))

p3 = DF %>% 
  ggplot(aes(Threshold_E, PEb)) +
  geom_point() + 
  geom_path()+
  geom_vline(xintercept = tom_efh_thresholds, color = c("red","blue")) +
  theme_minimal() + 
  xlab("Probability of Occurance") + 
  ylab("Proportion of Total Predicted Occurance") +
  labs(title = "EFH Threshold Setting:", 
       subtitle = paste0("1SD and 2SD of total occurance in MHI\nSuggested Thresholds: prob.occu =  ",
                         paste(round((tom_efh_thresholds), 3), collapse = ", "),
                         " respectively."))

p4 = map %>% 
  subset(island_group %in% c("Ni'ihau-Kaua'i","O'ahu","Moloka'i-Maui-Lana'i-Kaho'olawe","Hawai'i")) %>% 
  ggplot(aes_string("lon", "lat", fill = "tom_efh")) +
  geom_tile(aes(height = 0.01, width = 0.01)) +
  facet_wrap(~island_group, scales = "free", ncol = 2) + 
  scale_fill_discrete("", breaks = c('EFH above 2sd', 'EFH above 1sd', 'below threasholds')) +
  xlab("Longitude (dec deg)") +
  ylab("Latitude (dec deg)") + 
  theme_minimal() + 
  theme(aspect.ratio = 0.8,
        legend.key.size = unit(0.5, "cm"),
        panel.background = element_rect(fill = "gray10", colour = "gray10"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20"))

png("outputs/Level1_threshold_setting_rev.jpg", height = 10, width = 15, units = "in", res = 100)
(p1 + p2) / (p3 + p4)
dev.off()
