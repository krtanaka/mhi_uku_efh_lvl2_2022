rm(list=ls())

library(ggplot2)
library(patchwork)
library(dplyr)
library(cowplot)

# Tanaka et al. 2022 Uku EFH lvl2 output
df = readRDS(file = "outputs/predictions_dynamic_All_500.rds")

### EFH threshold based on Alaska Fisheries Science Center ###
# EFH identified as the upper 50th percentile of the cumulative distribution of predicted habitat-related abundance from the SDM EFH maps
cumulative_dist <- ecdf(exp(df$data$est))
plot(cumulative_dist, bty = "n", xlim = c(0, 1))
alaska_efh_threashold<- quantile(exp(df$data$est), 0.5, type = 7)
abline(v = alaska_efh_threashold, col = 2)

### EFH threshold based on Tom's efficiency frontier analysis ###
N = 100
Ex = exp(df$data$est); hist(Ex)
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

### map EFH thresholds ###

map = df$data %>% 
  mutate(island_group = case_when(lat > 21.73143 & lat < 22.29684 & lon > -160.3023 & lon < -159.2321 ~ "Ni'ihau-Kaua'i",
                                  lat > 21.19259 & lat < 21.75448 & lon > -158.3432 & lon < -157.5982 ~ "O'ahu",
                                  lat > 20.45111 & lat < 21.26553 & lon > -157.3642 & lon < -155.9242 ~ "Moloka'i-Maui-Lana'i-Kaho'olawe",
                                  lat > 18.86465 & lat < 20.32120 & lon > -156.1109 & lon < -154.7542 ~ "Hawai'i",))

map$island_group = factor(map$island_group, levels=c("Ni'ihau-Kaua'i","O'ahu","Moloka'i-Maui-Lana'i-Kaho'olawe","Hawai'i"))

map = map %>%
  group_by(lon, lat, island_group) %>%
  summarise(est = mean(est)) %>%
  mutate(est = exp(est))

map$alaska_efh = "below threasholds"
map$alaska_efh = ifelse(map$est > alaska_efh_threashold, "EFH above 50th %tile", map$alaska_efh)
map$tom_efh = "below threasholds"
map$tom_efh = ifelse(map$est > tom_efh_thresholds[1], "EFH above 1sd", map$tom_efh)
map$tom_efh = ifelse(map$est > tom_efh_thresholds[2], "EFH above 2sd", map$tom_efh)

cdf = data.frame(x = exp(df$data$est))

p1 = ggplot(cdf, aes(x)) + 
  stat_ecdf() + 
  geom_vline(xintercept = alaska_efh_threashold, color = c("red")) +
  theme_minimal() + 
  xlab("log10(abundanace n per m2)") + 
  ylab("Cumulative proportion") +
  labs(title = "EFH Threshold Setting",
       subtitle = paste0("Upper 50th percentile of the cumulative distribution of predicted abundance in MHI\nSuggested Thresholds: ", paste(round((alaska_efh_threashold), 3), collapse = ", ")," individuals per m2")) +
  scale_x_log10(breaks = c(0, .001, .01, .1, .5, 1, 2, 4))

p2 = map %>% 
  ggplot(aes_string("lon", "lat", fill = "alaska_efh")) +
  geom_tile(aes(height = 0.01, width = 0.01)) +
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
  xlab("log10(abundanace n per m2)") + 
  ylab("Proportion of Total Predicted Abundanace") +
  labs(title = "EFH Threshold Setting:", 
       subtitle = paste0("1SD and 2SD of total abundance in MHI\nSuggested Thresholds: ",
                         paste(round((tom_efh_thresholds), 3), collapse = ", "),
                         " individuals per m2 respectively.")) +
  scale_x_log10(breaks = c(0, .001, .01, .1, .5, 1, 2, 4))

p4 = map %>% 
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

png("outputs/Level2_threshold_setting.jpg", height = 10, width = 15, units = "in", res = 100)
(p1 + p2) / (p3 + p4)
dev.off()

