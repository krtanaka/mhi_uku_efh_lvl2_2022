library(ggplot2)
library(dplyr)
library(sdmTMB)

rm(list = ls())

load("outputs/clean_df.RData")

pcod1 = df
pcod1$year = as.integer(pcod1$year)
pcod1$density = pcod1$response

pcod_spde <- make_mesh(pcod1, c("X", "Y"), cutoff = 30); plot(pcod_spde)


# depth -------------------------------------------------------------------
m <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + depth + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)

print(m)

depth <- data.frame(
  depth = seq(min(pcod1$depth) + 0.2, 
              max(pcod1$depth) - 0.2, length.out = 50), 
  
  year = 2015L # a chosen year
)

p <- predict(m, newdata = depth, se_fit = TRUE, re_form = NA)
colnames(p)[1] = c("var")
p$cov = "depth"

ggplot(p, aes(var, exp(est), 
              ymin = exp(est - 1.96 * est_se), 
              ymax = exp(est + 1.96 * est_se))) +
  geom_line() + 
  geom_ribbon(alpha = 0.4) +
  theme_classic()


# sst ---------------------------------------------------------------------
m1 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + sst_d + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m2 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + sst_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m3 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + q95_sst_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m4 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + q05_sst_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m5 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + sd_sst_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)

sst_d <- data.frame(
  sst_d = seq(min(pcod1$sst_d) + 0.2, 
              max(pcod1$sst_d) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
sst_m <- data.frame(
  sst_m = seq(min(pcod1$sst_m) + 0.2, 
              max(pcod1$sst_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
q95_sst_m <- data.frame(
  q95_sst_m = seq(min(pcod1$q95_sst_m) + 0.2, 
                  max(pcod1$q95_sst_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
q05_sst_m <- data.frame(
  q05_sst_m = seq(min(pcod1$q05_sst_m) + 0.2, 
                  max(pcod1$q05_sst_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
sd_sst_m <- data.frame(
  q05_sst_m = seq(min(pcod1$sd_sst_m) + 0.2, 
                  max(pcod1$sd_sst_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)

p1 <- predict(m1, newdata = sst_d, se_fit = TRUE, re_form = NA); colnames(p1)[1] = c("var"); p1$cov = "sst_d"
p2 <- predict(m2, newdata = sst_m, se_fit = TRUE, re_form = NA); colnames(p2)[1] = c("var"); p2$cov = "sst_m"
p3 <- predict(m3, newdata = q95_sst_m, se_fit = TRUE, re_form = NA); colnames(p3)[1] = c("var"); p3$cov = "q95_sst_m"
p4 <- predict(m4, newdata = q05_sst_m, se_fit = TRUE, re_form = NA); colnames(p4)[1] = c("var"); p4$cov = "q05_sst_m"
p5 <- predict(m5, newdata = sd_sst_m, se_fit = TRUE, re_form = NA); colnames(p5)[1] = c("var"); p5$cov = "sd_sst_m"

p = rbind(p1, p2, p3, p4)

p %>% 
  ggplot(aes(var, exp(est), 
             ymin = exp(est - 1.96 * est_se), 
             ymax = exp(est + 1.96 * est_se),
             color = cov, 
             fill = cov)) +
  geom_line() + 
  geom_ribbon(alpha = 0.4) + 
  facet_wrap(.~ cov)


# chl_a -------------------------------------------------------------------
m1 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + chla_8d + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m2 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + chla_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m3 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + q95_chla_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m4 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + q05_chla_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m5 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + sd_chla_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)

chla_8d <- data.frame(
  chla_8d = seq(min(pcod1$chla_8d) + 0.2, 
                max(pcod1$chla_8d) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
chla_m <- data.frame(
  chla_m = seq(min(pcod1$chla_m) + 0.2, 
               max(pcod1$chla_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
q95_chla_m <- data.frame(
  q95_chla_m = seq(min(pcod1$q95_chla_m) + 0.2, 
                   max(pcod1$q95_chla_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
q05_chla_m <- data.frame(
  q05_chla_m = seq(min(pcod1$q05_chla_m) + 0.2, 
                   max(pcod1$q05_chla_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
sd_chla_m <- data.frame(
  q05_chla_m = seq(min(pcod1$sd_chla_m) + 0.2, 
                   max(pcod1$sd_chla_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)

p1 <- predict(m1, newdata = chla_d, se_fit = TRUE, re_form = NA); colnames(p1)[1] = c("var"); p1$cov = "chla_8d"
p2 <- predict(m2, newdata = chla_m, se_fit = TRUE, re_form = NA); colnames(p2)[1] = c("var"); p2$cov = "chla_m"
p3 <- predict(m3, newdata = q95_chla_m, se_fit = TRUE, re_form = NA); colnames(p3)[1] = c("var"); p3$cov = "q95_chla_m"
p4 <- predict(m4, newdata = q05_chla_m, se_fit = TRUE, re_form = NA); colnames(p4)[1] = c("var"); p4$cov = "q05_chla_m"
p5 <- predict(m5, newdata = sd_chla_m, se_fit = TRUE, re_form = NA); colnames(p5)[1] = c("var"); p5$cov = "sd_chla_m"

p = rbind(p1, p2, p3, p4)

p %>% 
  ggplot(aes(var, exp(est), 
             ymin = exp(est - 1.96 * est_se), 
             ymax = exp(est + 1.96 * est_se),
             color = cov, 
             fill = cov)) +
  geom_line() + 
  geom_ribbon(alpha = 0.4)
  # facet_wrap(.~ cov)


# wind --------------------------------------------------------------------
m1 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + wind_d + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m2 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + wind_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m3 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + q95_wind_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m4 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + q05_wind_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)
m5 <- sdmTMB(
  data = pcod1,
  formula = density ~ 0 + sd_wind_m + as.factor(year),
  time = "year", spde = pcod_spde,
  family = tweedie(link = "log"),
  silent = F
)

wind_d <- data.frame(
  wind_d = seq(min(pcod1$wind_d) + 0.2, 
                max(pcod1$wind_d) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
wind_m <- data.frame(
  wind_m = seq(min(pcod1$wind_m) + 0.2, 
               max(pcod1$wind_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
q95_wind_m <- data.frame(
  q95_wind_m = seq(min(pcod1$q95_wind_m) + 0.2, 
                   max(pcod1$q95_wind_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
q05_wind_m <- data.frame(
  q05_wind_m = seq(min(pcod1$q05_wind_m) + 0.2, 
                   max(pcod1$q05_wind_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)
sd_wind_m <- data.frame(
  q05_wind_m = seq(min(pcod1$sd_wind_m) + 0.2, 
                   max(pcod1$sd_wind_m) - 0.2, length.out = 100), 
  
  year = 2015L # a chosen year
)

p1 <- predict(m1, newdata = wind_d, se_fit = TRUE, re_form = NA); colnames(p1)[1] = c("var"); p1$cov = "wind_d"
p2 <- predict(m2, newdata = wind_m, se_fit = TRUE, re_form = NA); colnames(p2)[1] = c("var"); p2$cov = "wind_m"
p3 <- predict(m3, newdata = q95_wind_m, se_fit = TRUE, re_form = NA); colnames(p3)[1] = c("var"); p3$cov = "q95_wind_m"
p4 <- predict(m4, newdata = q05_wind_m, se_fit = TRUE, re_form = NA); colnames(p4)[1] = c("var"); p4$cov = "q05_wind_m"
p5 <- predict(m5, newdata = sd_wind_m, se_fit = TRUE, re_form = NA); colnames(p5)[1] = c("var"); p5$cov = "sd_wind_m"

p = rbind(p1, p2, p3, p4)

p %>% 
  ggplot(aes(var, exp(est), 
             ymin = exp(est - 1.96 * est_se), 
             ymax = exp(est + 1.96 * est_se),
             color = cov, 
             fill = cov)) +
  geom_line() + 
  geom_ribbon(alpha = 0.4)
