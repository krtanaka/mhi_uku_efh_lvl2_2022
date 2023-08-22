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

load("outputs/clean_df.RData")

zone <- (floor((df$lon[1] + 180)/6) %% 60) + 1
xy_utm = as.data.frame(cbind(utm = project(as.matrix(df[, c("lon", "lat")]), paste0("+proj=utm +units=km +zone=", zone))))
colnames(xy_utm) = c("X", "Y")
df = cbind(df, xy_utm)
plot(xy_utm, pch = ".", bty = 'n')
rm(xy_utm)

n_knots = c(1000, 500, 300, 100)[1]

density_model_static = readRDS(paste0("outputs/density_model_static_", n_knots, ".rds"))
density_model_dynamic = readRDS(paste0("outputs/density_model_dynamic_", n_knots, ".rds"))
density_model_full = readRDS(paste0("outputs/density_model_full_", n_knots, ".rds"))

#################
### Residuals ###
#################

# Get DHARMa residuals
# The `simulated_response` argument is first so the output from simulate() can be piped to dharma_residuals():
# simulate(fit, nsim = 500) %>% dharma_residuals(fit)
s1 <- simulate(density_model_static, nsim = 500)
s2 <- simulate(density_model_dynamic, nsim = 500)
s3 <- simulate(density_model_full, nsim = 500)

dharma_residuals(s1, density_model_static)
dharma_residuals(s2, density_model_dynamic)
dharma_residuals(s3, density_model_full)

r1 <- dharma_residuals(s1, density_model_static, plot = FALSE)
r2 <- dharma_residuals(s2, density_model_dynamic, plot = FALSE)
r3 <- dharma_residuals(s3, density_model_full, plot = FALSE)

head(r1); head(r2); head(r3)

par(pty = "s", mfrow = c(1,3))
plot(r1$expected, r1$observed, pch = ".", bty = "n"); abline(a = 0, b = 1)
plot(r2$expected, r2$observed, pch = ".", bty = "n"); abline(a = 0, b = 1)
plot(r3$expected, r3$observed, pch = ".", bty = "n"); abline(a = 0, b = 1)

df$residuals_1 <- residuals(density_model_static)
df$residuals_2 <- residuals(density_model_dynamic)
df$residuals_3 <- residuals(density_model_full)

png(paste0('outputs/qq_', n_knots, '.png'), width = 12, height = 5, units = "in", res = 500)
par(pty = "s", mfrow = c(1,3))
qqnorm(df$residuals_1, ylim = c(-5, 5), xlim = c(-5, 5), bty = "n", pch = 20, main = "static model"); abline(a = 0, b = 1)
qqnorm(df$residuals_2, ylim = c(-5, 5), xlim = c(-5, 5), bty = "n", pch = 20, main = "dynamic model"); abline(a = 0, b = 1)
qqnorm(df$residuals_3, ylim = c(-5, 5), xlim = c(-5, 5), bty = "n", pch = 20, main = "full model"); abline(a = 0, b = 1)
dev.off()

m_p1 <- predict(density_model_static); m_p1 = m_p1[,c("response", "est")]
m_p1$back_abs_res = abs(df$residuals_1)
m_p1$model = "static"

m_p2 <- predict(density_model_dynamic); m_p2 = m_p2[,c("response", "est")]
m_p2$back_abs_res = abs(df$residuals_2)
m_p2$model = "dynamic"

m_p3 <- predict(density_model_full); m_p3 = m_p3[,c("response", "est")]
m_p3$back_abs_res = abs(df$residuals_3)
m_p3$model = "full"

m_p = rbind(m_p1, m_p2)

df1 = df %>% 
  group_by(X, Y) %>% 
  summarise(residuals = mean(residuals_1)) %>% 
  mutate(model = "static")

df2 = df %>% 
  group_by(X, Y) %>% 
  summarise(residuals = mean(residuals_2)) %>% 
  mutate(model = "dynamic")

df3 = df %>% 
  group_by(X, Y) %>% 
  summarise(residuals = mean(residuals_3)) %>% 
  mutate(model = "full")

df_residuals = rbind(df1, df2) 

(p1 = df_residuals %>% 
    ggplot(aes_string("X", "Y", color = "model")) + 
    geom_point(size = round(abs(df_residuals$residuals), digits = 0), alpha = 0.5) + 
    xlab("Eastings (km)") +
    ylab("Northings (km)") + 
    coord_fixed() +
    # facet_wrap(~model) + 
    # scale_color_gradient2() + 
    cowplot::theme_half_open())

(p2 = m_p  %>% 
    ggplot(aes(response, exp(est), color = model, fill = model)) + 
    geom_point(alpha = 0.2, aes(size = back_abs_res), show.legend = F) +
    coord_fixed(ratio = 1) +
    ylab("Prediction") + 
    xlab("Observation") + 
    geom_abline(intercept = 0, slope = 1) +
    geom_smooth(method = "lm", se = T) + 
    cowplot::theme_half_open())

# confidence intervals on the fixed effects
tidy(density_model_static, conf.int = TRUE)
tidy(density_model_dynamic, conf.int = TRUE)
tidy(density_model_full, conf.int = TRUE)

# And similarly for the random effect parameters:
tidy(density_model_static, "ran_pars", conf.int = TRUE)
tidy(density_model_dynamic, "ran_pars", conf.int = TRUE)
tidy(density_model_full, "ran_pars", conf.int = TRUE)

png(paste0('outputs/residuals_', "Uku", '.png'), width = 8, height = 8, units = "in", res = 100)
print(p1)
dev.off()

png(paste0('outputs/pred_obs_', "Uku", '.png'), width = 5, height = 5, units = "in", res = 100)
print(p2)
dev.off()

# #  extract some parameter estimates
# sd <- as.data.frame(summary(TMB::sdreport(density_model$tmb_obj)))
# r <- density_model$tmb_obj$report()
# r

df = df_residuals %>% subset(model == "dynamic")

mp = m_p %>% subset(model == "dynamic")

(p1 = df %>% 
    ggplot(aes(X, Y, color = residuals)) + 
    geom_point(alpha = 0.5) + 
    xlab("Eastings (km)") +
    ylab("Northings (km)") + 
    coord_fixed() +
    scale_color_gradient2("") +
    cowplot::theme_half_open() +
    theme(legend.justification = c(-0.3, -0.1), 
          legend.position = c(0, 0)) + 
    labs(tag = "(a)"))

(p2 = mp  %>% 
    ggplot(aes(response, exp(est))) + 
    geom_point(alpha = 0.2, aes(size = back_abs_res), show.legend = F) +
    coord_fixed(ratio = 1) +
    ylab("Prediction") + 
    xlab("Observation") + 
    geom_abline(intercept = 0, slope = 1) +
    geom_smooth(method = "lm", se = T) + 
    cowplot::theme_half_open() + 
    labs(tag = "(b)"))

p1 + p2

png(paste0('outputs/pred_obs_', "Uku", '.png'), width = 10, height = 4, units = "in", res = 100)
print(p1 + p2)
dev.off()
