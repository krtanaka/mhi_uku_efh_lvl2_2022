# run_sdmTMB_model.R ~ lines 208 first

# sst
d = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = sst_d) %>% 
  mutate(env = "Daily_Mean")

m = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = sst_m) %>% 
  mutate(env = "Monthly_Mean")

q95 = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = q95_sst_m) %>% 
  mutate(env = "Monthly_q95")

q05 = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = q05_sst_m) %>% 
  mutate(env = "Monthly_q05")

sd = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = sd_sst_m) %>% 
  mutate(env = "Monthly_SST_sd")

d_m_q05_95 = rbind(d, m, q05, q95) %>%
  ggplot(aes(lon, lat, color = x)) + 
  geom_point(alpha = 0.5, size = 0.5) + 
  scale_color_gradientn(colours = matlab.like(100), "deg C") + 
  coord_fixed() + 
  facet_wrap(.~env) + 
  ggdark::dark_theme_minimal()

sd = sd %>% 
  ggplot(aes(lon, lat, color = x)) + 
  geom_point(alpha = 0.5, size = 0.5) + 
  scale_color_gradientn(colours = matlab.like(100), "deg C") + 
  coord_fixed() + 
  facet_wrap(.~env) + 
  ggdark::dark_theme_minimal()

d_m_q05_95 + sd

# chl_a
d = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = chla_8d) %>% 
  mutate(env = "Daily_Mean")

m = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = chla_m) %>% 
  mutate(env = "Monthly_Mean")

q95 = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = q95_chla_m) %>% 
  mutate(env = "Monthly_q95")

q05 = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = q05_chla_m) %>% 
  mutate(env = "Monthly_q05")

sd = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = sd_chla_m) %>% 
  mutate(env = "Monthly_chla_sd")

d_m_q05_95 = rbind(d, m, q05, q95) %>%
  ggplot(aes(lon, lat, color = x)) + 
  geom_point(alpha = 0.5, size = 0.5) + 
  scale_color_gradientn(colours = matlab.like(100), "mg m-3") + 
  coord_fixed() + 
  facet_wrap(.~env) + 
  ggdark::dark_theme_minimal()

sd = sd %>% 
  ggplot(aes(lon, lat, color = x)) + 
  geom_point(alpha = 0.5, size = 0.5) + 
  scale_color_gradientn(colours = matlab.like(100), "mg m-3") + 
  coord_fixed() + 
  facet_wrap(.~env) + 
  ggdark::dark_theme_minimal()

d_m_q05_95 + sd

# wind
d = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = wind_d) %>% 
  mutate(env = "Daily_Mean")

m = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = wind_m) %>% 
  mutate(env = "Monthly_Mean")

q95 = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = q95_wind_m) %>% 
  mutate(env = "Monthly_q95")

q05 = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = q05_wind_m) %>% 
  mutate(env = "Monthly_q05")

sd = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = sd_wind_m) %>% 
  mutate(env = "Monthly_sd")

d_m_q05_95 = rbind(d, m, q05, q95) %>%
  ggplot(aes(lon, lat, color = x)) + 
  geom_point(alpha = 0.5, size = 0.5) + 
  scale_color_gradientn(colours = matlab.like(100), "mg m-3") + 
  coord_fixed() + 
  facet_wrap(.~env) + 
  ggdark::dark_theme_minimal()

sd = sd %>% 
  ggplot(aes(lon, lat, color = x)) + 
  geom_point(alpha = 0.5, size = 0.5) + 
  scale_color_gradientn(colours = matlab.like(100), "mg m-3") + 
  coord_fixed() + 
  facet_wrap(.~env) + 
  ggdark::dark_theme_minimal()

d_m_q05_95 + sd





