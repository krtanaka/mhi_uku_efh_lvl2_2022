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

(d_m_q05_95 = rbind(d, m, q05, q95) %>%
    ggplot(aes(lon, lat, fill = x, color = x)) + 
    geom_point(alpha = 0.3, size = 2, shape = 21) + 
    scale_fill_gradientn(colours = matlab.like(100), "deg C", trans = "sqrt") + 
    scale_color_gradientn(colours = matlab.like(100), "deg C", trans = "sqrt") + 
    coord_fixed() + 
    facet_wrap(.~env) + 
    theme_minimal(base_size = 20) + 
    labs(x = "", y = "") + 
    theme(aspect.ratio = 0.7,
          legend.justification = c(0, -0.1),
          legend.position = c(0, 0),
          legend.text = element_text(color = "white", size = 12),
          legend.title = element_text(color = "white", size = 12),
          legend.key.size = unit(0.5, "cm"),
          panel.background = element_rect(fill = "gray10", colour = "gray10"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) )

(sd_p = sd %>% 
    ggplot(aes(lon, lat, fill = x, color = x)) + 
    geom_point(alpha = 0.3, size = 2, shape = 21) + 
    scale_fill_gradientn(colours = matlab.like(100), "deg C", trans = "sqrt") + 
    scale_color_gradientn(colours = matlab.like(100), "deg C", trans = "sqrt") + 
    coord_fixed() + 
    facet_wrap(.~env) + 
    theme_minimal(base_size = 20) + 
    labs(x = "", y = "") + 
    theme(aspect.ratio = 0.7,
          legend.justification = c(0, -0.1),
          legend.position = c(0, 0),
          legend.text = element_text(color = "white", size = 12),
          legend.title = element_text(color = "white", size = 12),
          legend.key.size = unit(0.5, "cm"),
          panel.background = element_rect(fill = "gray10", colour = "gray10"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) )

d_m_q05_95 + sd_p

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

(d_m_q05_95 = rbind(d, m, q05, q95) %>%
    ggplot(aes(lon, lat, color = x, fill = x)) + 
    geom_point(alpha = 0.3, size = 2, shape = 21) + 
    scale_color_viridis_c("mg m-3", trans = "sqrt") + 
    scale_fill_viridis_c("mg m-3", trans = "sqrt") + 
    coord_fixed() + 
    facet_wrap(.~env) + 
    theme_minimal() + 
    labs(x = "", y = "") + 
    theme(aspect.ratio = 0.7,
          legend.justification = c(0, -0.1),
          legend.position = c(0, 0),
          legend.text = element_text(color = "white", size = 12),
          legend.title = element_text(color = "white", size = 12),
          legend.key.size = unit(0.5, "cm"),
          panel.background = element_rect(fill = "gray10", colour = "gray10"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) )

(sd_p = sd %>% 
    ggplot(aes(lon, lat, color = x, fill = x)) + 
    geom_point(alpha = 0.3, size = 2, shape = 21) + 
    scale_color_viridis_c("mg m-3", trans = "sqrt") + 
    scale_fill_viridis_c("mg m-3", trans = "sqrt") + 
    coord_fixed() + 
    facet_wrap(.~env) + 
    theme_minimal() + 
    labs(x = "", y = "") + 
    theme(aspect.ratio = 0.7,
          legend.justification = c(0, -0.1),
          legend.position = c(0, 0),
          legend.text = element_text(color = "white", size = 12),
          legend.title = element_text(color = "white", size = 12),
          legend.key.size = unit(0.5, "cm"),
          panel.background = element_rect(fill = "gray10", colour = "gray10"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) )

d_m_q05_95 + sd_p

# wind
d = df %>% 
  group_by(lat, lon) %>% 
  summarise(x = wind_d) %>% 
  mutate(env = "Daily")

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

(d_m_q05_95 = rbind(d, m, q05, q95) %>%
    ggplot(aes(lon, lat, color = x, fill = x)) + 
    geom_point(alpha = 0.3, size = 2, shape = 21) + 
    scale_fill_gradientn(colours = matlab.like(100), "m s-1", trans = "sqrt") + 
    scale_color_gradientn(colours = matlab.like(100), "m s-1", trans = "sqrt") + 
    coord_fixed() + 
    labs(x = "", y = "") + 
    facet_wrap(.~env) + 
    ggdark::dark_theme_minimal(base_size = 20))

(sd_p = sd %>% 
    ggplot(aes(lon, lat, color = x, fill = x)) + 
    geom_point(alpha = 0.3, size = 2, shape = 21) + 
    scale_fill_gradientn(colours = matlab.like(100), "m s-1", trans = "sqrt") + 
    scale_color_gradientn(colours = matlab.like(100), "m s-1", trans = "sqrt") + 
    labs(x = "", y = "") + 
    facet_wrap(.~env) + 
    coord_fixed() + 
    ggdark::dark_theme_minimal(base_size = 20))

png("C:/Users/kisei.tanaka/Desktop/area.png", height = 8, width = 25, units = "in", res = 100)
d_m_q05_95 + sd_p
dev.off()



