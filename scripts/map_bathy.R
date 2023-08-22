library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(colorRamps)

df = read_rds("outputs/map_results.rds")
map = df

(fig1a = map %>%
    subset(island_group == "Niihau-Kauai") %>% 
    group_by(lon, lat, island_group) %>%
    summarise(depth = mean(depth)) %>%
    ggplot(aes_string("lon", "lat", fill = "depth")) +
    geom_raster(show.legend = F) +
    coord_fixed() +
    scale_fill_gradientn("", colours = matlab.like(100)) + 
    ylab("") + xlab("") + 
    theme_minimal() + 
    ggtitle("Niihau-Kauai") +
    theme(
          panel.background = element_rect(fill = "gray10", colour = "gray10"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")))

(fig1b = map %>%
    subset(island_group == "Oahu") %>% 
    group_by(lon, lat, island_group) %>%
    summarise(depth = mean(depth)) %>%
    ggplot(aes_string("lon", "lat", fill = "depth")) +
    geom_raster(show.legend = F) +
    coord_fixed() +
    scale_fill_gradientn("", colours = matlab.like(100)) + 
    ylab("") + xlab("") + 
    theme_minimal() + 
    ggtitle("Oahu") +
    theme(
      panel.background = element_rect(fill = "gray10", colour = "gray10"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")))

(fig1c = map %>%
    subset(island_group == "Molokai-Maui-Lanai-Kahoolawe") %>% 
    group_by(lon, lat, island_group) %>%
    summarise(depth = mean(depth)) %>%
    ggplot(aes_string("lon", "lat", fill = "depth")) +
    geom_raster(show.legend = F) +
    coord_fixed() +
    scale_fill_gradientn("", colours = matlab.like(100)) + 
    ylab("") + xlab("") + 
    theme_minimal() + 
    ggtitle("Molokai-Maui-Lanai-Kahoolawe") +
    theme(
      panel.background = element_rect(fill = "gray10", colour = "gray10"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")))

(fig1d = map %>%
    subset(island_group == "Hawaii") %>% 
    group_by(lon, lat, island_group) %>%
    summarise(depth = mean(depth)) %>%
    ggplot(aes_string("lon", "lat", fill = "depth")) +
    geom_raster(show.legend = T) +
    coord_fixed() +
    scale_fill_gradientn("", colours = matlab.like(100)) + 
    ylab("") + xlab("") + 
    theme_minimal() + 
    ggtitle("Hawaii") +
    theme(
      panel.background = element_rect(fill = "gray10", colour = "gray10"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")))

(fig1e = map %>%
    group_by(lon, lat) %>%
    summarise(depth = mean(depth)) %>%
    ggplot(aes_string("lon", "lat", fill = "depth")) +
    geom_raster(show.legend = T) +
    coord_fixed() +
    scale_fill_gradientn("", colours = matlab.like(100)) + 
    ylab("") + xlab("") + 
    theme_minimal() + 
    theme(
      panel.background = element_rect(fill = "gray10", colour = "gray10"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")))

fig1 = (fig1a | fig1b | fig1c | fig1d)
rm(fig4aa, fig4ab, fig4ac, fig4ad)

png(paste0("outputs/fig1_", n_knots[k], ".png"), height = 4, width = 22, units = "in", res = 500)
print(fig1) 
dev.off()
