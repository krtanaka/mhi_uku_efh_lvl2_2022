library(ggOceanMaps)
library(metR)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(marmap)
library(raster)
library(rmapshaper)
library(ggrepel)
library(dplyr)

rm(list = ls())

world <- ne_countries(scale = "large", returnclass = "sf")

Ibbox = read.csv("data/Island_Bounding_Boxes.csv", stringsAsFactors = F) # Updated Bounding boxes 2021
islands = unique(Ibbox$ISLAND.CODE); islands

load("data/rea/ALL_REA_FISH_RAW_SST.RData")

label = df %>% 
  subset(REGION == "MHI" & ISLAND %in% islands) %>% 
  group_by(ISLAND) %>% 
  summarise(lat = mean(LATITUDE),
            lon = mean(LONGITUDE))

b = marmap::getNOAA.bathy(lon1 = -160.5,
                          lon2 = -153,
                          lat1 = 17,
                          lat2 = 23,
                          resolution = 1)

b = marmap::fortify.bathy(b)

ggplot() +
  geom_contour(data = b,
               aes(x = x, y = y, z = z, colour = stat(level)),
               breaks = seq(-30, 0, by = 1),
               size = c(0.5)) +
  scale_colour_distiller(palette = "Blues", direction = 1, "Depth (m)") +
  geom_text_repel(data = label, 
                  aes(x = lon, y = lat, label = ISLAND), 
                  fontface = "bold",   
                  nudge_x = c(0.3, 0.3, 0.3, 0.3, 0.3),
                  nudge_y = c(0.2, 0.2, 0.2, 0.2, 0.2)) +
  coord_fixed() +
  # scale_x_continuous(expand = c(0, 0)) +
  # scale_y_continuous(expand = c(0, 0)) +
  theme_light() +
  labs(y = " ", x = "") + 
  theme( legend.position = c(1,1),
         legend.justification = c(1.1,1.1),
         axis.ticks = element_blank())
         #axis.text = element_blank())

b = marmap::getNOAA.bathy(lon1 = min(-161),
                          lon2 = max(-153),
                          lat1 = min(17),
                          lat2 = max(23),
                          resolution = 1)

b = marmap::fortify.bathy(b)

save(b, file = 'data/MHI_NOAA_bathymetry_1res.RData')
load('data/MHI_NOAA_bathymetry_1res.RData')

bathy_island = NULL

for (i in 1:length(islands)) {
  
  # i = 8
  
  box = Ibbox %>% subset(ISLAND.CODE == islands[i])
  
  bathy_i <- b %>% subset(x > box$RIGHT_XMAX & 
                            x < box$LEFT_XMIN & 
                            y > box$TOP_YMAX & 
                            y < box$BOTTOM_YMIN) %>% 
    mutate(island =  islands[i])
  
  bathy_island = rbind(bathy_i, bathy_island)
  
}

bathy_island %>% 
  ggplot(aes(x = x, y = y, z = z)) + 
  geom_contour(
    breaks = seq(-30, 0, by = 1),
    size = c(0.1),
    alpha = 0.8,
    show.legend = T,
    colour = rainbow(30327, rev = T)) + 
  ggdark::dark_theme_minimal()

# load("data/MHI_islands_shp.RData")
# crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands[i])),]
# world = st_transform(st_as_sf(world))
# world <- ms_simplify(world, keep = 0.1, keep_shapes = F)



map <- ggplot(data = world) +
  coord_sf(crs = st_crs(4135) # old hawaii projection code
           # xlim = c(-160.5, -154.8),
           # ylim = c(18.91, 22.25), expand = F
  ) +
  geom_sf() +
  # scale_x_continuous(breaks = seq(-160.5, -154.8, by = 0.5)) +
  # scale_y_continuous(breaks = seq(18.91, 22.25, by = 0.5)) +
  # geom_point(data = df, aes(LONGITUDE, LATITUDE, color = factor(OBS_YEAR))) + 
  geom_contour(data = b,
               aes(x = x, y = y, z = z),
               breaks = seq(-30, 0, by = 1),
               size = c(0.1),
               alpha = 0.8,
               show.legend = T,
               colour = rainbow(30482, rev = T)) +
  scale_fill_discrete("") + 
  scale_color_discrete("") + 
  geom_text_repel(data = label, 
                  aes(x = lon, y = lat, label = ISLAND), 
                  fontface = "bold",   
                  nudge_x = c(0.5, 0.5, 0.5, 0.5, 0.5),
                  nudge_y = c(0.5, 0.5, 0.5, 0.5, 0.5)) +
  # theme_pubr() + 
  theme(
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title = element_blank())
# legend.position = c(0.1, 0.3))

grid_i %>% 
  ggplot(aes(lon, lat, fill = sd_chla_m)) + 
  geom_raster() + 
  facet_wrap(.~year) + 
  scale_fill_viridis_c()

}



# pdf('/Users/Kisei.Tanaka/Desktop/MHI_200m_Bathy_Countour.pdf', height = 5, width = 7)
png('/Users/kisei/Desktop/MHI_200m_Bathy_Countour.png', height = 5, width = 7, res = 500, units = "in")
print(map)
dev.off()








library(ggplot2)
library(raster)
library(rmapshaper)
library(ggrepel)
library(dplyr)

rm(list = ls())

# save this for threashold modeling 
load("outputs/clean_df.RData")

islands = unique(df$island)

bathy = NULL

for (i in 1:length(islands)) {
  
  # i = 2
  
  load(paste0('outputs/bathymetry_year_', islands[i], ".Rdata"))
  
  df = grid_year %>% 
    group_by(x, y, island) %>% 
    summarise(depth = round(mean(depth), 3))
  
  bathy = rbind(bathy, df)
  
}

(map = bathy %>%
    ggplot(aes(x, y)) + 
    geom_tile(aes(fill = depth, height = 0.005, width = 0.005), alpha = 0.3) +
    # geom_point(aes(color = depth), size = 1.5, alpha = 0.3) +
    facet_wrap(~island, scales = "free", ncol = 4) + 
    scale_fill_gradientn("(m)", colours = matlab.like(100)) +
    scale_color_gradientn("(m)", colours = matlab.like(100)) + 
    xlab("Longitude (dec deg)") +
    ylab("Latitude (dec deg)") + 
    theme_minimal(I(8)) + 
    theme(aspect.ratio = 1,
          # legend.key.size = unit(0.5, "cm"),
          panel.background = element_rect(fill = "gray10", colour = "gray10"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")))

png('outputs/fig1.png', height = 5, width = 10, res = 500, units = "in")
print(map)
dev.off()
