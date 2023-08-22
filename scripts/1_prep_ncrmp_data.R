library(dplyr)
library(ggplot2)
library(colorRamps)
library(patchwork)
library(readr)
library(ggpubr)
library(cowplot)
library(tidyr)

rm(list = ls())

select = dplyr::select

# load survey data, adjust columns, create id
df = readRDS("data/ncrmp/nSPC.site.abund.20092022.rds")

df$ISLAND = gsub(" ", "_", df$ISLAND)
df$DATE_ = as.character(df$DATE_)
df$DATE_ = substr(df$DATE_, 1, 10)
df$abund.site = df$abund.site * 100

colnames(df)[14] = "LAT"
colnames(df)[15] = "LON"

df$DATE_ = paste0(substr(df$DATE_, 6, 7),
                  "-", substr(df$DATE_, 9, 10),
                  "-", substr(df$DATE_, 1, 4))

df$DATE_ = lubridate::mdy(df$DATE_)

df$ID = paste(df$LON, df$LAT, df$DATE_, df$ISLAND, sep = "-")

unique(df$REGION)

# include all species or filter by speices of interest
target_sp = c("all", "select")[1]

sp_list = unique(df$SPECIES)

if (target_sp == "select") {
  
  # filter by species of interest (n = 31)
  library(stringr)
  sp = read.csv("data/sp_list/Copy of Guam Priority Species List.csv")
  sp = stringr::str_split_fixed(sp$Species, " ", 2) %>% as.data.frame()
  sp_list = toupper(paste0(substr(sp$V1, start = 1, stop = 2), substr(sp$V2, start = 1, stop = 2)))
  rm(sp)
  
  df = df %>% 
    subset(SPECIES == sp_list) %>% 
    na.omit()
  
}

df = df %>% na.omit()

glimpse(df)

# add unix date and year columns
df = df %>%
  mutate(DATE_Unix = as.numeric(as.POSIXct(df$DATE_)),
         YEAR = substr(DATE_, 1, 4)) 

colnames(df)[16] = "DATE"

# import scientific names
fish_info = read_csv("data/ncrmp/NCRMP_fish_taxainfo.csv")
# fish_info = fish_info %>% separate(TAXONNAME, c('TAXONNAME1', 'TAXONNAME2'))
# fish_info$TAXONNAME1 = substr(fish_info$TAXONNAME1, 1, 1)
# fish_info$TAXONNAME_Short = paste0(fish_info$TAXONNAME1, ".", fish_info$TAXONNAME2)
fish_info  = fish_info %>% 
  select("SPECIES", 
         "TAXONNAME",
         "COMMONNAME",
         "TROPHIC_MONREP")

df = left_join(df, fish_info)
rm(fish_info)

colnames(df) = tolower(colnames(df))

table(df$region)

(df %>%
    filter(island %in% c("Guam")) %>%
    # filter(year == 2022) %>%
    ggplot(aes(lon, lat, fill = year)) +
    geom_point(shape = 21, size = 3, alpha = 0.8) +
    # geom_hex() + 
    scale_fill_viridis_d("") + 
    coord_fixed() + 
    theme_bw() + 
    labs(x = "Longitude", y = "Latitude") + 
    theme(legend.position = c(0.1, 0.9),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA)))

(df %>%
    group_by(lon, lat, date, year, region, island, depth, depth_bin, species, trophic_monrep) %>%
    summarise(response = sum(abund.site, na.rm = T)) %>%
    # na.omit() %>% 
    subset(region %in% c("MARIAN")) %>%
    # subset(island %in% c("Guam")) %>%
    subset(trophic_monrep != "UNKNOWN") %>% 
    group_by(year, trophic_monrep, island) %>%
    summarise(sd = sd(response, na.rm = T),
              response = mean(response, na.rm = T)) %>% 
    ggplot(aes(factor(year), response, fill = island, color = island, group = island)) +
    # geom_point(shape = 21, size = 3) +
    # geom_line() +
    geom_pointrange(
      aes(ymin = ifelse(response - sd < 0, 0, response - sd),
          ymax = response + sd, color = island),
    position = position_dodge(0.5)) +
    scale_fill_viridis_d("") +
    scale_color_viridis_d("") +
    # labs(y = expression("Biomass (g per " ~ m^2~")"), x = "") + 
    labs(y = expression("Abundance (n per " ~ m^2~")"), x = "") + 
    facet_wrap(~ trophic_monrep, scales = "free", nrow = 1) +
    # facet_grid(~ trophic_monrep) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

(df %>%
    group_by(lon, lat, date, year, region, island, depth, depth_bin, species, trophic_monrep) %>%
    summarise(response = sum(abund.site, na.rm = T)) %>%
    na.omit() %>% 
    subset(region %in% c("MARIAN")) %>%
    subset(trophic_monrep != "UNKNOWN") %>% 
    group_by(year, species, island) %>%
    summarise(sd = sd(response, na.rm = T),
              response = mean(response, na.rm = T)) %>% 
    subset(response > 5) %>%
    ggplot(aes(year, response)) +
    geom_pointrange(
      aes(ymin = ifelse(response - sd < 0, 0, response - sd),
          ymax = response + sd, color = species),
      position = position_dodge(0.5)) +
    # scale_color_manual(values = matlab.like(length(unique(df$species))), "") +
    scale_color_viridis_d("") +
    # labs(y = expression("Biomass (g per " ~ m^2~")"), x = "") + 
    labs(y = expression("Abundance (n per " ~ m^2~")"), x = "") + 
    facet_wrap(~island, scales = "free"))

(df %>%
    group_by(lon, lat, date, year, region, island, depth, depth_bin, species, trophic_monrep) %>%
    summarise(response = sum(abund.site, na.rm = T)) %>%
    na.omit() %>% 
    subset(region %in% c("MARIAN")) %>%
    subset(trophic_monrep != "UNKNOWN") %>% 
    group_by(depth_bin, island) %>%
    summarise(sd = sd(response, na.rm = T),
              response = mean(response, na.rm = T)) %>% 
    # subset(response > 0) %>%
    ggplot(aes(depth_bin, response)) +
    geom_pointrange(
      aes(ymin = ifelse(response - sd < 0, 0, response - sd), 
          ymax = response + sd, color = island),
      position = position_dodge(0.3)) + 
    # scale_color_manual(values = matlab.like(length(unique(df$sci))), "") +
    scale_color_viridis_d("") + 
    labs(y = expression("Biomass (g per" ~ m^2~")"), x = ""))

(df %>%
    group_by(lon, lat, date, year, region, island, depth, depth_bin, species, trophic_monrep) %>%
    summarise(response = sum(abund.site, na.rm = T)) %>%
    na.omit() %>% 
    subset(region %in% c("MARIAN")) %>%
    subset(trophic_monrep != "UNKNOWN") %>% 
    group_by(depth, island) %>%
    summarise(response = mean(response, na.rm = T)) %>% 
    ggplot(aes(depth, response, color = island, fill = island, group = 1)) +
    geom_point(shape = 21) +
    geom_smooth(show.legend = F) + 
    facet_wrap(~island, scales = "free") + 
    # scale_color_viridis_c("") + 
    labs(y = expression("Biomass (g per" ~ m^2~")"), x = ""))

# png("outputs/xxx.png", height = x, width = x, units = "in", res = 500)
# dev.off()

# map observed species biomass
(df %>% 
    group_by(lon, lat, date, year, region, island, depth, depth_bin, species, trophic_monrep) %>%
    summarise(response = sum(abund.site, na.rm = T)) %>%
    na.omit() %>% 
    subset(region %in% c("MARIAN")) %>%
    subset(trophic_monrep != "UNKNOWN") %>% 
    mutate(lon = ifelse(lon < 0, lon + 360, lon)) %>% 
    group_by(lon, lat, trophic_monrep) %>% 
    summarise(response = mean(response)) %>% 
    ggplot(aes(lon, lat)) + 
    geom_point(aes(size = response, fill = response, color = response), shape = 21, alpha = 0.5) +
    scale_color_gradientn(colours = matlab.like(4), guide = "legend") +
    scale_fill_gradientn(colours = matlab.like(4), guide = "legend") +
    facet_grid(.~trophic_monrep) +
    labs(x = expression(paste("Longitude ", degree, "W", sep = "")),
         y = expression(paste("Latitude ", degree, "N", sep = ""))) +
    annotation_map(map_data("world")) +
    # theme(aspect.ratio = 1,
    #       panel.background = element_rect(fill = "gray10", colour = "gray10"),
    #       panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"), 
    #       panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20")) + 
    # guides(color = guide_legend(expression("Biomass (g per" ~ m^2~")")), 
    #        fill = guide_legend(expression("Biomass (g per" ~ m^2~")")),
    #        size = guide_legend(expression("Biomass (g per" ~ m^2~")"))) + 
    guides(color = guide_legend(expression("log(g per" ~ m^2~"+1)")), 
           fill = guide_legend(expression("log(g per" ~ m^2~"+1)")), 
           size = guide_legend(expression("log(g per" ~ m^2~"+1)"))))

# png("outputs/xxx.png", height = x, width = x, units = "in", res = 500)
# dev.off()

df = df %>% subset(region %in% c('SAMOA', 'MARIAN'))

# bring in EDS outputs
eds = read_csv("data/eds/ncrmp_eds_marian_samoa_jcrfmp.csv")

df = left_join(df, eds)

save(df, file = paste0("data/ncrmp_eds_marian_samoa_", target_sp, "_sp.Rdata"))
