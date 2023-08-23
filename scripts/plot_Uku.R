library(dplyr)
library(ggplot2)
library(patchwork)
library(ggpubr)

rm(list = ls())

select = dplyr::select

# for Uku: 
# Total numerical density estimates (individuals per 100 m2) were obtained by dividing fish counts in each survey by the survey area (353 m2 from two 15-m diameter survey cylinders) and multiplying by 100. - Nadon et al. 2020

# load survey data, adjust columns, create id
df = readRDS("data/rea/NCRMP_fish_site_20102019.rds")

df$ISLAND = gsub(" ", "_", df$ISLAND)
df$DATE_ = as.character(df$DATE_)
df$DATE_ = substr(df$DATE_, 1, 10)

colnames(df)[14] = "LAT"
colnames(df)[15] = "LON"

colnames(df)[7:9] = c("Year", "Month", "Day")
df$Year = substr(df$DATE_, 1, 4)
df$Month = substr(df$DATE_, 6, 7)
df$Day = substr(df$DATE_, 9, 10)

df$DATE = paste0(df$Month, "-", df$Day, "-", df$Year)
df$DATE_R = lubridate::mdy(df$DATE)
df$id = paste(df$LON, df$LAT, df$DATE_R, df$ISLAND, sep = "-")

islands = c("Kauai", #1
            "Lehua", #2
            "Niihau", #3
            "Kaula", #4
            "Oahu", #5
            "Molokai", #6
            "Maui", #7
            "Lanai", #8
            "Molokini", #9
            "Kahoolawe", #10
            "Hawaii")#[7:11]

Abund = df %>% 
  # subset(ISLAND %in% islands) %>% 
  subset(REGION == "MHI") %>%
  subset(Response == "Abund_m2") %>% 
  select(id, names(df[,1:18]), names(select(df, contains("APVI"))))

Biom = df %>% 
  # subset(ISLAND %in% islands) %>% 
  subset(REGION == "MHI") %>%
  subset(Response == "Biom_gm2") %>%
  select(id, names(df[,1:18]), names(select(df, contains("APVI"))))

# select response group = all size bins
Abund = Abund %>% 
  select(names(Abund[,1:20])) %>% 
  mutate(YEAR = substr(DATE_, 1, 4)) %>% 
  group_by(ISLAND, YEAR) %>% 
  summarise(Response = sum(APVI, na.rm = T)*100) %>%
  na.omit()

Biom = Biom %>% 
  select(names(Biom[,1:20])) %>% 
  mutate(YEAR = substr(DATE_, 1, 4)) %>% 
  group_by(ISLAND, YEAR) %>% 
  summarise(Response = sum(APVI, na.rm = T)*100) %>%
  na.omit()

p1a = Abund %>% 
  ggplot(aes(Response)) +
  geom_histogram() + 
  xlab("n/sq.m") + 
  ylab("") + 
  theme_pubr() + 
  annotate("text", 
           y = Inf,
           x = Inf,
           label = "Abundance",
           vjust = 2, 
           hjust = 2)  + 
  labs(tag = "(a)")

p1b = Biom %>% 
  ggplot(aes(Response)) +
  geom_histogram() + 
  xlab("g/sq.m") + 
  ylab("") + 
  theme_pubr() + 
  annotate("text", 
           y = Inf,
           x = Inf,
           label = "Biomass",
           vjust = 2, 
           hjust = 2)  + 
  labs(tag = "")

(p1 = p1a + p1b)

p2a = Abund %>% 
  group_by(YEAR) %>% 
  summarise(n = mean(Response)) %>% 
  ggplot(aes(YEAR, n)) +
  geom_line(aes(group = 1)) + 
  geom_point() +
  xlab("") + 
  ylab("mean abundance n/sq.m") + 
  scale_x_discrete(breaks = c(min(Abund$YEAR), median(Abund$YEAR), max(Abund$YEAR))) +
  theme_pubr() + 
  labs(tag = "(b)")

p2b = Biom %>% 
  group_by(YEAR) %>% 
  summarise(n = mean(Response)) %>% 
  ggplot(aes(YEAR, n)) +
  geom_line(aes(group = 1)) + 
  geom_point() +
  xlab("") + 
  ylab("mean biomass g/sq.m") + 
  scale_x_discrete(breaks = c(min(Biom$YEAR), median(Biom$YEAR), max(Biom$YEAR))) +
  theme_pubr() + 
  labs(tag = "")

(p2 = p2a + p2b)

Abund = df %>% 
  # subset(ISLAND %in% islands) %>% 
  subset(REGION == "MHI") %>%
  subset(Response == "Abund_m2") %>% 
  select(id, names(df[,1:18]), names(select(df, contains("APVI"))))

Biom = df %>% 
  # subset(ISLAND %in% islands) %>% 
  subset(REGION == "MHI") %>%
  subset(Response == "Biom_gm2") %>%
  select(id, names(df[,1:18]), names(select(df, contains("APVI"))))

# select response group = all size bins
Abund = Abund %>% 
  select(names(Abund[,1:20])) %>% 
  mutate(YEAR = substr(DATE_, 1, 4)) %>% 
  group_by(ISLAND, YEAR) %>% 
  summarise(Response = sum(APVI, na.rm = T)*100) %>%
  na.omit()

Biom = Biom %>% 
  select(names(Biom[,1:20])) %>% 
  mutate(YEAR = substr(DATE_, 1, 4)) %>% 
  group_by(ISLAND, YEAR) %>% 
  summarise(Response = sum(APVI, na.rm = T)*100) %>%
  na.omit()

biomass = Biom %>% 
  group_by(YEAR, ISLAND) %>% 
  summarise(n = mean(Response)) %>% 
  mutate(scaled_Response = scale(n, center = T),
         gp = "biomass")

abundance = Abund %>% 
  group_by(YEAR, ISLAND) %>% 
  summarise(n = mean(Response)) %>% 
  mutate(scaled_Response = scale(n, center = T),
         gp = "abundance")

p3 = rbind(abundance, biomass) %>% 
  ggplot(aes(YEAR, scaled_Response, color = gp)) + 
  geom_point() + 
  geom_line(aes(group = gp)) + 
  facet_wrap(.~ISLAND, scales = "free_y") + 
  scale_color_discrete("") + 
  xlab("") + 
  ylab("z-score") + 
  scale_x_discrete(breaks = c(min(abundance$YEAR), median(abundance$YEAR), max(abundance$YEAR))) +
  theme_pubr() + 
  theme(legend.position = c(0.9,0.1)) + 
  labs(tag = "(c)")

png("outputs/s1_Uku_MHI_2010_2019.png", height = 6, width = 12, units = "in", res = 500)
(p1/p2) | p3
dev.off()
