library(dplyr)
library(cowplot)
library(ggplot2)
library(colorRamps)
library(patchwork)
library(readr)
library(ggpubr)
library(tidyr)

rm(list = ls())

select = dplyr::select

options(scipen = 999)

load("data/clean_ncrmp_marian_all_sp.Rdata")

functional_group = c("PISCIVORE", "PLANKTIVORE", "PRIMARY", "SECONDARY")

functional_group_table = NULL

for (t in 1:length(functional_group)) {
  
  # t = 1
  
  df_t = df %>% 
    # subset(island == "Guam") %>%
    subset(func == functional_group[t]) %>% 
    group_by(sci) %>% 
    summarise(n = sum(response, na.rm = T),
              Mean_Biomass = mean(response, na.rm = T)) %>% 
    mutate(freq = n/sum(n)) %>% 
    mutate(Cumulative_sum = cumsum(freq),
           Total_biomass = n/1000,
           Mean_Biomass = round(Mean_Biomass/1000, 4),
           Proportion = freq, 
           Functional_group = functional_group[t])
  
  functional_group_table = rbind(functional_group_table, df_t)
  
}

(functional_group_table %>% 
    subset(Proportion > 0.03) %>%
    ggplot(aes(Proportion, reorder(sci, Proportion), fill = Proportion)) +  
    geom_bar(stat = "identity", show.legend = T) + 
    scale_fill_gradientn(colours = matlab.like(100)) +
    # scale_fill_viridis_c() + 
    facet_wrap(.~Functional_group, ncol = 2, scales = "free") + 
    ylab("") + 
    theme_half_open() + 
    theme(axis.text.y = element_text(face = "italic")))

# alt approach
df = readRDS("data/ncrmp/NCRMP_fish_site_20102019.rds")

df = df %>% 
  select(-contains("_(")) %>% 
  select(-contains("_[")) %>% 
  select(-contains("Total")) %>% 
  # subset(REGION %in% c('NWHI', 'PRIAs', 'SAMOA', 'MHI', 'S.MARIAN', 'N.MARIAN')[5:6]) %>% 
  subset(ISLAND == "Guam") %>% 
  na.omit()

df_abundance = df %>% subset(Response == "Abund_m2")
df_biomass = df %>% subset(Response == "Biom_gm2")

# look at all species
sp_list = names(df)[19:731]

# all species by biomass & abundance
all_sp = NULL

for (sp in 1:length(sp_list)) {
  
  # sp = 5
  
  df_abundance_n <- df_abundance[,19:731] %>% select(contains(sp_list[[sp]]))
  df_biomass_n <- df_biomass[,19:731] %>% select(contains(sp_list[[sp]]))
  
  df_abundance_n = df_abundance_n[,1]
  df_biomass_n = df_biomass_n[,1]
  
  df_abundance_n = mean(df_abundance_n, na.rm = T)
  df_biomass_n = mean(df_biomass_n, na.rm = T)
  
  # if (df_abundance_n < 0.001 | df_biomass_n < 0.001) next
  
  df_abundance_n = data.frame(Species = sp_list[[sp]],
                              Abundance = df_abundance_n)
  df_biomass_n = data.frame(Species = sp_list[[sp]],
                            Biomass = df_biomass_n)
  
  df_n = merge(df_biomass_n, df_abundance_n)
  
  all_sp = rbind(all_sp, df_n)
  
  cat(paste0(round(sp / length(sp_list) * 100), '% completed'))
  Sys.sleep(0.0001)
  if (sp == length(sp_list)) cat(': Done')
  else cat('\014')
  
}

all_sp %>% 
  ggplot(aes(Abundance, Biomass)) + 
  geom_point()

head(all_sp[order(all_sp$Abundance, decreasing = T),])
head(all_sp[order(all_sp$Biomass, decreasing = T),])

# filter by species of interest (n = 31)
library(stringr)
sp = read.csv("data/sp_list/Copy of Guam Priority Species List.csv")
sp = stringr::str_split_fixed(sp$Species, " ", 2) %>% as.data.frame()
sp_list = toupper(paste0(substr(sp$V1, start = 1, stop = 2), substr(sp$V2, start = 1, stop = 2)))
sp_list = data.frame(Species = sp_list, 
                     Priority = "TRUE")
rm(sp)

all_sp = left_join(all_sp, sp_list)

# import scientific names
fish_info = read_csv("data/ncrmp/NCRMP_fish_taxainfo.csv")
fish_info = fish_info %>% separate(TAXONNAME, c('TAXONNAME1', 'TAXONNAME2'))
fish_info$TAXONNAME1 = substr(fish_info$TAXONNAME1, 1, 1)
fish_info$TAXONNAME = paste0(fish_info$TAXONNAME1, ". ", fish_info$TAXONNAME2)
fish_info = fish_info %>% 
  # subset(SPECIES %in% sp_list) %>% 
  mutate(Species = SPECIES,
         Functional = TROPHIC_MONREP) %>% 
  select(Species, Functional) 

all_sp = merge(all_sp, fish_info)

all_sp %>% 
  subset(Abundance > 0.0005) %>%
  ggplot(aes(reorder(Species, -Abundance), Abundance, 
             fill = ifelse(Priority == "TRUE", "Highlighted", "Normal"))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~Functional, scales = "free", nrow = 4) + 
  theme_half_open() + 
  labs(x = "", y = "Abund_m2") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1))

all_sp %>% 
  subset(Biomass > 0) %>%
  ggplot(aes(reorder(Species, -Biomass), Biomass,
             fill = ifelse(Priority == "TRUE", "Highlighted", "Normal"))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~Functional, scales = "free", nrow = 4) + 
  theme_half_open() + 
  labs(x = "", y = "Biom_gm2") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1))

