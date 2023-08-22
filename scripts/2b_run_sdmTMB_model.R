library(sdmTMB)
library(ggplot2)
library(rgdal)
library(colorRamps)
library(patchwork)
library(raster)
library(sf)
library(readr)
library(ggpubr)
library(tidyr)
library(lubridate)
# library(beepr)
library(dplyr, warn.conflicts = F)

options(dplyr.summarise.inform = F)

rm(list = ls())

select = dplyr::select

# load clean eds-enhanced ncrmp data. See 1_prep_ncrmp_data.R
load("data/ncrmp_eds_marian_samoa_all_sp.Rdata") # eds-enhanced ncrmp data w/ all species (n = 727)

df = df %>% filter(region == "MARIAN") %>% select(species, abund.site, taxonname, island, depth, lat, lon, date, year)

islands = unique(df$island)
islands <- c(islands, "All")[1:14]

# load the 500m resolution Guam prediction grid from prep.grid.R
res = c(0.5, 0.05)[2]

if (res == 0.5) {
  
  load("data/grid/marian_grid_0.5km.Rdata") #500m
  
} else {
  
  load("data/grid/marian_grid_0.05km.Rdata") #50m
  
}

grid$Island <- gsub('Pajaros', 'Farallon_de_Pajaros', grid$Island)

# Read in Island Boundaries (i.e., land mass)
load('data/gis/MHI_islands_shp.RData')
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

ncrmp_sdm = function(isl){
  
  obs_cutoff = 0.03
  cutoff_dist = c(0.5, 1)
  threshold = 0.95
  
  for (c in 1:length(cutoff_dist)) {
    
    # c = 2
    
    # isl = "Guam"
    
    print(paste0("Starting sdmTMB with cutoff distance at ", cutoff_dist[c]))
    
    if (isl == "All") {
      
      df_i = df 
      grid_i = grid
      
    } else {
      
      df_i = df %>% filter(island == isl)
      grid_i = grid %>% filter(Island ==  isl)
      
    }
    
    rank = df_i %>%
      group_by(species) %>% 
      summarise(mean = mean(abund.site)) %>% 
      arrange(-mean)
    
    sp_list = unique(rank$species)
    
    for (s in 1:length(sp_list)) {
      
      # s = 1
      
      if (file.exists(paste0("outputs/species/", res, "/", isl, "_", sp_list[s], "_prediction.rds"))) {
        
        print(paste0("Skipping ", sp_list[s], " because the result already exist."))
        next
        
      }
      
      df_i_s = df_i %>% 
        mutate(response = ifelse(species == sp_list[s], abund.site, 0)) %>% 
        mutate(response = ifelse(is.na(response), 0, response)) %>% 
        mutate(month = month(date), 
               year = paste(year, sprintf("%02d", month), sep = "-")) %>% 
        group_by(lon, lat, date, year, depth) %>%
        summarise(response = sum(response, na.rm = T)) %>%
        # subset(response < quantile(response, probs = 0.999)) %>%
        na.omit()
      
      # hist(df_i_s$response, breaks = 100)
      
      # Calculate the proportion of zeros in the column
      zero_prop <- round(sum(df_i_s$response == 0) / length( df_i_s$response), 2)
      
      # Check if the proportion of zeros exceeds the threshold
      if (zero_prop >= threshold) {
        print(paste0("Skipping ", sp_list[s], " because prop of zero is higher than ", threshold*100, "%."))
        next
      }
      
      # if (nrow(subset(df_i_s, response > 0)) / nrow(subset(df_i_s)) < obs_cutoff) {
      #   print(paste0("Skipping ", sp_list[s], " because the species obs. rate is less than ", obs_cutoff*100, "%."))
      #   next
      # }
      
      print(paste0("Modeling ", sp_list[s], "... #", s))
      
      get_crs(df_i_s, c("lon", "lat"))
      df_i_s = add_utm_columns(df_i_s, c("lon", "lat"), units = "km")
      
      # add utm coordinates
      zone <- (floor((df_i_s$lon[1] + 180)/6) %% 60) + 1
      
      ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
      ISL_this_utm = spTransform(ISL_this, CRS(paste0("+proj=utm +units=km +zone=", zone)))
      ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=", zone))
      
      rea_spde <- make_mesh(df_i_s, c("X", "Y"), cutoff = cutoff_dist[c], seed = 2023)
      rea_spde_coast <- add_barrier_mesh(rea_spde, ISL_this_sf)
      
      # obs_year = unique(df_i_s$year); obs_year
      # full_year = seq(min(df_i_s$year), max(df_i_s$year), by = 1)
      # missing_year = setdiff(full_year, obs_year)
      # missing_year = as.integer(missing_year);missing_year
      
      # png(paste0("outputs/SPDE_mesh_field_", n_knots[k], "_knots.png"), units = "in", height = 10, width = 10, res = 500)
      # plot(rea_spde_coast$mesh, asp = 1, main = ""); axis(1); axis(2)
      # plot(ISL_this_utm, add = TRUE)
      # points(rea_spde_coast$loc_xy,col = "green", pch = ".", cex = 5)
      # bar_i = rea_spde_coast$barrier_triangles
      # norm_i = rea_spde_coast$normal_triangles
      # points(rea_spde_coast$spde$mesh$loc[,1], rea_spde_coast$spde$mesh$loc[,2], pch = 20, col = "black")
      # points(rea_spde_coast$mesh_sf$V1[bar_i], rea_spde_coast$mesh_sf$V2[bar_i], col = "red", pch = 20)
      # points(rea_spde_coast$mesh_sf$V1[norm_i], rea_spde_coast$mesh_sf$V2[norm_i], col = "blue", pch = 20)
      # dev.off()
      
      # a basic model for index standardization.
      # you will likely want to include 0 + as.factor(year) or -1 + as.factor(year)
      # so that there is a factor predictor that represents the mean estimate for each time slice.
      # Use tryCatch to handle the warning and continue with the next iteration
      
      # model just to visualize depth effect
      
      gc()
      
      m <- tryCatch(
        {
          sdmTMB(
            data = df_i_s, 
            formula = response ~ s(depth, k = 3),
            mesh = rea_spde_coast,
            # control = sdmTMBcontrol(newton_loops = 1),
            family = tweedie(link = "log")
          )
        },
        error = function(e) {
          message("Error occurred. Skipping iteration.")
          skip_to_next <<- TRUE
        },
        warning = function(w) {
          message("Warning: The model may not have converged. Skipping iteration.")
          skip_to_next <<- TRUE
        }
      )
      
      if(class(m) == "logical") {
        message("Bad model!!! Skipping iteration.")
        next
      }  
      
      # summary(m)
      
      nd <- data.frame(
        depth = seq(min(df_i_s$depth),
                    max(df_i_s$depth),
                    length.out = 100),
        year = unique(df_i_s$year)[1] # a chosen year
      )
      
      pred_depth <- predict(m, newdata = nd, se_fit = T, re_form = NA)
      
      # pred_depth %>%
      #   mutate(depth = round(depth, 1)) %>%
      #   group_by(depth) %>%
      #   summarise(est = mean(est),
      #             est_se = mean(est_se)) %>%
      #   ggplot(aes(depth, exp(est),
      #              ymin = exp(est - 1.96 * est_se),
      #              ymax = exp(est + 1.96 * est_se))) +
      #   geom_line() +
      #   geom_ribbon(alpha = 0.3) +
      #   scale_x_continuous() +
      #   coord_cartesian(expand = F) +
      #   labs(x = "Depth (m)", y = "n per 100 sq.m")
      
      gc()
      
      # actual model
      m <- tryCatch(
        {
          sdmTMB(
            data = df_i_s, 
            formula = response ~ 0 + as.factor(year) + s(depth, k = 3),
            time = NULL,
            mesh = rea_spde_coast,
            control = sdmTMBcontrol(newton_loops = 1),
            family = tweedie(link = "log")
          )
        },
        error = function(e) {
          message("Error occurred. Skipping iteration.")
          skip_to_next <<- TRUE
        },
        warning = function(w) {
          message("Warning: The model may not have converged. Skipping iteration.")
          skip_to_next <<- TRUE
        }
      )
      
      if(class(m) == "logical") {
        message("Bad model!!! Skipping iteration.")
        next
      }  
      
      # Run extra optimization
      m <- run_extra_optimization(m, newton_loops = 0)
      
      # check gradients and AIC
      # max(m$gradients)
      # AIC(m)
      
      # Extract coefficients:
      coef = rbind(tidy(m, conf.int = TRUE), 
                   tidy(m, effects = "ran_par", conf.int = TRUE))
      
      # Perform several 'sanity' checks:
      sanity = sanity(m) %>% as.data.frame() %>% t()
      colnames(sanity)[1] = sp_list[s]
      
      df_i_s$resids <- residuals(m) # randomized quantile residuals
      # qqnorm(df_i_s$resids, pch = 20, axes = F); axis(1); axis(2)
      # qqline(df_i_s$resids)
      
      # df_i_s %>%
      #   ggplot(aes(sample = resids)) +
      #   stat_qq(shape = 21) +
      #   stat_qq_line() +
      #   xlab("Theoretical Quantiles") +
      #   ylab("Sample Quantiles") +
      #   theme(aspect.ratio = 1)
      # 
      # df_i_s %>%
      #   ggplot(aes(x = resids, y = ..count.., fill = ..x..)) +
      #   geom_histogram(show.legend = F) +
      #   scale_fill_gradient2(trans = 'reverse', "")
      # 
      # df_i_s %>%
      #   ggplot(aes(lon, lat, fill = resids, size = resids)) +
      #   geom_point(shape = 21, alpha = 0.8,  show.legend = F) +
      #   scale_fill_gradient2(trans = 'reverse', "") +
      #   scale_color_gradient2(trans = 'reverse', "")
      
      residuals = df_i_s %>% as.data.frame %>% select(X, Y, resids)
      
      # r <- residuals(m, "mle-mcmc", mcmc_warmup = 100, mcmc_iter = 101)
      # qqnorm(r)
      # qqline(r)
      
      # Predict on new data:
      # if you're scaling, you need to make sure both the data and grid are scaled
      # by the same mean and standard deviation!
      # (or use poly() or a smoother s())
      # grid_i$depth_scaled = (log(grid_i$Depth) - mean(log(df_i_s$depth)))/sd(log(df_i_s$depth))
      # grid_i$depth_scaled2 = grid_i$depth_scaled ^ 2
      # grid$fyear <- as.factor(grid$year)
      # grid$year_scaled <- scale(as.numeric(grid$year))
      
      colnames(grid_i) = c("X", "Y", "year", "island", "depth")
      
      p <-  predict(m, newdata = df_i_s); gc()
      
      # ggplot() +
      #   geom_abline(intercept = 0, slope = 1, color = "gray20", linetype = "dotted") +
      #   geom_smooth(data = p, aes(response, exp(est)), method = "lm", se = F, color = "gray10", size = 0.5) +
      #   geom_point(data = p, aes(response, exp(est), fill = exp(est), size = exp(est)),
      #              alpha = 0.8, shape = 21, show.legend = F) +
      #   scale_fill_gradientn(colours = matlab.like(100)) +
      #   labs(x = "Observed (n/100 sq.m)", y = "Predicted (n/100 sq.m)") +
      #   scale_x_continuous( limits = range(df_i_s$response)) +
      #   scale_y_continuous( limits = range(df_i_s$response)) +
      #   coord_equal() +
      #   ggtitle(isl,
      #           subtitle = paste0(sp_list[s], ", #", s, ": Cutoff = ", cutoff_dist)) +
      #   theme(
      #     # panel.background = element_rect(fill = "gray10", colour = "gray10"),
      #     # panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"),
      #     # panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20"),
      #     aspect.ratio = 1)
      
      obs_pred = p %>% select(lon, lat, response, est) %>% as.data.frame()
      obs_pred$sp = sp_list[s]
      
      p <-  predict(m, newdata = grid_i %>% subset(year %in% unique(df_i_s$year))); gc()
      p$est = exp(p$est)
      p = p %>% subset(est < quantile(df_i_s$response, probs = 1))
      
      # p %>%
      #   # filter(island == "Rota") %>%
      #   mutate(est = est) %>%
      #   # group_by(X, Y) %>%
      #   # summarise(est = mean(est)) %>%
      #   ggplot(aes(X, Y, fill = est)) +
      #   geom_raster() +
      #   coord_fixed() +
      #   scale_fill_gradientn(
      #     colors = matlab.like(100),
      #     trans = "sqrt",
      #     "n/100 sq.m",
      #   ) +
      #   labs(x = "Eastings (km)", y = "Northings (km)") +
      #   facet_wrap(~year) +
      #   # ggtitle("",
      #   #         subtitle = paste0("Maximum predicted abundance = ", round(max(p$est), 1), " per 500 m2"),
      #   #         subtitle = paste0(sp_list[s], " mean predicted abundance (2009-2022)")) +
      #   theme(panel.background = element_rect(fill = "gray10", colour = "gray10"),
      #         panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray20"),
      #         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray20"))
      
      pred_grid = p %>% select(X, Y, year, island, depth, est ) %>% as.data.frame()
      colnames(pred_grid)[6] =  sp_list[s]
      
      p = list(coef = coef, 
               sanity = sanity, 
               residuals = residuals,
               obs_pred = obs_pred,
               depth = pred_depth, 
               grid = pred_grid)
      
      # saveRDS(p, paste0("G:/sdmtmb_outputs/predictions/", isl, "_", sp_list[s], "_prediction.rds"))
      saveRDS(p, paste0("outputs/species/", res, "/", isl, "_", sp_list[s], "_prediction.rds"))
      
      gc()
      
    }
    
  }
  
}

ncrmp_sdm("Agrihan")
ncrmp_sdm("Aguijan")
ncrmp_sdm("Alamagan")
ncrmp_sdm("Asuncion")
ncrmp_sdm("Farallon_de_Pajaros")
ncrmp_sdm("Guam")
ncrmp_sdm("Guguan")
ncrmp_sdm("Maug")
ncrmp_sdm("Pagan")
ncrmp_sdm("Rota")
ncrmp_sdm("Saipan")
ncrmp_sdm("Sarigan")
ncrmp_sdm("Tinian")
ncrmp_sdm("All")
