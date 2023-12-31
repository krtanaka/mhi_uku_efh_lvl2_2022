library(ggplot2)
library(dplyr)
library(sdmTMB)

# We describe the basic steps in fitting a spatial or spatiotemporal predictive-process GLMM with TMB. This can be useful for (dynamic) species distribution models and relative abundance index standardization among many other uses. See model description for full model structure and equations.

# We will use built-in data for Pacific cod from a fisheries independent trawl survey.

# The density units are kg/km2 as calculated from catch biomass, net characteristics, and time on bottom.
# X and Y are coordinates in UTM zone 9.
# There are columns for depth and depth squared.
# Depth was centred and scaled by its standard deviation and we’ve included those in the data frame so that they could be used to similarly scale the prediction grid.

glimpse(pcod)

# The most basic model structure possible in sdmTMB replicates glmmTMB(). However, sdmTMB is meant for spatial models, so first we must create the spatial mesh even if we won’t actually include any spatial random effects in our first example.

# The spatial components in sdmTMB are included as random fields using a triangulated mesh with vertices, known as knots, used to approximate the spatial variability in observations. Bilinear interpolation is used to approximate a continuous spatial field (Lindgren et al., 2011; Rue et al., 2009) from the estimated values of the spatial surface at these knot locations to other locations including those of actual observations. These spatial random effects are assumed to be drawn from Gaussian Markov random fields (e.g. Cressie & Wikle, 2011; Latimer et al., 2009; Lindgren et al., 2011) with covariance matrices that are constrained by Matérn covariance functions (Cressie & Wikle, 2011).

# There are different options for creating the spatial mesh (see [make_mesh function description] (https://pbs-assess.github.io/sdmTMB/reference/make_mesh.html). We will start with relatively course mesh for a balance between speed and accuracy (cutoff = 10 where cutoff is in the units of X and Y (km here) and represents the minimum distance between points before a new mesh vertex is added). Smaller values create meshes with more knots. You will likely want to use a higher resolution mesh (more knots) for some scenarios but care must be taken to avoid overfitting (see Identifying spatial complexity in sdmTMB models. The circles represent observations and the vertices are the knot locations.

mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 10)
plot(mesh)

# Here is a logistic regression of pacific cod occupancy as a function of survey year (as a factor ), depth, and depth squared conducted first in sdmTMB without any spatial or spatiotemporal random effects (spatial = “off”):

m <- sdmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  mesh = mesh,
  family = binomial(link = "logit"),
  spatial = "off" # without any spatial random effects 
)
m
AIC(m)

# And for comparison the same model in glmmTMB:
m0 <- glmmTMB::glmmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2 ,
  family = binomial(link = "logit")
)
summary(m0)
  
# Notice that AIC, logLik, parameter estimates, and SE are all equivalent.
# Next, we can incorporate just spatial random effects into the above model by changing spatial to “on” and see that this changes coefficient estimates .
  
m1 <- sdmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  mesh = mesh,
  family = binomial(link = "logit"),
  spatial = "on"
)
m1
AIC(m1)

# To add spatiotemporal random fields to this model we need to include both the time argument that indicates what column of your dataframe contains the time slices for at which spatial random fields should be estimated (e.g., time = “year) and we need to choose whether these fields are independent and identically distributed (spatiotemporal =”IID“), first-order autoregressive (spatiotemporal =”AR1“), or as a random walk (spatiotemporal =”RW"). We will stick with IID for these examples.

m2 <- sdmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  mesh = mesh,
  family = binomial(link = "logit"),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID"
)
m2
AIC(m2)

# We can also model biomass density using a Tweedie distribution:

m3 <- sdmTMB(
  data = pcod,
  formula = density ~ poly(log(depth), 2),
  mesh = mesh,
  family = tweedie(link = "log"),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID"
)
m3
AIC(m3)

###########################
### Parameter estimates ###
###########################

# We can view the confidence intervals on the fixed effects by using the tidy function:
tidy(m3, conf.int = TRUE)

# And similarly for the random effect parameters:
tidy(m3, "ran_pars", conf.int = TRUE)

# Note that SE are not reported when coefficients are in log space, but examining the confidence intervals with give an indication of the precision of these estimates. These parameters are defined as follows:

# range - Parameter that controls the decay of spatial correlations. If the share_range argument is changed to FALSE and the spatial and spatiotemporal ranges will be unique, otherwise the default is for both to share the same range.
# phi - Observation error scale parameter (e.g., SD in Gaussian).
# sigma_O - SD of spatial process (Omega).
# sigma_E - SD of spatiotemporal process (Epsilon).
# tweedie_p - Tweedie p (power) parameter; between 1 and 2.

# If the model used AR1 spatiotemporal fields than:
# rho - Spatiotemporal correlation between years; should be between -1 and 1.

# If the model includes a spatial_varying predictor than:
# sigma_Z - SD of spatially varying coefficient field (Zeta).

#########################
### Model diagnostics ###
#########################

# We can inspect randomized quantile residuals:
pcod$resids <- residuals(m3) # randomized quantile residuals
qqnorm(pcod$resids)
qqline(pcod$resids)

ggplot(pcod, aes(X, Y, col = resids)) +
  scale_colour_gradient2() +
  geom_point() +
  facet_wrap(~year) +
  coord_fixed()

r <- residuals(m3, "mle-mcmc", mcmc_warmup = 100, mcmc_iter = 101)
qqnorm(r)
qqline(r)

###########################
### Spatial predictions ###
###########################

# Now we want to predict on a fine-scale grid on the entire survey domain. There is a grid built into the package for Queen Charlotte Sound named qcs_grid. Our prediction grid also needs to have all the covariates that we used in the model above.

glimpse(qcs_grid)

# Now make the predictions on new data.
# We will set the area argument to 4 km2 since our grid cells are 2 km x 2 km. If some grid cells were not fully in the survey domain (or were on land), we could feed a vector of grid areas to the area argument that matched the number of grid cells.

predictions <- predict(m3, newdata = qcs_grid)

# Let’s make a small function to make maps.

plot_map <- function(dat, column) {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_raster() +
    coord_fixed()
}

# There are four kinds of predictions that we get out of the model.
# First we will show the predictions that incorporate all fixed effects and random effects:

plot_map(predictions, "exp(est)") +
  scale_fill_viridis_c(trans = "sqrt", 
                       # trim extreme high values to make spatial variation more visible
                       na.value = "yellow", limits = c(0, quantile(exp(predictions$est), 0.995))) + 
  facet_wrap(~year) +
  ggtitle("Prediction (fixed effects + all random effects)", 
          subtitle = paste("maximum estimated biomass density =", round(max(exp(predictions$est)))))

# We can also look at just the fixed effects, here only a quadratic effect of depth:
  
plot_map(predictions, "exp(est_non_rf)")+
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Prediction (fixed effects only)") 

# We can look at the spatial random effects that represent consistent deviations in space through time that are not accounted for by our fixed effects. In other words, these deviations represent consistent biotic and abiotic factors that are affecting biomass density but are not accounted for in the model.

plot_map(predictions, "omega_s") +
  scale_fill_gradient2()+
  ggtitle("Spatial random effects only") 

# And finally we can look at the spatiotemporal random effects that represent deviation from the fixed effect predictions and the spatial random effect deviations. These represent biotic and abiotic factors that are changing through time and are not accounted for in the model.

plot_map(predictions, "epsilon_st") +
  scale_fill_gradient2()+
  facet_wrap(~year) +
  ggtitle("Spatiotemporal random effects only") 

# We can also estimate the uncertainty in our spatiotemporal density predictions using the simulations from the joint precision matrix by setting sims > 0 in the predict function. Here we generate 100 estimates and use apply() to calculate a median estimate, upper and lower confidence intervals, and a CV.

sim <- predict(m3, newdata = qcs_grid, sims = 100)
sim_last <- sim[qcs_grid$year == max(qcs_grid$year), ] # just plot last year
pred_last <- predictions[predictions$year == max(qcs_grid$year), ]
pred_last$median <- apply(exp(sim_last), 1, median)
pred_last$lwr <- apply(exp(sim_last), 1, quantile, probs = 0.025)
pred_last$upr <- apply(exp(sim_last), 1, quantile, probs = 0.975)
pred_last$cv <- round(apply(exp(sim_last), 1, function(x) sd(x) / mean(x)), 2)

# Plot the median estimated biomass densities for the last year.

ggplot(pred_last, aes(X, Y, fill = median)) +
  geom_raster() +
  scale_fill_viridis_c(trans = "sqrt")

# And then the CV on the estimates.

ggplot(pred_last, aes(X, Y, fill = cv)) + 
  geom_raster()+
  scale_fill_viridis_c()

###########################
### Conditional effects ###
###########################

# We can visualize the conditional effect of any covariates by feeding simplified dataframes to the predict function:

nd <- data.frame(
  depth = seq(min(pcod$depth),
              max(pcod$depth),
              length.out = 100
  ),
  year = 2015L # a chosen year
)

p <- predict(m3, newdata = nd, se_fit = TRUE, re_form = NA)

ggplot(p, aes(depth, exp(est),
              ymin = exp(est - 1.96 * est_se),
              ymax = exp(est + 1.96 * est_se)
)) +
  geom_line() +
  geom_ribbon(alpha = 0.4) +
  scale_x_continuous() +
  coord_cartesian(expand = F) +
  labs(x = "Depth (m)", y = "Biomass density (kg/km2)")

visreg::visreg(m3, "depth")

ggeffects::ggeffect(m3,  "depth [0:500 by=1]") %>% plot()

############################
### Time-varying effects ###
############################

# We could also let the effect of depth vary through time. To do this it helps to give each year a separate intercept.

m4 <- sdmTMB(
  density ~ 0 + as.factor(year),
  data = pcod,
  time_varying = ~ 0 + depth_scaled + depth_scaled2,
  mesh = mesh,
  family = tweedie(link = "log"),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID"
)
m4
AIC(m4)

# To plot these make newdata that contains all combinations of the time-varying covariate and time. This is easily created using expand.grid.

nd <- expand.grid(
  depth_scaled = seq(min(pcod$depth_scaled) + 0.2,
                     max(pcod$depth_scaled) - 0.2,
                     length.out = 50
  ),
  year = unique(pcod$year) # all years
)
nd$depth_scaled2 <- nd$depth_scaled^2

p <- predict(m4, newdata = nd, se_fit = TRUE, re_form = NA)

ggplot(p, aes(depth_scaled, exp(est),
              ymin = exp(est - 1.96 * est_se),
              ymax = exp(est + 1.96 * est_se),
              group = as.factor(year)
)) +
  geom_line(aes(colour = year), lwd = 0.1) +
  geom_ribbon(aes(fill = year), alpha = 0.5) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(labels = function(x) round(exp(x * pcod$depth_sd[1] + pcod$depth_mean[1]))) +
  coord_cartesian(expand = F) +
  labs(x = "Depth (m)", y = "Biomass density (kg/km2)")
