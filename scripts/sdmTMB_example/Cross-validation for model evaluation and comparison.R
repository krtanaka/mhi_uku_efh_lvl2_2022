# Cross-validation for model evaluation and comparison

library(ggplot2)
library(dplyr)
library(sdmTMB)

# Cross-validation is one of the best approaches that can be used to quantify model performance and compare sdmTMB models with different structures (unlike AIC, this approach will also factor in uncertainty in random effects). Arguably the most challenging decision in implementing cross-validation is how to specify the folds (each fold representing a subset of data that is in turn held out and used as a test set). Folds may vary in number and how data are partitioned, and will likely be slightly different for each application.

# The goals of some sdmTMB applications may be focused on spatial prediction; these include making prediction to new spatial regions (e.g. unsampled areas or areas not sampled in every year). For these types of models we recommend exploring folds using the blockCV or spatialsample packages (Silge 2021; Valavi et al. 2019). In general, these spatial sampling approaches assign observations that are spatially autocorrelated to the same fold. Accounting for the spatial correlation can lead to better estimates of covariate effects, as well as prediction errors.

# Alternatively, the goals of an analysis with sdmTMB may be to evaluate the predictive accuracy of a model in time (e.g. a missing survey year, or prediction to future years). For retrospective analyses, all points within a year may be assigned to a fold (or groups of years to the same fold). In contrast, models that are forward looking would use Leave Future Out Cross-Validation (LFOCV). In LFOCV, data up to year t are used to predict observations at t+1, etc.

# Cross validation in sdmTMB
# Cross validation in sdmTMB is implemented using the sdmTMB_cv() function, with the k_folds argument specifying the number of folds (defaults to 8). The function uses parallelization by default a future::plan() is set, but this can be turned off with the parallel argument.

data(pcod)
mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 25)
pcod$fyear <- as.factor(pcod$year)

# Set parallel processing if desired:
library(future)
plan(multisession)
m_cv <- sdmTMB_cv(
  density ~ 0 + s(depth_scaled) + fyear,
  data = pcod,
  mesh = mesh,
  family = tweedie(link = "log"),
  k_folds = 4
)

In the above example, folds are assigned randomly—but these can be modified to specific spatial or temporal applications. Without getting into the complexities of the blockCV or spatialsample packages, we could simply use kmeans to generate spatial clusters, e.g.

clust <- kmeans(pcod[, c("X", "Y")], 20)$cluster

m_cv <- sdmTMB_cv(
  density ~ 0 + s(depth_scaled) + fyear,
  data = pcod,
  mesh = mesh,
  fold_ids = clust,
  family = tweedie(link = "log"),
  k_folds = length(unique(clust))
)

Or similarly, these clusters could be assigned in time—here, each year to a unique fold. Note that year is not included as a factor and spatiotemporal fields are turned off because they cannot be estimated in missing years.

clust <- as.numeric(as.factor(pcod$year))

m_cv <- sdmTMB_cv(
  density ~ 0 + s(depth_scaled),
  data = pcod,
  mesh = mesh,
  fold_ids = clust,
  spatiotemporal = "off",
  family = tweedie(link = "log"),
  k_folds = length(unique(clust))
)

# Measuring model performance
# Lots of measures of predictive accuracy can be used to evaluate model performance. By default, sdmTMB_cv() returns a list that contains 2 measures: the log likelihoods for each fold (and total), and the expected log predictive density for each fold (and total). The latter (ELPD) is a measure of the predictive ability of the model for new observations, while the log-likelihood of the hold out data corresponds to the density for those particular observations. These can be accessed as below, and inspecting the quantities across folds may help elucidate whether there are particular folds that are difficult to predict.

m_cv <- sdmTMB_cv(
  density ~ 0 + s(depth_scaled) + fyear,
  data = pcod,
  mesh = mesh,
  family = tweedie(link = "log"),
  k_folds = 4
)

m_cv$fold_elpd # fold ELPD
m_cv$elpd # total ELPD

m_cv$fold_loglik # fold log-likelihood
m_cv$sum_loglik # total log-likelihood

# Single splits and Leave Future Out Cross-Validation
# In cases where only a single test set is evaluated (e.g. 10% of the data), using the sdmTMB_cv() function may be overkill because two sdmTMB() models will be fit, but using this function may be worthwhile to reduce coding errors (in the log-likelihood or ELPD calculations). For example, here we assign two folds, randomly holding out 10% of the observations as a test set (the test set is given ID = 1, and the training set is given ID = 2).

clust <- sample(1:2, size = nrow(pcod), replace = T, prob = c(0.1, 0.9))

m_cv <- sdmTMB_cv(
  density ~ 0 + s(depth_scaled) + fyear,
  data = pcod,
  mesh = mesh,
  fold_ids = clust,
  family = tweedie(link = "log"),
  k_folds = length(unique(clust))
)

# We can ignore the total log-likelihood and total ELPD, and just focus on the first elements of these lists, e.g.

m_cv$fold_loglik[[1]]

# If we wanted to do LFOCV, we could also use the sdmTMB_cv() function—though either way, it gets complicated because we need to change the data for each prediction. With the pcod dataset, the years are

unique(pcod$year)

# As above with temporal folds, we cannot include year as a factor and turn spatiotemporal fields off. We can use years 2011-2017 as test years. Two things to note are that if we specified time varying coefficients or a smooth on year effects ~ s(year), we’d want to specify missing years with the extra_time argument. Second, given the ways the folds are set up below, we have to extract the log-likelihood values for just the years of interest ourselves. Finally, it’s also possible to do this same procedure using sdmTMB() rather than sdmTMB_cv().

test_years <- c(2011, 2013, 2015, 2017)

models <- list()
log_lik <- list()

for (i in 1:length(test_years)) {
  clust <- rep(1, nrow(pcod))
  clust[which(pcod$year < test_years[i])] <- 2
  
  models[[i]] <- sdmTMB_cv(
    density ~ 0 + s(depth_scaled),
    data = pcod,
    mesh = mesh,
    spatiotemporal = "off",
    fold_ids = clust,
    family = tweedie(link = "log"),
    k_folds = length(unique(clust))
  )
  
  log_lik[[i]] <- sum(m_cv$data$cv_loglik[which(m_cv$data$year == test_years[i])])
}

# Comparing two or more models
# We can use the output of sdmTMB_cv() to compare two or more models. For example, if we wanted to evaluate the support for a depth effect or not, we could do 10-fold cross validation (it’s important that the folds be the same across the two models). In this example, using either the predictive log-likelihood or ELPD would lead one to conclude that including depth improves the predictive accuracy of the model.

clust <- sample(1:10, size = nrow(pcod), replace = T)

m1 <- sdmTMB_cv(
  density ~ 0 + fyear,
  data = pcod,
  mesh = mesh,
  fold_ids = clust,
  family = tweedie(link = "log"),
  k_folds = length(unique(clust))
)

m2 <- sdmTMB_cv(
  density ~ 0 + fyear + s(depth_scaled),
  data = pcod,
  mesh = mesh,
  fold_ids = clust,
  family = tweedie(link = "log"),
  k_folds = length(unique(clust))
)

# Compare log-likelihoods -- higher is better!
m1$sum_loglik
m2$sum_loglik

# Compare ELPD -- higher is better!
m1$elpd
m2$elpd

# Model ensembling
# Finally, instead of identifying single “best” models, we may be interested in doing model averaging. In the sdmTMB package, we’ve implemented the model stacking procedure described by (Yao et al. 2018) in the sdmTMB_stacking() function. This procedure uses optimization to find the normalized weights that maximize the total log-likelihood across models (other metrics may also be used). Inputs to the function are a list of models, where each list element is the output of a call to sdmTMB_cv():
  
  weights <- sdmTMB_stacking(model_list)
# By default this calculation uses data from each fold. If instead, we had split the data into the 10/90 split (as in the example above), we wouldn’t want to use the 2nd model fit to generate these weights. If we had just wanted to use the predictions from the first fold onto the 10% test set, we could specify that using the include_folds argument.

weights <- sdmTMB_stacking(model_list, include_folds = 1)
