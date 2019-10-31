
# Cheat sheet week 6
# Add elevation and land use to the spatial model and update

# first get elevation and LULC rasters (LULC from week 2)
eth_elev <- raster::getData("alt", country="ETH")
ETH_land_use <- raster("https://github.com/HughSt/HughSt.github.io/blob/master/course_materials/week2/Lab_files/ETH_land_use.tif?raw=true")

# Make sure these are at the same resolution/extent as the bioclim layers
# For elevation, we can use the resample function with the default 'method=bilinear' 
# which uses bilinear interpolation to resample to a new grid. 
# This is fine for continuous outcomes like elevation
eth_elev <- resample(eth_elev, bioclim_layers_oromia)

# For Land use, as this is categorical, we have to use nearest 
# neighbor method when resampling (see week 2 section on resampling)
# First check the resolution
res(ETH_land_use) 

# As the resolution is 3 times that of the other layers, its good 
# practice to first aggregate to the right resolution, and then 
# resample to make sure its on the same grid. To save on computation
# time, let's first crop and mask to Oromia
Oromia_land_use <- crop(ETH_land_use, Oromia)
Oromia_land_use <- mask(Oromia_land_use, Oromia)

# Now aggregate using modal value
Oromia_land_use_agg <- aggregate(Oromia_land_use, fact=3, fun = "modal")

# Now resample with nearest neighbor
Oromia_land_use_resamp <- raster::resample(Oromia_land_use_agg, eth_elev, method = "ngb")

# Now let's recategorize to reduce the number of possible LULC categories
# Let's use the reclassify function from the raster package. Let's change anything >150 to NA
# as a) we don't have any training data in this class and b) there are not many
# pixels in Oromia with these values
Oromia_land_use_resamp <- # reclassify the values into three groups 
  # all values > 0 and <= 40 become 1, etc.
  m <- c(0, 40, 1,  40, 120, 2,  120, 130, 3, 130, 150, 4, 150, 220, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
Oromia_land_use_reclass <- raster::reclassify(Oromia_land_use_resamp, rclmat)
 
# Check it looks the same as the others
Oromia_land_use_resamp; eth_elev

# Now we can extract values of these layers
ETH_malaria_data$land_use <- extract(Oromia_land_use_reclass, ETH_malaria_data[,c("longitude", "latitude")])
ETH_malaria_data$elevation <- extract(eth_elev, ETH_malaria_data[,c("longitude", "latitude")])

# # Check that all observations have values
# sum(!complete.cases(ETH_malaria_data))
# 
# # Ah, so 2 observations have an NA somewhere. Let's take a look
# ETH_malaria_data[!complete.cases(ETH_malaria_data),]
# 
# # Looks like there are 2 missing lulc values. Let's drop those observations. 
# # If we were being really diligent, we could investigate further and try and 
# # estimate those missing values by plotting on a map. But let's be lazy..
# ETH_malaria_data <- ETH_malaria_data[complete.cases(ETH_malaria_data),]
# 

# Now rerun the model
# Define 5 folds
set.seed(1981)
ix = caret::createFolds(ETH_malaria_data$pf_pos, k = 5)

# Run through all options and compare best performing models 
# with different numbers of covariates. Remeber to include land use 
# as a factor as it is a categorial variable as opposed to a continuous
# measure of land use
layer_names <- c("bioclim1", "bioclim12", "as.factor(land_use)", "elevation")
formula_kern <- "cbind(pf_pos, examined - pf_pos) ~ Matern(1|latitude+longitude)"
formula_model <- paste(c(formula_kern, layer_names), collapse = " + ")
scores <- c(cv_eth(ETH_malaria_data, as.formula(formula_model), ix))

# Simpler model
num_covariates <- length(layer_names)
max_covariates <- num_covariates - 1
indices <- 1:num_covariates

board <- data.frame(MSE = tail(scores, n=1), Covariates = paste(indices, collapse = ","))
while (num_covariates > max_covariates) {
        scores_iter <- c()
        ix_subsets <- gtools::combinations(n=num_covariates, r=num_covariates-1, v=indices)
        for (i in 1:nrow(ix_subsets)) {
          cov_subset <- layer_names[ix_subsets[i,]]
          formula_model <- paste(c(formula_kern, cov_subset), collapse = " + ")
          scores_iter <- c(scores_iter, cv_eth(ETH_malaria_data, as.formula(formula_model), ix))
        }
        best <- which.min(scores_iter)
        indices <- ix_subsets[best, ]
        scores <- c(scores, scores_iter[best])
        num_covariates <- length(indices)
        if (diff(tail(scores, n=2)) < 0 & max_covariates >= 2) {
          max_covariates <- max_covariates - 1
        }
        board <- rbind(board,
                       data.frame(MSE = tail(scores, n=1),
                                  Covariates = paste(indices, collapse = ",")))
}

# Take a look
board

# Looks like a model with just elevation produces the lowest MSE
# Let's use that model to predict across the province
prev_eth <- spaMM::fitme(cbind(pf_pos, examined - pf_pos) ~ elevation + 
                           Matern(1|latitude+longitude), 
                         data=ETH_malaria_data, 
                         family=binomial())

# Take a look at model output.
prev_eth # Looks like a negative effect of elevation, which makes sense

# Predict by adding elevation to the stack of covariate rasters
pred_stack <- stack(pred_stack, eth_elev)

# Change the name of the elevation raster layer to match the
# variable name used in the model
names(pred_stack)[5] <- "elevation"
pred_raster <- predict(pred_stack, prev_eth)
plot(pred_raster)
lines(Oromia)



