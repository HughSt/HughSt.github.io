Spatial Regression
================

Week 6 - Spatial Regression
===========================

About this lecture
------------------

In this session we will introduce some concepts of spatial regression. We will focus on continuous spatial variation and see how to include it in a regression model. We will also discuss model selection using cross validation. The session is divided into the following sections:

-   Linear Regression Models
-   Spatial Covariance
-   Geostatistics
-   Cross-Validation
-   Application Example: Malaria Case

Besides the code displayed here, we will use some additional code to generate some toy datasets that will help illustrate the exposition. Before starting we will load these code as well as the ggplot2 library, which will be used for creating most of the images displayed.

``` r
source("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week6/Lab_files/R%20Files/background_functions.R")
library(ggplot2)
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

Linear Regression Models
------------------------

### Univariate Linear Model

As a first step we will do a recap on a *linear regression model*. In this problem we have a set of measurments of two variables, say *X* and *Y*, and we try to explain the values of *Y* based on the values on *X*. To do this we find the line that is the closest to all the points (*x*, *y*).

The command below generates a toy dataset that we will use as an example.

``` r
# Generate example data
dset1 <- univariate_lm()

# Show data
head(dset1)
```

    ##             x         y
    ## 1 -0.38813740 -8.005137
    ## 2  0.01616137 -7.917746
    ## 3  0.40175907 -7.892104
    ## 4  0.54888497 -8.061714
    ## 5  0.97495187 -7.694279
    ## 6  1.05565842 -7.741698

In *R* we can fit a linear model and make predictions with the comands shown next.

``` r
# Fit linear model on dataset 1
m1 <- lm(y ~ x, data = dset1)
m1_pred <- predict(m1, newdata = dset1, interval = "confidence")
dset1$y_hat <- m1_pred[,1]
dset1$y_lwr <- m1_pred[,2]
dset1$y_upr <- m1_pred[,3]
```

![](week6_spatial_regression_files/figure-markdown_github/plt_lm-1.png)

### Univariate GLM

While very useful, it is common that the model above turns out to be a not good assumption. Think of the case where *Y* is constrained to be positive. A straight line, unless it is horizontal, will cross the *y*-axis at some point. If the values of *X* where *Y* becomes negative are rare or they are a set of values we are not interested in, we may simply ignore them, however there are scenarios where we cannot afford having impossible values for *Y*.

As an example, we will load a second toy dataset and fit a liner regression model. Look at the bottom left corner of figure below. The predictions of *Y* are starting to cross the zero value and become negative, but the observed data remain positive.

``` r
# Fit linear model on dataset 2
dset2 <- univariate_glm()
m2 <- lm(y ~ x, data = dset2)
m2_pred <- predict(m2, newdata = dset1, interval = "confidence")
dset2$y_hat <- m2_pred[,1]
dset2$y_lwr <- m2_pred[,2]
dset2$y_upr <- m2_pred[,3]

ggplot(data=subset(dset2), aes(x, y)) + 
  geom_point(col="steelblue", size=2) + 
  geom_line(aes(x, y_hat), col="red") +
  geom_ribbon(aes(ymin=y_lwr, ymax=y_upr), fill="magenta", alpha=.25) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
```

![](week6_spatial_regression_files/figure-markdown_github/glm-1.png)

A solution to this problem is to use *generalized linear models* (GLM). A GLM uses a transformation on *Y* where the assumptions of the standard linear regression are valid (figure below), then it goes back to the original scale of *Y* and makes predictions. ![](week6_spatial_regression_files/figure-markdown_github/plt_logit-1.png)

When fitting a GLM to the dataset shown in the second example above, the resulting predictions draw a curve that never reaches zero. ![](week6_spatial_regression_files/figure-markdown_github/glm_quasibinomial-1.png)

### GLM with Spatially Structured Data

We will now move to a example of regression on spatial data. Say that we have a parcel of land where we are interested in quantifying the amount of organic matter. We take measurments at different locations randomly chosen, so that the locations can be any set of points in the parcel. We will also assume that there is a covariate *X*, say humidity, measured at the same locations.

The code below generates the data for this example and the figure shows such data. We are assuming that the organic matter is measuered in a fictitious scale where the unit is OM.

``` r
# load toy dataset for example
spatial_reg <- soil_data(n_peaks=3, n_data = 300, seed=0)
head(spatial_reg)
```

    ##         lng       lat   humidity        OM
    ## 1 357.78218 370.77231  1.6951691 205.56831
    ## 2 105.93796 268.81118  1.8340258 221.05495
    ## 3 148.47744  37.84828  0.9823934 167.15905
    ## 4 228.56849 196.54585  0.8873421 101.62076
    ## 5 362.37491 184.15918 -0.8146412  73.02679
    ## 6  80.47109 149.71140 -0.3820763  65.79837

![](week6_spatial_regression_files/figure-markdown_github/plt_soil_data-1.png)

The plot below shows the organic matter vs humidity. Notice that the values of organic matter are positive and that they become more spread the larger the values of humidity. ![](week6_spatial_regression_files/figure-markdown_github/plt_soil_cov-1.png)

If we transform the organic matter values with the logarithm, we get a clear linear relation with the values of humidity (see figure below). This resembles a perfect straight line, because this is a toy example designed this way. Things are less clear in reality, but the principles shown here can still be applied to it. ![](week6_spatial_regression_files/figure-markdown_github/plt_logsoil_cova-1.png)

In this case we will fit a GLM using the logarithm as link function. The model is described as

log*y*<sub>*i*</sub> = *η*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub>,

where *y*<sub>*i*</sub> is the amount of organic matter, *x*<sub>*i*</sub> is the humidity level, and *β*<sub>0</sub> and *β*<sub>1</sub> are parameters. To fit this model we can use the following command.

``` r
# Fit GLM to toy dataset
m3 <- glm(OM ~ humidity, data=spatial_reg, family=gaussian(link="log"))
summary(m3)
```

    ## 
    ## Call:
    ## glm(formula = OM ~ humidity, family = gaussian(link = "log"), 
    ##     data = spatial_reg)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -69.974  -14.630   -4.598   10.982  101.743  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.43623    0.02176  203.83   <2e-16 ***
    ## humidity     0.50586    0.01662   30.44   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 674.2259)
    ## 
    ##     Null deviance: 918485  on 299  degrees of freedom
    ## Residual deviance: 200919  on 298  degrees of freedom
    ## AIC: 2809.4
    ## 
    ## Number of Fisher Scoring iterations: 4

Once we have our GLM fitted, we can analyze the residuals to check if the assumption of them being independent and identically distributed is valid. In the figure below it seems that the values of the residuals are spatially related. ![](week6_spatial_regression_files/figure-markdown_github/plt_soil_cova-1.png)

We will make a more objective assesment of the residual's independence with Moran's coefficient. The figure displayed below is a spatial autocorrelogram shows that there is spatial autocorrelation and therefore that the residuals are not independent.

``` r
# Compute correlogram of the residuals
nbc <- 10
cor_r <- pgirmess::correlog(coords=spatial_reg[,c("lng", "lat")],
                            z=spatial_reg$residuals,
                            method="Moran", nbclass=nbc)

correlograms <- as.data.frame(cor_r)
correlograms$variable <- "residuals_glm" 

# Plot correlogram
ggplot(subset(correlograms, variable=="residuals_glm"), aes(dist.class, coef)) + 
  geom_hline(yintercept = 0, col="grey") +
  geom_line(col="steelblue") + 
  geom_point(col="steelblue") +
  xlab("distance") + 
  ylab("Moran's coefficient")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
```

![](week6_spatial_regression_files/figure-markdown_github/correlogram_1-1.png)

An approach to account for the spatial structure could be to include the GPS coordinates as covariates in the model. For example

*η*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + *β*<sub>2</sub>*s*<sub>\[*i*, 1\]</sub> + *β*<sub>3</sub>*s*<sub>\[*i*, 2\]</sub>,

where (*s*<sub>\[*i*, 1\]</sub>, *s*<sub>\[*i*, 2\]</sub>) are the longitude and latitude coordinates where each measurment was taken.

Having a trend across the surface may seem like a good idea, but it is not the best approach; and sometimes it is not even a good approach. First of all, the assumption of a surface trend may be to rigid. A polynomial fit, while more flexible, may still be too rigid or overfit the data. In any case we would need to decide which polynomial to use among all possibilities. For example

*η*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + *β*<sub>2</sub>*s*<sub>\[*i*, 1\]</sub> + *β*<sub>3</sub>*s*<sub>\[*i*, 2\]</sub> + *β*<sub>4</sub>*s*<sub>\[*i*, 1\]</sub>*s*<sub>\[*i*, 2\]</sub>,

*η*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + *β*<sub>2</sub>*s*<sub>\[*i*, 1\]</sub> + *β*<sub>3</sub>*s*<sub>\[*i*, 2\]</sub> + *β*<sub>4</sub>*s*<sub>\[*i*, 1\]</sub>*s*<sub>\[*i*, 2\]</sub> + *β*<sub>5</sub>*s*<sub>\[*i*, 1\]</sub><sup>2</sup> + *β*<sub>6</sub>*s*<sub>\[*i*, 2\]</sub><sup>2</sup>,

etc...

Spatial Covariance
------------------

The core idea behind Spatial Statistics is to understand an characterize this spatial dependence that is observed in different processes, for example: amount of rainfall, global temperature, air pollution, etc. Spatial Statistics deal with problems were nearby things are expected to be more alike.

When want to measure how much two variables change together, we use the covariance function. Under the right assumptions, we can also use the covariance function to describe the similarity of the observed values based on their location.

A covariance function *K* : 𝕊 × 𝕊 → ℝ maps a pair of points *z*<sub>1</sub> = (*s*<sub>\[1, 1\]</sub>, *s*<sub>\[1, 2\]</sub>) and *z*<sub>2</sub> = (*s*<sub>\[2, 1\]</sub>, *s*<sub>\[2, 2\]</sub>) to the real line. We can define such a function in terms of the distance between a pair of points. Let the distance between the points be given by *r* = ∥*z*<sub>1</sub> − *z*<sub>2</sub>∥, the following are examples of covarinace functions:

Exponentiated Quadratic: *K*(*z*<sub>1</sub>, *z*<sub>2</sub>)=*σ*<sup>2</sup>exp(−*r*<sup>2</sup>/ℓ<sup>2</sup>)

Rational Quadratic: *K*(*z*<sub>1</sub>, *z*<sub>2</sub>)=*σ*<sup>2</sup>(1 + *r*<sup>2</sup>/(2*α*ℓ<sup>2</sup>))<sup>−*α*</sup>

Matern Covariance: *K*(*z*<sub>1</sub>, *z*<sub>2</sub>)=*σ*<sup>2</sup>2<sup>1 − *ν*</sup>/*Γ*(*ν*)((2*ν*)<sup>.5</sup>*r*/ℓ)<sup>*ν*</sup>𝒦<sub>*ν*</sub>((2*ν*)<sup>.5</sup>*r*/ℓ)

The quantities ℓ, *α*, *ν* are parameters of the functions mentioned and 𝒦<sub>*ν*</sub> is the modified Bessel function of second kind. In the three cases, while less clear in the Matern case, the covariance decreases asymptotically towards zero the larger the value of *r*. This is the more distance between a pair of points, the weaker the covariance between them.

The election of which covariance function to use depends on our assumptions about the change in the association between the points across space (eg., the speed of decay).

Geostatistics
-------------

Now that we have discussed how the covariance function can help model spatial dependence, we can discuss how to incorporate this ideas into our model. In our GLM example above we fitted a model of the form

*η*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub>,

Now we will incorporate an spatial component as

*η*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + *f*(*z*<sub>*i*</sub>), where (*f*(*z*<sub>1</sub>),…,*f*(*z*<sub>2</sub>)) is a multivariate Gaussian with spatial covariance *K*.

We can implement this model, assuming a Matern covariance, as shown below.

``` r
# Fit GAM with spatial smooth
m4 <- spaMM::fitme(OM ~ humidity + Matern(1|lng+lat), data=spatial_reg, family=gaussian(link="log"), init=list(lambda=.5, phi=.5))

summary(m4)
```

    ## formula: OM ~ humidity + Matern(1 | lng + lat)
    ## ML: Estimation of corrPars, lambda and phi by ML.
    ##     Estimation of fixed effects by ML.
    ## Estimation of lambda and phi by 'outer' ML, maximizing p_v.
    ## Family: gaussian ( link = log ) 
    ##  ------------ Fixed effects (beta) ------------
    ##             Estimate Cond. SE t-value
    ## (Intercept)   4.3187 0.092593   46.64
    ## humidity      0.5047 0.008029   62.87
    ##  --------------- Random effects ---------------
    ## Family: gaussian ( link = identity ) 
    ##                    --- Correlation parameters:
    ##        1.nu       1.rho 
    ## 0.234445463 0.004405141 
    ##            --- Variance parameters ('lambda'):
    ## lambda = var(u) for u ~ Gaussian; 
    ##    lng + lat  :  0.04179  
    ## # of obs: 300; # of groups: lng + lat, 300 
    ##  ------------- Residual variance  -------------
    ## phi estimate was 0.999906 
    ##  ------------- Likelihood values  -------------
    ##                         logLik
    ## p_v(h) (marginal L): -1195.988

In the next figure, we will show the spatial effect by predicting the values of organic matter across space with a fixed level of humidity.

``` r
# Make predictions on a spatial grid
surf_grid <- as.data.frame(make_grid(size = 20))
surf_grid$humidity <- mean(spatial_reg$humidity) # Assume covariate is constant across all space (for visualization only)
surf_grid$spatial_effect <- predict(m4, newdata=surf_grid, type="response")[, 1]
```

![](week6_spatial_regression_files/figure-markdown_github/plt_smooth-1.png)

Next, we will compare the autocorrelation observed in the residuals of this geostatistic model and the autocorrelation of the residuals of the GLM. As we saw above, the residuals of the GLM were spatially correlated. That is not the case for the geostatistic model.

``` r
# Compute correlogram of the residuals
cor_g <- pgirmess::correlog(coords=spatial_reg[,c("lng", "lat")],
                            z=residuals(m4),
                            method="Moran", nbclass=nbc)

cor_g <- as.data.frame(cor_g)
cor_g$variable <- "residuals_geostatistic"
correlograms <- rbind(correlograms, cor_g)

# Plot both correlograms
ggplot(correlograms, aes(dist.class, coef)) + 
  geom_hline(yintercept = 0, col="grey") +
  geom_line(aes(col=variable)) + 
  geom_point(aes(col=variable)) +
  xlab("distance") + 
  ylab("Moran's coefficient") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
```

![](week6_spatial_regression_files/figure-markdown_github/correlogram_2-1.png)

Cross-Validation
----------------

Cross-validation can be used for for model selection. Once we have different models with their parameters calibrated, we can use cross-valiation to select the one that has a better performance in the data of interest.

The proceedure for doing *k*-fold cross-validation is as follows. Instead of fitting the model to the observed data, we first split the data into *k* subsets. Then we train the model *k* times, each one using only 4 of the groups, and computing some performance metric on the left out group. The performance metric could be the sum of squared errors or any other sensible metric depending on the application. Below we show a diagram of a 5-fold cross validation. ![](week6_spatial_regression_files/figure-markdown_github/k_fold_cv-1.png)

### GLM Surface Trend vs Geostatistic Model

Before introducing the geostatistic models we discussed fitting a GLM using the location of the measurments as covariates. Here we will compare the performance of a GLM with a surface trend with a Geostatistic model using cross-validation.

The code below will split the data using only 3 folds.

``` r
# Copy spatial_reg without residuals
all_data <- spatial_reg[, c("lng", "lat", "humidity", "OM")]

# Make an index with the 2 folds
ix = caret::createFolds(all_data$OM, k = 3)
```

Now we will do 3-fold cross-validation using this the mean squared error as performance metric.

``` r
mse_glm <- c()
mse_geo <- c()
for (i in 1:3) {
  test_set <- all_data[ix[[i]], ] 
  train_set <- all_data[(1:300)[-c(ix[[i]])], ]
  m_glm <- glm(OM ~ humidity + lng + lat + lng*lat, data=train_set, family=gaussian(link="log"))
  m_geo <- spaMM::fitme(OM ~ humidity + Matern(1|lng+lat), data=train_set, family=gaussian(link="log"), init=list(lambda=.5, phi=.5))

  mse_glm[i] <- mean((predict(m_glm, newdata=test_set, type="response") - test_set$OM)^2)
  mse_geo[i] <- mean((predict(m_geo, newdata=test_set, type="response") - test_set$OM)^2)
}

print(mse_glm) # MSE for GLM in each round
```

    ## [1] 690.3978 684.8136 689.3160

``` r
print(mse_geo) # MSE for geostatistic model in each round
```

    ## [1] 228.5151 208.3310 229.2601

``` r
print(mean(mse_glm)) # Average MSE with GLM
```

    ## [1] 688.1758

``` r
print(mean(mse_geo)) # Average MSE with geostatistic model
```

    ## [1] 222.0354

Clearly the geostatisc model showed a better performance than a 1st order surface trend.

Application Example: Malaria Case
---------------------------------

Now we will estimate the prevalence of malaria in Ethiopia, using data from 2009. We have used this dataset in previous sessions. This survey data contains information about number of positive cases and number of examined people in different schools. Spatial information is encoded in the fields *longitude* and *latitude*. To represent this data we can use a Binomial likelihood, that models number of successes out of a number of trials.

``` r
# Load data
malaria_eth <- read.csv("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week6/Lab_files/Data/mal_data_w_covs.csv") # Case data
ETH_Adm_1 <- raster::getData("GADM", country="ETH", level=1) # Admin boundaries

# Plot both country and data points
raster::plot(ETH_Adm_1)
points(malaria_eth$longitude, malaria_eth$latitude,
       pch = 16, ylab = "Latitude", xlab="Longitude", col="red", cex=.5)
```

![](week6_spatial_regression_files/figure-markdown_github/malaria_eth_data-1.png)

The data points in the survey provide us information to learn a rule of how prevalence changes across the country. However, if we want to make a map of the disease prevalence of the whole country we need a set of points that covers all the territory. Then, once we have fitted a model to the survey data, we can predict a these new set of points. To generate a grid of points inside the polygon of the country, we can use the following code.

``` r
# Random grid
s_grid <- sp::spsample(ETH_Adm_1, n = 3000, type = "regular")@coords

# Plot both country and data points
raster::plot(ETH_Adm_1)
points(s_grid[,1], s_grid[,2], pch=16, cex=.5, col="blue")
```

![](week6_spatial_regression_files/figure-markdown_github/eth_grid-1.png)

Here we will not use the object *s\_grid* defined above, but a dataset (generated in a similar way) for which we also have a set of bioclimatic covariates obtained from <https://www.worldclim.org/bioclim>. Note that de dataset *malaria\_eth* also has the same covariates defined for the points in the survey.

``` r
# Random grid
grid_eth <- read.csv("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week6/Lab_files/Data/eth_grid_w_covs.csv")

# Plot both country and data points
head(grid_eth)
```

    ##   longitude latitude elev_m dist_to_water_m bioclim1 bioclim4 bioclim12
    ## 1  39.38805 3.517579    830           14823 232.3875 1543.700  571.1000
    ## 2  37.93084 3.881881    820           10006 235.6000 1292.432  450.6050
    ## 3  38.29515 3.881881   1101            5982 202.8075 1417.385  593.5175
    ## 4  38.65945 3.881881   1271           17478 198.3000 1516.525  656.3950
    ## 5  39.02375 3.881881   1136              34 213.9000 1549.787  636.8875
    ## 6  39.38805 3.881881   1004            5111 226.7000 1462.940  535.2800
    ##   bioclim15
    ## 1  101.5250
    ## 2   98.6150
    ## 3   97.4650
    ## 4   97.5675
    ## 5  100.7000
    ## 6  106.0000

Below we have a map of the bioclimatic variables in the country.

``` r
# Make grid (for plotting purposes)
layer_names <- colnames(grid_eth)[3:8]
plt_covs <- list()
for (i in 1:6) {
  # One plot per variable
  min_val = round(min(grid_eth[, c(layer_names[i])])) + 1
  max_val = round(max(grid_eth[, c(layer_names[i])])) - 1
  
  plt_covs[[i]] <- ggplot(grid_eth, aes(longitude, latitude)) + geom_raster(aes_string(fill=layer_names[i])) +
    viridis::scale_fill_viridis(option="plasma", na.value="darkblue",
                                breaks=c(min_val, max_val), labels=c(min_val, max_val)) +
    coord_fixed(ratio=1) + theme_void() +
    theme(legend.position = "bottom", legend.title = element_blank()) 
}

# Put all plots together
cowplot::ggdraw(xlim = c(0,15), ylim = c(0,10)) +
  cowplot::draw_plot(plt_covs[[1]], x = 0, y = 5, width = 5, height = 5) +
  cowplot::draw_plot(plt_covs[[2]], x = 5, y = 5, width = 5, height = 5) +
  cowplot::draw_plot(plt_covs[[3]], x = 10, y = 5, width = 5, height = 5) +
  cowplot::draw_plot(plt_covs[[4]], x = 0, y = 0, width = 5, height = 5) +
  cowplot::draw_plot(plt_covs[[5]], x = 5, y = 0, width = 5, height = 5) +
  cowplot::draw_plot(plt_covs[[6]], x = 10, y = 0, width = 5, height = 5) +
  cowplot::draw_plot_label(layer_names, x=rep(c(0, 5, 10), 2), y = c(rep(10, 3), rep(5, 3)))
```

![](week6_spatial_regression_files/figure-markdown_github/grid_data-1.png)

Now we fit the model, as we saw in the before.

``` r
prev_eth <- spaMM::fitme(cbind(pf_pos, examined - pf_pos) ~ bioclim1 + bioclim4 + bioclim15 + elev_m + dist_to_water_m + Matern(1|latitude+longitude), data=malaria_eth, family=binomial())
summary(prev_eth)
```

    ## formula: cbind(pf_pos, examined - pf_pos) ~ bioclim1 + bioclim4 + bioclim15 + 
    ##     elev_m + dist_to_water_m + Matern(1 | latitude + longitude)
    ## Estimation of corrPars and lambda by Laplace ML approximation (p_v).
    ## Estimation of fixed effects by Laplace ML approximation (p_v).
    ## Estimation of lambda by 'outer' ML, maximizing p_v.
    ## Family: binomial ( link = logit ) 
    ##  ------------ Fixed effects (beta) ------------
    ##                   Estimate  Cond. SE t-value
    ## (Intercept)     -5.891e+00 1.180e+01 -0.4994
    ## bioclim1         1.294e-02 4.318e-02  0.2996
    ## bioclim4        -4.188e-04 1.831e-03 -0.2287
    ## bioclim15       -2.571e-02 3.536e-02 -0.7271
    ## elev_m          -1.402e-03 2.063e-03 -0.6797
    ## dist_to_water_m -4.147e-05 8.074e-05 -0.5136
    ##  --------------- Random effects ---------------
    ## Family: gaussian ( link = identity ) 
    ##                    --- Correlation parameters:
    ##      1.nu     1.rho 
    ## 0.4461784 1.8070579 
    ##            --- Variance parameters ('lambda'):
    ## lambda = var(u) for u ~ Gaussian; 
    ##    latitude .  :  4.457  
    ## # of obs: 203; # of groups: latitude ., 203 
    ##  ------------- Likelihood values  -------------
    ##                         logLik
    ## p_v(h) (marginal L): -106.0101

Finally, we will display a map of the predicted prevalence values in the grid. On the left we have the prevalence estimates and on the right we have the logit of the predicted prevalence. We are plotting the logit, becouse this is the link funciton used in our model. On the logit-scale the spatial changes or the predicted values are more evident.

``` r
# Make predictions at the points in the grid
grid_eth$prevalence <- predict(prev_eth, newdata = grid_eth, type="response")[,1]
grid_eth$logit_prev <- log(grid_eth$prevalence/(1 - grid_eth$prevalence))

# Plot of prevalence
min_val = round(min(grid_eth[, c("prevalence")]), 4) + .0001
max_val = round(max(grid_eth[, c("prevalence")]), 4) - .0001
plt_1 <- ggplot(grid_eth, aes(longitude, latitude)) + 
  geom_raster(aes(fill= prevalence)) +
  viridis::scale_fill_viridis(option="plasma", na.value="darkblue", 
                              breaks=c(min_val, max_val), labels=c(min_val, max_val)) +
  coord_fixed(ratio=1) + theme_void() +
  theme(legend.position = "bottom")

# Plot of linear predictor
min_val = round(min(grid_eth[, c("logit_prev")]), 2) + .01
max_val = round(max(grid_eth[, c("logit_prev")]), 2) - .01
plt_2 <- ggplot(grid_eth, aes(longitude, latitude)) + 
  geom_raster(aes(fill= logit_prev)) +
  viridis::scale_fill_viridis(option="plasma", na.value="darkblue", 
                              breaks=c(min_val, max_val), labels=c(min_val, max_val)) +
  coord_fixed(ratio=1) + theme_void() +
  theme(legend.position = "bottom")

# Show both plots
cowplot::ggdraw(xlim = c(0,10), ylim = c(0,5)) +
  cowplot::draw_plot(plt_1, x = 0, y = 0, width = 5, height = 5) +
  cowplot::draw_plot(plt_2, x = 5, y = 0, width = 5, height = 5)
```

![](week6_spatial_regression_files/figure-markdown_github/bf_map-1.png)

We have six bioclimatic variables. Are we sure we need all of them in the model? If not, how to decide which one(s) we should remove?

We will do 5-fold cross-validation using this the mean squared error as performance metric. The function below will compute the mean square error across all folds. This function takes as input a dataset, an R formula object (that tells which covariates are used in the model) and a list of indices that describes the data splitting k folds.

``` r
# Function to compute a cross-validated MSE score
cv_eth <- function(data, spamm_formula, ix_test_list) {
  mse <- c()
  for (i in 1:length(ix_test_list)) {
    test_set <- data[ix_test_list[[i]], ] 
    train_set <- data[(1:300)[-c(ix_test_list[[i]])], ]
    model <- spaMM::fitme(spamm_formula, data=train_set, family=binomial())
    model_prediction <- predict(model, newdata=test_set, type="response")[,1]
    mse[i] <- mean((model_prediction * test_set$examined  - test_set$pf_pos)^2)
  }
  return(mean(mse))
}
```

As we saw above, to split the data into folds we call the command

``` r
# Define 5 folds
ix = caret::createFolds(malaria_eth$pf_pos, k = 5)
```

The procedure we will follow to choose the variables to include in the model is know as Backward Selection. We start with a model that contains all variables and compute the CV-MSE. We then remove one variable at a time, refit the model and compute the CV-MSE. If the best of these new models (with one less variable) outperforms the model with all variables, then select this new model as the optimal. Hence we will have decided which variable to remove. Afterwards we repeat the same proceedure removing a new variable from the ones that are still included in the new model. The code to carry on the Backward Selection method is below.

``` r
formula_kern <- "cbind(pf_pos, examined - pf_pos) ~ Matern(1|latitude+longitude)"
formula_model <- paste(c(formula_kern, layer_names), collapse = " + ")
scores <- c(cv_eth(malaria_eth, as.formula(formula_model), ix))
```

    ## maxeval reached in nloptr(); nloptr() called again until apparent convergence of objective.

``` r
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
    scores_iter <- c(scores_iter, cv_eth(malaria_eth, as.formula(formula_model), ix))
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
```

Here is a summary of the results. The covariates are indexed as: 1 elevation, 2 distance to water, 3 bioclim1, 4 bioclim4, 5 bioclim12 and 6 bioclim15.

``` r
print(board)
```

    ##        MSE  Covariates
    ## 1 2.025537 1,2,3,4,5,6
    ## 2 1.958220   1,3,4,5,6
    ## 3 1.930581     3,4,5,6
    ## 4 1.929210       3,4,6
    ## 5 1.917555         3,4
    ## 6 1.943383           3

According to this results the best model according to the lowest MSE is achieved when using only variables 3 and 4, this is bioclim1 and bioclim4.

Conclusion
----------

In this session we learnt the basic concepts of spatial regression. We saw how the spatial covariance is an essential component of a spatial model. By encoding the spatial association into a kernel function, a geostatistic model outperforms linear models even when they include a polynomial representation of the observations coordinates. When properly accounting for the spatial structure of the data, the residuals of the model are independent.

We also reviewed the concept of cross-validation as a means to select model. In particular we saw how it can help determine which covariates to include in the model.