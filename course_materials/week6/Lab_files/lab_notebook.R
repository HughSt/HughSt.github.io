library(ggplot2)
source("auxiliary_functions.R")
options(repr.plot.width=7, repr.plot.height=5)

# Fit linear model on dataset 1
dset1 <- univariate_lm()
m1 <- lm(y ~ x, data = dset1)
m1_pred <- predict(m1, newdata = dset1, interval = "confidence")
dset1$y_hat <- m1_pred[,1]
dset1$y_lwr <- m1_pred[,2]
dset1$y_upr <- m1_pred[,3]

plt <- ggplot(data=subset(dset1), aes(x, y)) + 
        geom_point(col="steelblue", size=2) + 
        geom_line(aes(x, y_hat), col="red") +
        geom_ribbon(aes(ymin=y_lwr, ymax=y_upr), fill="magenta", alpha=.25) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
plt

# Fit linear model on dataset 2
dset2 <- univariate_glm()
m2 <- lm(y ~ x, data = dset2)
m2_pred <- predict(m2, newdata = dset1, interval = "confidence")
dset2$y_hat <- m2_pred[,1]
dset2$y_lwr <- m2_pred[,2]
dset2$y_upr <- m2_pred[,3]

plt <- ggplot(data=subset(dset2), aes(x, y)) + 
        geom_point(col="steelblue", size=2) + 
        geom_line(aes(x, y_hat), col="red") +
        geom_ribbon(aes(ymin=y_lwr, ymax=y_upr), fill="magenta", alpha=.25) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
plt

# Transform data with logit function
plt <- ggplot(data=subset(dset2), aes(x, log(y/(1-y)))) + 
        geom_point(col="steelblue", size=2) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
plt

# Fit GLM to dataset 2
dset2 <- univariate_glm()
m2 <- glm(y ~ x, data = dset2, family = quasibinomial)
m2_pred <- predict.glm(m2, newdata = dset2, se.fit = TRUE, type="response")
dset2$y_hat <- m2_pred$fit
dset2$y_lwr <- m2_pred$fit - 1.96 * m2_pred$se.fit
dset2$y_upr <- m2_pred$fit + 1.96 * m2_pred$se.fit

plt <- ggplot(data=subset(dset2), aes(x, y)) + 
        geom_point(col="steelblue", size=2) + 
        geom_line(aes(x, y_hat), col="red") +
        geom_ribbon(aes(ymin=y_lwr, ymax=y_upr), fill="magenta", alpha=.25) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
plt

# load toy dataset for example
spatial_reg <- soil_data(n_peaks=2, n_data = 150, seed=0)
head(spatial_reg)

# Plot soiliness vs covariate
plt <- ggplot(data=subset(spatial_reg), aes(x, soiliness)) + 
        geom_point(col="steelblue", size=2) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
plt

# Plot log soiliness vs covariate
plt <- ggplot(data=subset(spatial_reg), aes(x, log(soiliness))) + 
        geom_point(col="steelblue", size=2) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
plt

# Fit GLM to toy dataset
m3 <- glm(soiliness ~ x, data=spatial_reg, family=gaussian(link="log"))
summary(m3)

# Plot residuals
spatial_reg$residuals <- m3$residuals
gg <- ggplot(spatial_reg, aes(lng, lat)) + 
        geom_point(aes(col=residuals), size=2.5) +
        viridis::scale_color_viridis(option="plasma") +
        theme_void()
plt

# Compute correlogram of the residuals
nbc <- 10
cor_r <- pgirmess::correlog(coords=spatial_reg[,c("lng", "lat")],
                            z=spatial_reg$residuals,
                            method="Moran", nbclass=nbc)

correlograms <- as.data.frame(cor_r)
correlograms$variable <- "residuals_glm" 

# Plot correlogram
plt <-  ggplot(subset(correlograms, variable=="residuals_glm"), aes(dist.class, coef)) + 
        geom_hline(yintercept = 0, col="grey") +
        geom_line(col="steelblue") + 
        geom_point(col="steelblue") +
        xlab("distance") + 
        ylab("Moran's coefficient")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
plt

# Fit GAM with spatial smooth
m4 <- mgcv::gam(soiliness ~ x +s(lng, lat), data=spatial_reg, family=gaussian(link="log"))
summary(m4)

# Make predictions on a spatial grid
surf_grid <- as.data.frame(make_grid(size = 20))
surf_grid$x <- mean(spatial_reg$x) # Assume covariate is constant across all space (for visualization only)
surf_grid$spatial_effect <- mgcv::predict.gam(m4, newdata = surf_grid)

# Plot smooth surface
plt <- ggplot(surf_grid, aes(lng, lat)) + 
        geom_raster(aes(fill=spatial_effect)) +
        geom_contour(aes(z=spatial_effect), col="white", linetype=1, alpha=.5) +
        viridis::scale_fill_viridis(option="plasma", na.value="darkblue") +
        theme_void()
plt

# Compute correlogram of the residuals
cor_g <- pgirmess::correlog(coords=spatial_reg[,c("lng", "lat")],
                            z=m4$residuals,
                            method="Moran", nbclass=nbc)

cor_g <- as.data.frame(cor_g)
cor_g$variable <- "residuals_gam"
correlograms <- rbind(correlograms, cor_g)

# Plot both correlograms
plt <-  ggplot(correlograms, aes(dist.class, coef)) + 
        geom_hline(yintercept = 0, col="grey") +
        geom_line(aes(col=variable)) + 
        geom_point(aes(col=variable)) +
        xlab("distance") + 
        ylab("Moran's coefficient") +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
plt

# K-fold validation diagram
k = 5
label_ <- rep("Train", k^2)
for(i in 1:k) {
    label_[k*(i-1) +(k+1-i)] <- "Validation"
}
cvplot <- data.frame(x=rep(1:k, k), y=sort(rep(1:k, k), decreasing = TRUE), label=label_)

plt <- ggplot(cvplot, aes(x, y)) + geom_tile(aes(fill=label)) + 
        geom_vline(xintercept = 0:k + .5, col='white') +
        geom_hline(yintercept = 0:k + .5, col="white", size=10) +
        scale_y_continuous(breaks = 1:k, labels = paste("fold", 1:k)) +
        scale_x_continuous(breaks = 1:k, labels = paste("subset", 1:k)) +
        #ggtitle(paste0(k, "-Fold Validation Diagram")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
        ylab('') + xlab('')
plt

# First we split the data into two sets:
all_data <- soil_data(n_peaks=2, n_data = 150, seed=0)

# Make an index with the 2 folds
ix = caret::createFolds(all_data$soiliness, k = 2)
train_set <- all_data[ix$Fold1, ]
test_set <- all_data[ix$Fold2, ]

# Cross-Validation with one split only: # Fit two different models
gam1 <- mgcv::gam(soiliness ~ x + s(lng, lat), data=train_set, family=gaussian(link="log"))
gam2 <- mgcv::gam(soiliness ~ x + s(lng, lat), data=train_set, family=quasipoisson(link="log"))

# Make predictions on the test set
pred_gam1 <- mgcv::predict.gam(gam1, newdata=test_set, type="response")
pred_gam2 <- mgcv::predict.gam(gam2, newdata=test_set, type="response")

# Compute mean squared errors
mse1 <- mean((pred_gam1 - test_set$soiliness)^2)
mse2 <- mean((pred_gam2 - test_set$soiliness)^2)

print(paste('GAM1:', mse1))
print(paste('GAM2:', mse2))

# 5 fold Cross-Validation 
ix = caret::createFolds(spatial_reg$soiliness, k = 5)
mse1 = 0
mse2 = 0
for (i in 1:5){
    # Split data
    train_set <- spatial_reg[-ix[[i]], ] 
    test_set <- spatial_reg[ix[[i]], ]

    # Train models
    h1 <- mgcv::gam(soiliness ~ x + s(lng, lat), data = train_set, family = gaussian(link="log"))
    h2 <- mgcv::gam(soiliness ~ x + s(lng, lat), data = train_set, family = quasipoisson(link="log"))
    # Predict hold-out data
    y1_hat <- mgcv::predict.gam(h1, newdata = test_set, type="response")
    y2_hat <- mgcv::predict.gam(h2, newdata = test_set, type="response")
    # Compute SSE
    mse1 <- mse1 + mean((test_set$soiliness - y1_hat)^2)
    mse2 <- mse2 + mean((test_set$soiliness - y2_hat)^2)
    print(c(paste("Fold", i), mean((test_set$soiliness - y1_hat)^2), mean((test_set$soiliness - y2_hat)^2)))
}
print(c("Overall MSE", mse1/5, mse2/5))

# Load data
bfcase <- read.csv("BF_malaria_data.csv")
bfelev <- raster::getData("alt", country="BF")

# Add elevation data to bfcase dataset
bfcase$elevation <- raster::extract(x=bfelev, y=bfcase[, c("longitude", "latitude")])
head(bfcase)

# Make grid (for plotting purposes)
bfgrid <- as.data.frame(raster::xyFromCell(bfelev, seq(bfelev[])[!is.na(bfelev[])]))
colnames(bfgrid) <- c("longitude", "latitude")
bfgrid$elevation <- bfelev[!is.na(bfelev)]

# Plot data
plt <- ggplot(bfgrid, aes(longitude, latitude)) + 
    geom_raster(aes(fill=elevation)) +
    geom_point(data=bfcase, aes(longitude, latitude), col="green") +
    viridis::scale_fill_viridis(option="plasma", na.value="darkblue") +
    theme_void()
plt

# Fit GAM with binomial distribution
bfcase$prop <- bfcase$positives / bfcase$examined
m6 <- mgcv::gam(prop ~ elevation + s(longitude, latitude), weights = examined, data = bfcase, family = binomial(link="logit"))
summary(m6)

# Plot map of prevalence
bfgrid$prevalence <- mgcv::predict.gam(m6, newdata = bfgrid, type="response")
plt <- ggplot(bfgrid, aes(longitude, latitude)) + 
        geom_raster(aes(fill=prevalence)) +
        viridis::scale_fill_viridis(option="plasma", na.value="darkblue") +
        theme_void()
plt
