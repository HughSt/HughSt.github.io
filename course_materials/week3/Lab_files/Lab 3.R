###Lab 3: Spatial Variation in Risk

# Load relevant libraries
install.packages("spatstat")
install.packages("lme4")
install.packages("lgcp")
install.packages("geoR")
install.packages("gtools")
install.packages("Metrics")
library(Metrics)
library(spatstat)
library(raster)
library(sp)
#library(lgcp)
library(geoR)
library(gtools)
library(lme4)
library(leaflet)
library(oro.nifti)

source("assignment 3.R")

# Open Namibia malaria case data
setwd("...")
CaseControl<-read.csv("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week3/Lab_files/CaseControl.csv")

#Create a new object with just the cases, recoded as a number 1
Cases<-CaseControl[CaseControl$case==1,]

#Create a new object with just the controls, recoded as a number 0
Controls<-CaseControl[CaseControl$case==0,]

# And boundary file
NAM_Adm0<-raster::getData('GADM',country='NAM',level=0)

# Convert to a SPDF
CaseControl_SPDF <- SpatialPointsDataFrame(coords = CaseControl[,c("long", "lat")],
                                           data = CaseControl[,c("household_id", "case")])

# Let's plot and see what we have
#First, create a color scheme based on the case classification (0 or 1)
case_color_scheme <- colorNumeric(c("blue", "red"), CaseControl_SPDF$case)
#Then, plot
leaflet() %>% addTiles() %>% addCircleMarkers(data=CaseControl_SPDF, 
                                              color = case_color_scheme(CaseControl_SPDF$case),
                                              radius = 2)
#Can you change the colors? Can you color the points based on another column?

## Risk Mapping using Kernel Density
# To generate a kernel density estimate, we first 
# need to generate point pattern object of points (aka ppp)
# First need to define a window
Nam_Owin <- owin(xrange=range(CaseControl$long),yrange=range(CaseControl$lat))

# Now define a ppp of the case data
Cases_ppp <- ppp(Cases$long, Cases$lat, window = Nam_Owin)
plot(Cases_ppp)
#Can you rename this plot so it has a clearer title?

# Plot kernel density estimate
plot(density(Cases_ppp)) # Units are intensity of points per unit square

# Try with different bandwidths 
plot(density(Cases_ppp,0.02))
plot(density(Cases_ppp,0.1))
plot(density(Cases_ppp,bw.ppl)) # automatic bandwidth selection based on cross-validation

# Map on leaflet
#It needs to be a rasterLayer object, so let's convert it first
density_raster <- raster(density(Cases_ppp, bw.ppl), crs = crs(NAM_Adm0))
#What is the crs parameter again?

#Now plot the raster
leaflet() %>% addTiles() %>% addRasterImage(density_raster, opacity=0.6)

# But this is just a density of cases, e.g. 
# it doesn't account for the denominator - the controls
# To do this, we can use the kelsall & diggle method, 
# which calculates the ratio of the
# density estimate of cases:controls

# First we have to add 'marks' to the points
# Marks are just values associated with each point
# such as case or control (1/0) 
CaseControl_ppp <- ppp(CaseControl$long, CaseControl$lat, 
                       window = Nam_Owin, 
                       marks=as.factor(CaseControl$case))

# If the 'relative' argument is not included in the code line, 
# then it is technically specified as 'FALSE',
# because that is the default. 
# The output is, therefore, the probability of
# being a case. Sigma is the bandwidth
risk_est <-  relrisk(CaseControl_ppp, sigma = 0.1) 
plot(risk_est)

# However, if you specify relative = TRUE in the code line,
# then the output is the relative risk, 
# the (probability of being a case, relative to probability of being a control)
rel_risk_est <-  relrisk(CaseControl_ppp, relative = TRUE, sigma = 0.1)
plot(rel_risk_est)

# to plot on a web map, first specify the projection
risk_raster <- raster(risk_est, crs = crs(NAM_Adm0))

# Then define a color palette
pal = colorNumeric(palette=tim.colors(64), domain=values(risk_raster), na.color = NA)

#Then plot with leaflet
leaflet() %>% addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>% 
  addRasterImage(risk_raster, opacity=0.6, col = pal)


## Interpolation of point (prevalence etc.) data
# Open BF malaria data
BF_malaria_data <- read.csv("BF_malaria_data.csv",
                            header=T)

# Get the Burkina Faso Adm 1 level raster
BF_Adm_1 <- raster::getData("GADM", country="BFA", level=1)

# Calculate prevalence
BF_malaria_data$prevalence <- BF_malaria_data$positives / BF_malaria_data$examined

## Inverse distance weighting (IDW)
# Once again, start by setting the window
BF_malaria_window<-owin(xrange=range(BF_malaria_data$longitude),yrange=range(BF_malaria_data$latitude))

#Then define a ppp of the prevalence data
BF_malaria_data_ppp<-ppp(BF_malaria_data$longitude,BF_malaria_data$latitude,
                         marks=BF_malaria_data$prevalence,window=BF_malaria_window)

#Set the parameters for displaying multiple plots in one screen
par(mfrow=c(2,2))

#Now plot different IDW results
# power represents the power function we want to use
# 'at' can be 'pixels' where it generates estimates across a grid of pixels
# or 'points' where it interpolates values at every point using 
# leave-one-out-cross validation
plot(idw(BF_malaria_data_ppp, power=0.2, at="pixels"),col=heat.colors(20), main="power = 0.2") 
plot(idw(BF_malaria_data_ppp, power=0.5, at="pixels"),col=heat.colors(20), main="power = 0.5")
plot(idw(BF_malaria_data_ppp, power=1, at="pixels"),col=heat.colors(20), main="power = 0.1")
plot(idw(BF_malaria_data_ppp, power=2, at="pixels"),col=heat.colors(20), main="power = 2") # Larger power puts more weight on nearer values

# Plot using leaflet. Again, 
# Convert to a raster
BF_malaria_prev_idw_raster <- raster(idw(BF_malaria_data_ppp, power=0.2, at="pixels"),
                                     crs= crs(BF_Adm_1))

#Define a color paletter
colPal <- colorNumeric(tim.colors(), BF_malaria_prev_idw_raster[], na.color = NA)

#Plot
leaflet() %>% addTiles() %>% addRasterImage(BF_malaria_prev_idw_raster, col = colPal, opacity=0.7) %>%
  addLegend(pal = colPal, values = BF_malaria_prev_idw_raster[])


# To calculate the 'best' power to use, you can use cross-validation. 
CV_idw_1<-idw(BF_malaria_data_ppp, power=1, at="points")
plot(BF_malaria_data_ppp$marks, CV_idw_1, asp=1) # Not very good! 'asp=1' makes the plot aspect ratio equal for x and y axes
# Can you edit the plot axis labels so they are more clear? Can you change the circles to a different color? To a different size? To a different shape altogether?

# Calculate Mean Squared Error (MSE)
mse(BF_malaria_data_ppp$marks,CV_idw_1) # Mean squared error


## Kriging
# Before Kriging, good to transform prevalence 
# data to something vaguely normal
# Here we use the logistic transformation (log odds)
BF_malaria_data$log_odds <- logit(BF_malaria_data$prevalence)
hist(BF_malaria_data$log_odds)


# (Do you notice anything about the way your plots are displaying? Most likely still in the 2x3 structure -- 
# that's because you changed the global parameters, remember? To reset back to one at a time par(mfrow=c(1,1))

# First have to create a geodata object with the package GeoR
# this wants dataframe of x,y and data 
BF_malaria_data_geo<-as.geodata(BF_malaria_data[,c("longitude","latitude","log_odds")])

# We can plot a summary plot using ther Lowes parameter
# The Lowes option gives us lowes curves for the relationship between x and y
plot(BF_malaria_data_geo, lowes=T)

#We can see from the result there is potentially a trend on x and y

#Let's try to plot with the trend parameter, which regresses on x and y 
# i.e. fits a regression model with x and y as predictors
# and log odds as the outcome and then looks at the residuals.
plot(BF_malaria_data_geo, lowes=T,trend="2nd")

# Now generate and plot a variogram
# We will divide the code line by 2 because the
# max distance you should estimate is half max
# interpoint distance
MaxDist=max(dist(BF_malaria_data[,c("longitude","latitude")]))  /2 
VarioCloud<-variog(BF_malaria_data_geo,option="cloud",max.dist=3)
plot(VarioCloud) # all pairwise comparisons

# To produce binned variogram
Vario<-variog(BF_malaria_data_geo, max.dist = MaxDist)
plot(Vario)

#Change the parameters a bit
# uvec controls the binning
Vario<-variog(BF_malaria_data_geo,max.dist=MaxDist,uvec=seq(0.01,MaxDist,0.12)) 
#Let's look at the number in each bin
Vario$n
#What is the minimum? Should be at least 30 in each bin
min(Vario$n)
#Plot
plot(Vario,pch=16)
#What does the pch parameter control?

# Fit variogram model by minimized least sqaures
# using different covariance models
VarioMod_lin<-variofit(VarioCloud, cov.model = "linear") 
VarioMod_sph<-variofit(VarioCloud, cov.model = "sph")
VarioMod_exp<-variofit(VarioCloud, cov.model = "exp")

# plot results
lines(VarioMod_lin,col="green",lwd=2)
lines(VarioMod_sph,col="blue",lwd=2)
lines(VarioMod_exp,col="red",lwd=2) 
# In this example, all models converge on essentially the same line

VarioMod_lin
VarioMod_sph
VarioMod_exp
# The lines model has lower sum of squares so 'better'

# Use variogram to Krig values at prediction locations 
# First get grid of points from the IDW example for comparison
# could use the expand.grid function 
IDW<-idw(BF_malaria_data_ppp, power=0.2, at="pixels")
pred_grid_x<-rep(IDW$xcol,length(IDW$yrow))
pred_grid_y<-sort(rep(IDW$yrow,length(IDW$xcol)))
pred_grid<-cbind(pred_grid_x,pred_grid_y)


KrigPred <- krige.conv(BF_malaria_data_geo, loc=pred_grid,
                       krige=krige.control(obj.model=VarioMod_lin))

# Visualize predictions
image(KrigPred,col=heat.colors(50))

# Back transform to prevalence
KrigPred_prev<-inv.logit(KrigPred$predict)
KrigPred_raster <- rasterFromXYZ(data.frame(x=pred_grid_x,
                                 y=pred_grid_y,
                                 z=KrigPred_prev))

plot(KrigPred_raster)
points(BF_malaria_data[,c("longitude","latitude")],
       cex = BF_malaria_data$prevalence)


# Its straightforward to CV kriged predictions in geoR
xvalid_result <- xvalid(BF_malaria_data_geo, model = VarioMod_lin)
# By default it xvalidates point by point

#Plot on log odds scale
plot(xvalid_result$data,xvalid_result$predicted, asp=1)
#Plot the 
abline(0,1)







