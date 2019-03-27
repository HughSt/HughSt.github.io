## Week 5: Remote Sensing

#Set your directory
setwd("...")

# Load spatial packages
#install.packages("raster")
library(raster)
#install.packages("sp")
library(sp)
#install.packages("rgdal")
library(rgdal)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rasterVis")
library(rasterVis)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("randomForest")
library(randomForest)

###################################
# Read in and visualize the Landsat files
####################################

# get list of all tifs- note the full path, 
# relative to our working directory, is included
# ./Landsat will look at all the files in the 
# folder 'Landsat'
all_Landsat <- list.files("./Landsat",
                          full.names = TRUE)

# view list
all_Landsat

# Create a raster stack of the Landsat data
Landsat_stack <- stack(all_Landsat)

# Get a summary of the raster stack. This is
# like a rasterLayer but instead of having a single
# raster layer, it is made up of several identical
# size/resolution rasters. Now in the 'dimensions'
# parameter there is nlayers which shows you how many
# raster layers are in the stack
Landsat_stack

#Plot landsat bands
plot(Landsat_stack)

#Plot True and False Color Composite of the Landsat data
par(mfrow = c(1,1))

#False Color Composite
plotRGB(Landsat_stack, 5,4,3, 
        stretch = 'lin', 
        main = "Landsat False Color Composite")  #axes = TRUE,

#True Color Composite
plotRGB(Landsat_stack, 4,3,2, stretch = 'lin', main = "Landsat True Color Composite") 

# Display names of each individual layer
names(Landsat_stack)

#Rename bands
names(Landsat_stack) <- c('band1', 'band2', 'band3', 'band4', 'band5', 'band6', 'band7')


###################################
#Train the classifier using training samples
####################################

### Read in the csv file.
training <-  read.csv("LULC_Training_Tana.csv", header=T)

### Convert training into a SpatialPointsDataFrame
coordinates(training) <- c("Longitude", "Latitude")

  # note that this is a shortcut way alternative to
  # training <- SpatialPointsDataFrame(coords = training[,c("Longitude", "Latitude")],
  #                                 data = data.frame(LULC=training$LULC))

### Extract values from the image for the point locations.
extract <- as.data.frame(raster::extract(Landsat_stack, training))
head(extract) 

# Now you have extracted the reflectance values at
# each training data point, you can fit a classification model
# to predict Land use land class given a set of 
# reflectance values

# Here we are going to use a random forest model
# but there are a number of classification models
# you could use including a multinomial glm
# For a random forest we are going to split
# our data into predictors (reflectance values)
# and outcome (LULC)
lulctable <- as.data.frame(training)
ydata <- as.factor(lulctable[,"LULC"])
ydata

xdata <- extract
xdata

# FIT FINAL RF MODEL
cat("Starting to calculate random forest object\n")
RF_Model <- randomForest(x=xdata, 
                         y=ydata) 

#Confusion Matrix showing the out-of-bag
# estimates which are the internally cross-validated
# results. Note that this is specific to random forests
# for other classification models you would have to 
# do this manually.
#Savanna=9, Grassland=10, Wetland=11, Cropland=12, Urban=13, Water=17 
print(RF_Model)

################################
#Classify
################################

# Now we have a model of the relationship between
# LULC and reflectance values, we can predict LULC
# over our entire region

#Start predictions
cat("Starting predictions\n")

# Name and path of the output GeoTiff image
outImage <- "Model_2017.tif"
pr2017 <- predict(Landsat_stack, 
                  RF_Model, 
                  filename=outImage, 
                  format='GTiff', 
                  datatype='INT1U', 
                  progress='text', 
                  type='response',  
                  overwrite=TRUE) 
pr2017

## Plotting
par(mfrow = c(1,1))

# Define a colour scale for the classes (as above)
# corresponding to: Savanna, Grassland, Wetland, Cropland, Urban, Water 
cols <- c("#e6550d", "#31a354", "#9ecae1", "#addd8e", "#c51b8a", "#2c7fb8")

## Plot without a legend
plot(pr2017, col=cols, legend=FALSE)

## Add a customized legend
legend("bottomleft", 
       legend=c("Savanna", "Grassland", "Wetland", "Cropland", "Urban", "Water"), 
       fill=cols, 
       bg="white")

