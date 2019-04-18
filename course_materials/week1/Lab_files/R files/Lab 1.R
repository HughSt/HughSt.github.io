###Lab 1: Working with Spatial Data in R
# Normally this type of analysis deals with three types of files:
# Shapefiles, geojson, or tables with coordinates

#Start by ensuring the following packages are loaded
library(sp)
library(raster)
library(rgdal)

# The simplest data is a table with coordinates (i.e. point data)
# For this assignment, we'll work with malaria prevalence point data from Burkina Faso

# First set your working directory to the folder containing the data files 
# for week 1
setwd("your file path here")

##Plotting Point Data
#Import the data
BF_malaria_data <- read.csv("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week1/Lab_files/Data/BF_malaria_data.csv",
                            header=T)

# The columns should be self-explanatory, but briefly:
# examined = numbers tested
# positives = of those tested, how many were positive for malaria
# longitude = longitude in decimal degrees
# latitude = latitude in decimal degrees

head(BF_malaria_data) # gives you the first few rows
names(BF_malaria_data) # gives you the column names

# If you want to create a new variable, you can use the $ sign
#Example: Prevalence
BF_malaria_data$infection_prevalence <- BF_malaria_data$positives / BF_malaria_data$examined

#Create a histogram of the prevalence
hist(BF_malaria_data$infection_prevalence, breaks=20)

# Use R's basic plotting function to plot 
plot(BF_malaria_data$longitude, BF_malaria_data$latitude,
     ylab = "Latitude", xlab="Longitude") #boring!

# Use the cex function to plot circle size as a function of a variable
plot(BF_malaria_data$longitude, BF_malaria_data$latitude, 
     cex=BF_malaria_data$infection_prevalence,
     ylab = "Latitude", xlab="Longitude (decimal degrees)")

#Manipulate the prevalence column so the data is divided into 4 classes
prev_class <- as.numeric(cut(BF_malaria_data$infection_prevalence, 4))
prev_class

#Add color
color_class <- c("blue", "yellow", "orange", "red")
color_class
color_class[prev_class]
#Can you create your own color scheme?

# Use the cex function to plot
# Do some research on your own... what does the cex function do?
plot(BF_malaria_data$longitude, BF_malaria_data$latitude, 
     cex=BF_malaria_data$infection_prevalence,
     ylab = "Latitude", xlab="Longitude", 
     col=color_class[prev_class])

# With filled circles
plot(BF_malaria_data$longitude, BF_malaria_data$latitude, 
     cex=BF_malaria_data$infection_prevalence,
     ylab = "Latitude", xlab="Longitude", 
     col=color_class[prev_class],
     pch=16)
#Can you change the symbol to something other than circles?

# With larger filled circles
plot(BF_malaria_data$longitude, BF_malaria_data$latitude, 
     cex=BF_malaria_data$infection_prevalence*2,
     ylab = "Latitude", xlab="Longitude", 
     col=color_class[prev_class],
     pch=16)
#Can you change the size of the circle ?

##Creating a SpatialPointsDataFrame
# Let's make a SpatialPointsDataFrame object (useful for other operations)
BF_malaria_data_SPDF <- SpatialPointsDataFrame(coords = BF_malaria_data[,c("longitude", "latitude")],
                                      data = BF_malaria_data[,c("examined", "positives", "infection_prevalence")],
                                      proj4string = CRS("+init=epsg:4326")) # WGS 1984 using lat/long. Optional but good to specify

# Summary of object
BF_malaria_data_SPDF

# SPDFs partition data elements, e.g. the coordinates are stored separately from the data
BF_malaria_data_SPDF@coords
BF_malaria_data_SPDF@data

# You can quickly access the data frame as per a standard data frame, e.g.
BF_malaria_data_SPDF$infection_prevalence

# You can use the plot or spplot function to get quick plots
plot(BF_malaria_data_SPDF)
spplot(BF_malaria_data_SPDF, zcol = "infection_prevalence")
# Play around with the spplot function...
# Can you change the legend location, legend breaks, color gradient?


# The readOGR function is useful for reading in spatial files such as shapefiles and GeoJSON
# For example, here we can read in admin 1 level data for Burkina Faso
BF_Adm_1 <- readOGR("BF_Adm_1_shapefile", "BF_Adm_1")

# The raster package allows you to pull data admin boundaries from GADM to get country info
ccodes()
BF_Adm_1 <- raster::getData("GADM", country="BFA", level=1) # Same file as read in on line

# Let's take a look at that object
BF_Adm_1
plot(BF_Adm_1)

# Plot both country and data points
plot(BF_Adm_1)
points(BF_malaria_data$longitude, BF_malaria_data$latitude, 
     cex=BF_malaria_data$infection_prevalence*2,
     ylab = "Latitude", xlab="Longitude", 
     col=color_class[prev_class],
     pch=16)


## Plotting Over a Web Map
library(leaflet)

#leaflet allows you to layer a basemap using the pipe command %>%
basemap <- leaflet() %>% addTiles()
basemap

# Or choose another basemap
basemap <- leaflet() %>% addProviderTiles("Esri.WorldImagery")
basemap

#Let's choose a simple one
basemap <- leaflet() %>% addProviderTiles("CartoDB.Positron")

#You can use the 'piping' command %>% to add layers
#As our point and polygon data are already SP object this is easy
basemap %>% addPolygons(data=BF_Adm_1)

#Can you play with the way it plots?
# e.g. to change the colors/line weight
basemap %>% addPolygons(data=BF_Adm_1, color = "red", 
                        weight = 1, fillOpacity = 0.2)

#You can also add popups
basemap %>% addPolygons(data=BF_Adm_1, 
                        popup = BF_Adm_1$NAME_1)
#Click on the map in the Viewer to confirm the popus loaded

# If you want to add points as well
basemap %>% addPolygons(data=BF_Adm_1, weight = 2,
                        popup = BF_Adm_1$NAME_1) %>%
  
            addCircleMarkers(data=BF_malaria_data_SPDF,
                             color="red", radius = 2)

#If you want to change the color according to a variable, i.e. prevalence, you can use the colorNumeric function
library(wesanderson) # for a nice color palette
colorPal <- colorQuantile(wes_palette("Zissou1")[1:5], BF_malaria_data_SPDF$infection_prevalence, n = 5)

# colorPal is now a function you can apply to get the corresponding
# color for a value
colorPal(0.6)

basemap %>% addPolygons(data=BF_Adm_1, weight = 2, fillOpacity=0,
                        popup = BF_Adm_1$NAME_1) %>%
  
  addCircleMarkers(data=BF_malaria_data_SPDF,
                   color = colorPal(BF_malaria_data_SPDF$infection_prevalence), 
                   radius = 2,
                   popup = as.character(BF_malaria_data_SPDF$infection_prevalence))
#Again, can you apply your own color pallete?

# Might want to add a legend. This just goes on as another layer
# First calculate the labels. In this case, the quantiles of prevalence
basemap %>% addPolygons(data=BF_Adm_1, weight = 2, fillOpacity=0,
                        popup = BF_Adm_1$NAME_1) %>%
  
  addCircleMarkers(data=BF_malaria_data_SPDF,
                   color = colorPal(BF_malaria_data_SPDF$infection_prevalence), 
                   radius = 2,
                   popup = as.character(BF_malaria_data_SPDF$infection_prevalence)) %>%
  
  addLegend(colors=wes_palette("Zissou1")[1:5], 
            title = "Quintile",
            labels = c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1"))



# For more complex popups, you can define the html
basemap %>% addPolygons(data=BF_Adm_1, weight = 2, fillOpacity=0,
                        popup = BF_Adm_1$NAME_1) %>%
  
  addCircleMarkers(data=BF_malaria_data_SPDF,
                   color = colorPal(BF_malaria_data_SPDF$infection_prevalence), 
                   radius = 2,
                   popup = paste("<p>","Prevalence:",
                                 round(BF_malaria_data_SPDF$infection_prevalence,2),
                                 "<p>"))
#Click on the points. What does the popup say now?

##Plotting Raster Data
# Import the raster file
elev <- raster("elev_BFA.tif")

# Can also use the raster package getData function (very useful!)
elev <- raster::getData("alt", country="BFA")
elev

# You can plot using the standard plot function
plot(elev)

# ...and you can use leaflet
basemap %>% addRasterImage(elev)

# If you want to add a legend, you have to define the color palette first
raster_colorPal <- colorNumeric(topo.colors(64), values(elev), na.color = NA)
raster_colorPal(500)

basemap %>% addRasterImage(elev, color = raster_colorPal) %>%
addLegend(values = values(elev), pal = raster_colorPal)

# If you want to export the data, there are several options.
# 1. Export button
# 2. Save as kml for someone to open in Google Earth
library(plotKML)
plotKML(BF_malaria_data_SPDF) # see ?plotKML for more options





