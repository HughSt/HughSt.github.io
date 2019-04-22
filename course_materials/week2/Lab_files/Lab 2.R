###Lab 2: Manipulating Spatial Data

#Please ensure the following packages are loaded
# Remember, if you don't already have a package
# install it first. i.e. for geosphere package
install.packages("geosphere")

library(sp)
library(raster)
library(leaflet)
library(oro.nifti)
library(rgdal)
library(geosphere)
library(rgeos)

# First set your working directory to the folder containing the files 
# for week 2
setwd("your file path here")

##Subsetting Data
# Let's get admin 1 data for Burkina Faso
BF_Adm_1 <- raster::getData("GADM", country="BFA", level = 1)

# You can subset a SpatialPolygonsDataFrame just like a data frame
#Let's subset the data first by row
BF_Adm_1_cropped <- BF_Adm_1[1,]
BF_Adm_1_cropped

#Plot the result
plot(BF_Adm_1)
lines(BF_Adm_1_cropped, col="red", lwd=2)
# Can you change the color of the line? The thickness of the line?

# You can also subset by name
BF_Adm_1_Cascades <- subset(BF_Adm_1, BF_Adm_1$NAME_1=="Cascades") #OR BF_Adm_1[BF_Adm_1$NAME_1=="Cascades",] will also work
BF_Adm_1_Cascades
#What is "Cascades"? Where did it come from? What function could you run on BF_Adm_1 to find other names like Cascades?

#Plot the result
plot(BF_Adm_1)
lines(BF_Adm_1_Cascades, col="blue", lwd=2)
#Why did the outline change geographic location?

## Projections 
# Now read in the BF malaria data
# (This one has no covariates, we'll get these ourselves
BF_malaria_data <- read.csv("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week1/Lab_files/Data/BF_malaria_data.csv",header=T)

# Convert to a SPDF
BF_malaria_data_SPDF <- SpatialPointsDataFrame(coords = BF_malaria_data[,c("longitude", "latitude")],
                                               data = BF_malaria_data[,c("examined", "positives")],
                                               proj4string = CRS("+init=epsg:4326"))


# To identify the admin 1 unit each point lies within you can use the 'over' function
over(BF_malaria_data_SPDF, BF_Adm_1)

# The CRS isn't identical, so over kicks out an error.
crs(BF_Adm_1)
crs(BF_malaria_data_SPDF)

# To transform projections you can use the spTransform function
BF_malaria_data_SPDF <- spTransform(BF_malaria_data_SPDF, crs(BF_Adm_1))
BF_malaria_data_SPDF

# Now try again
BF_Adm_1_per_point <- over(BF_malaria_data_SPDF, BF_Adm_1)

##Data Calculations
# Now we can use this to calculate admin unit specific statistics
# We might be interested in the number of sites per admin unit
# We could just create a table
table(BF_Adm_1_per_point$NAME_1)

#Or we can use the tapply function for more complex calculations
#Let's look at the number examined per admin unit
Nex_per_Adm1 <- tapply(BF_malaria_data_SPDF$examined, BF_Adm_1_per_point$NAME_1, sum)
# Now let's get the number of positives by admin unit
Npos_per_Adm1 <- tapply(BF_malaria_data_SPDF$positives, BF_Adm_1_per_point$NAME_1, sum)
#Calculate the prevalence
prev_per_Adm1 <- Npos_per_Adm1 / Nex_per_Adm1

#You can now merge these prevalence estimates
#back into your SPDF. First convert your prev_per_Adm1
#vector into a dataframe with an ID column
prev_per_Adm1_df <- data.frame(NAME_1 = names(prev_per_Adm1),
                               prevalence = prev_per_Adm1,
                               row.names=NULL)

BF_Adm_1 <- merge(BF_Adm_1, prev_per_Adm1_df,
                  by = "NAME_1")

head(BF_Adm_1)

#Mapping Calculated Values
# Now we can use this to make a map of prevalence
#Assign a color pallete to the quantiles
colorPal <- colorNumeric(wes_palette("Zissou1")[1:5], BF_Adm_1$prevalence)
#Plot
plot(BF_Adm_1, col=colorPal(BF_Adm_1$prevalence))

#...Or
leaflet() %>% addProviderTiles("CartoDB.Positron") %>% addPolygons(data=BF_Adm_1, 
                                                                   col=colorPal(BF_Adm_1$prevalence),
                                                                   fillOpacity=0.6) %>%
  addLegend(pal = colorPal, 
            values = BF_Adm_1$prevalence,
            title = "Prevalence")
#For practice, can you insert a different basemap using leaflet? Also, can you add a legend?

#You can also define your own color bins
colorPal <- colorBin(tim.colors(), BF_Adm_1$prevalence, bins = c(0, 0.25, 0.5, 0.75, 1))

#Plot
plot(BF_Adm_1, col=colorPal(BF_Adm_1$prevalence))
#Can you change the color bin breaks?

##Importing Raster Covariates
# Let's get some other layers

# Elevation
BF_elev <- raster::getData("alt", country="BF")
plot(BF_elev)

# Land use (# For information on land use classifications see http://due.esrin.esa.int/files/GLOBCOVER2009_Validation_Report_2.2.pdf)
BF_land_use <- raster("BF_land_use.tif")
BF_land_use

#Plot the land use raster
plot(BF_land_use)

# For a break down of the classes in BF aka how often each land use type occurs in BF
#(Note: this is just the number of pixels per land use type - NOT acres)
table(BF_land_use[]) 


##Resampling Raster Files
# Its good practice to resample rasters to the same extent and resolution (i.e. same grid)
# This makes it easier to deal with later and to relate rasters to each other
# The resample command makes this process easy
# The default method is bilinear interpolation, which doesn't make sense for our categorical
# variable, so we can use the nearest neighbour function 'ngb
BF_land_use_resampled <- resample(BF_land_use, BF_elev, method="ngb") 

# AH - why might they not intercect?? Hint: check the projections...
crs(BF_land_use) # Mercator
crs(BF_elev) # WGS84

# To reproject a raster, you can use the projectRaster function
BF_land_use <- projectRaster(BF_land_use, BF_elev, method="ngb")

# Now try resampling
BF_land_use_resampled <- resample(BF_land_use, BF_elev, method="ngb") 
BF_land_use_resampled;BF_elev

##Manipulating Raster Data
#1.You might want to change the resolution for analysis
#First, let's check the resolution
res(BF_elev) # in decimal degrees. 1 dd roughly 111km at the equator
#Let's change it
BF_elev_low_res <- aggregate(BF_elev, fact = 10) # by default, calculates mean
res(BF_elev_low_res)
plot(BF_elev_low_res)
# What does the fact parameter control? What happens if you change it?

#2.You can crop the raster to another spatial layer
BF_elev_Cascades <- crop(BF_elev, BF_Adm_1_Cascades)
plot(BF_elev_Cascades)
lines(BF_Adm_1_Cascades)

#3.You can change the values of tthe pixels
plot(BF_elev*3.28) # in feet
# to recategorize
plot(cut(BF_elev, 4))

#or create a new raster and change its values
BF_elev_cat <- BF_elev
BF_elev_cat[] <- ifelse(BF_elev[]>250,1,2)
plot(BF_elev_cat)

# If a raster is the same resolution and extent, you can perform joint operations on them, e.g. 
plot(BF_elev - BF_land_use_resampled) # Meaningless! Just for illustrative purposes..

##Extracting Data From Raster Files
# Now let's extract values of elevation at each survey point
# you can use the extract function from the raster package
extract(BF_elev, BF_malaria_data_SPDF)

# We can catch this as an object, or add it directly to our SPDF
BF_malaria_data_SPDF$elev <- extract(BF_elev, BF_malaria_data_SPDF)
BF_malaria_data_SPDF# now has 3 variables

#Spatial Data Analysis
# We can now have a quick look at the relationship between prevalence and elevation
# First generate a prevalence variable
BF_malaria_data_SPDF$prevalence <- BF_malaria_data_SPDF$positives / BF_malaria_data_SPDF$examined
plot(BF_malaria_data_SPDF$elev, BF_malaria_data_SPDF$prevalence)
# Can you change the x and y axis labels so they are cleaner?
library(stats)
scatter.smooth(BF_malaria_data_SPDF$elev, BF_malaria_data_SPDF$prevalence) # with lowes

# You can also extract values using polygons e.g to get admin 1 level elevations
# You just have to define a function to apply, otherwise you get all the pixel values per polygon
BF_Adm_1$elev <- extract(BF_elev, BF_Adm_1, fun=mean, na.rm=TRUE) # takes a little longer..


# You might also be interested in distances to/from other features (e.g. health facilities, water)
# e.g. distance to nearest river
# Get a rivers file (obtained via http://www.diva-gis.org/Data)
waterbodies <- readOGR("BFA_waterbody_shapefile", "BFA_water_areas_dcw")
waterbodies
# What is the projection of this shapefile?
plot(waterbodies)

# The goesphere package has some nice functions such as (takes a little while to compute)
dist_to_water <- dist2Line(BF_malaria_data_SPDF, waterbodies)

# Look at the result
dist_to_water

# Can add to your data frame
BF_malaria_data_SPDF$dist_to_water <- dist_to_water[,1]

# If the objects you are interested in calucating distance
# to are points as opposed to polygons/lines (as above)
# you first have to calculate the distance to every
# point and then identify the minimum
# For example, imagine waterbodies data was only available 
# as a point dataset (we can fake this by calculating the 
# centroid of each polygon)
waterbodies_points <- gCentroid(waterbodies, byid=TRUE)

# Now calucate a distance matrix showing distances
# between each observation and each waterbody point
dist_matrix <- distm(BF_malaria_data_SPDF, waterbodies_points)

# Then use the apply function to apply the 'minimum'
# function to each row (as each row represents the distance of 
# every waterbody point from our first observation)
BF_malaria_data_SPDF$dist_to_water_point <- apply(dist_matrix, 1, min)





