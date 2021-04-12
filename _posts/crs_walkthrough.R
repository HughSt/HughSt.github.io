
library(sp)
library(raster)

# Create some random points
points <- data.frame(x = c(1:10),
                     y = c(1:10))

points_sp <- SpatialPoints(coords = points)
plot(points_sp)

# We haven't told R what the CRS is. You can check to see 
proj4string(points_sp)

# Let's set the CRS to WGS84 (EPSG: 4326)
# The EPSG codes provide a quick reference to 
# a CRS
proj4string(points_sp) <- CRS("+init=epsg:4326")

# If you want to reproject from one CRS to another,
# use the spTransform function. 
spTransform(points_sp, CRS("+init=epsg:4238"))

# Many make the mistake of 'setting' the CRS to 
# a different CRS instead of transforming it. 
# For example, if you ran the code below, you 
# would set the CRS to ESPG:4238 without changing
# any of the coordinate values. 
# proj4string(points_sp) <- CRS("+init=epsg:4238")

# If you have one Spatial object with a CRS and want to 
# transform another to have the same CRS, you can point
# to that object when you run spTransform. e.g. imagine
# we want to use the CRS of a Spatial object called reference_object
spTransform(points_sp, proj4string(reference_object))

# For rasters, the same logic applies, however, the 
# functions are slightly different. Let's get the land use
# raster from week 2
ETH_land_use <- raster("https://github.com/HughSt/HughSt.github.io/blob/master/course_materials/week2/Lab_files/ETH_land_use.tif?raw=true")
ETH_land_use

# you can get/set the CRS using the crs function.
# This raster already has a CRS set
crs(ETH_land_use)

# To transform, you can use the projectRaster function. In this case,
# our data are categorical, so we need to tell R to use nearest neighbor
# technique if the reprojection involves any resampling. The default is
# bilinear interpolation which is fine for most continuous outcomes (e.g. elevation)
ETH_land_use_4238 <- projectRaster(ETH_land_use, crs=CRS("+init=epsg:4238"),
                                   method = "ngb")
ETH_land_use_4238

# As for Spatial objects, you can also point to another raster object to 
# use as a reference. i.e. to project back to the original CRS
projectRaster(ETH_land_use_4238, ETH_land_use)

