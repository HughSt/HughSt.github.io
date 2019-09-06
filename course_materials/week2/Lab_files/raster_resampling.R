# Raster resampling

# let's load up the data we need
BF_elev <- raster::getData("alt", country="BF")
BF_land_use <- raster("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week2/Lab_files/BF_land_use.tif")
BF_land_use <- projectRaster(BF_land_use, crs=crs(BF_elev), method="ngb") #reproject
pop <- raster("https://www.dropbox.com/s/a9glj1is86o0xvz/BF_pop.tif?dl=1")

# Crop for demo purposes
BF_elev_crop <- crop(BF_elev, extent(BF_elev,499,507, 301,309))
pop_crop <- crop(pop, extent(BF_elev_crop))
BF_land_use_crop <- crop(BF_land_use, extent(BF_elev_crop))

# Create dummy lower res raster to resample to
new_raster <- aggregate(BF_elev_crop, fact=3)
new_raster <- shift(new_raster, x=0.004, y=0.002)
new_raster_outline <- rasterToPolygons(new_raster, dissolve=TRUE)

# Plot
plot(BF_elev_crop)

# plot cell outlines
lines(new_raster_outline)

# Now use resample 
BF_elev_crop_resampled_bilin<- resample(BF_elev_crop, new_raster, method="bilinear")
plot(BF_elev_crop_resampled_bilin)
lines(new_raster_outline)

# Using land class
plot(BF_land_use_crop)
BF_land_use_crop
table(BF_land_use_crop[])


# As this is categorial, and given that new
# raster is coarser (lower resolution), first
# aggregate using the mode
BF_land_use_crop_aggregated <- aggregate(BF_land_use_crop, fun='modal', fact = 9)
plot(BF_land_use_crop_aggregated)
lines(new_raster_outline)
BF_land_use_crop_aggregated_resamp <- resample(BF_land_use_crop_aggregated, new_raster, method = "ngb")
plot(BF_land_use_crop_aggregated_resamp)
lines(new_raster_outline)

# Using pop
plot(pop_crop, col=topo.colors(64))
cellStats(pop_crop, sum)

# As for the land use example,
# the new raster is at lower resolution
# Therefore have to aggregate first. 
# As we want total population to remain the same,
# aggregate by summing population
pop_crop_aggregated <- aggregate(pop_crop, fact = 3.012048, fun = sum)
cellStats(pop_crop_aggregated, sum)
plot(pop_crop_aggregated, col=topo.colors(64))
lines(new_raster_outline)

# Then use interpolation to move to new raster grid
pop_crop_aggregated_resamp <- resample(pop_crop_aggregated, new_raster)
plot(pop_crop_aggregated_resamp, col=topo.colors(64))
lines(new_raster_outline)
