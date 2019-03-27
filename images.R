library(ochRe)
library(leaflet)
library(wesanderson)

# Get elevation for Rwanda
alt <- raster::getData('alt', country="RWA")

# Define color palette
col_pal <- colorNumeric(wes_palette("Zissou1", 64, type = "continuous")[1:64], values(alt))
  leaflet() %>% 
    addTiles() %>% 
      addRasterImage(alt, col = col_pal) %>% 
        addLegend(pal = col_pal, values = values(alt)) %>% 
    addScaleBar("topleft")

# Raster analysis  
LULC <- raster("https://www.dropbox.com/s/hc9m6ac3kb845ip/BF_land_use.tif?dl=1")
elev <- getData("alt", country="BFA")
adm1 <- raster::getData("GADM", country="BFA", level=1)
LULC <- projectRaster(LULC, crs=crs(adm1), method="ngb")
classes <- as.numeric(as.character(LULC[]))
plot(LULC, col = ochre_pal("emu_woman_paired")(14))
lines(adm1)
LULC_crop <- crop(LULC, adm1[4,])
plot(LULC_crop,col = ochre_pal("emu_woman_paired")(14))


# Clsuter analysis
leaflet() %>% 
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(lat=0.509227, lng=29.432802, radius = 50, col="red") %>% 
  addCircleMarkers(lat=0.51, lng=29.445, radius = 100, col="red")


