# Precipitation from worldclim at the 0.5 resolution (minutes of a degree)... Left half of South Sudan
SSD_prec_0.5_left <- raster::getData(name = "worldclim",
                                     var = "prec",
                                     res = 0.5,
                                     lon = 20,
                                     lat = 10)

# Restrict to October
SSD_prec_0.5_Oct_left <- SSD_prec_0.5_left[[10]]

# Merge left and right halves together
SSD_prec_0.5_Oct <- raster::merge(x = SSD_prec_0.5_Oct_left,
                                  y = ETH_prec_0.5_Oct)

# Crop to South Sudan extent 
SSD_prec_0.5_Oct_Crop_Unmasked <- raster::crop(x = SSD_prec_0.5_Oct,
                                               y = SSD_Adm_0)

# Mask to South Sudan and plot
SSD_prec_0.5_Oct_Crop <- raster::mask(x = SSD_prec_0.5_Oct_Crop_Unmasked,
                                      mask = SSD_Adm_0)

raster_colorPal_prec_SSD <- colorNumeric(palette = topo.colors(64),
                                         domain = values(SSD_prec_0.5_Oct_Crop),
                                         na.color = NA) # Define palette

leaflet() %>% # Plot
  addProviderTiles("CartoDB.Positron") %>%
  addRasterImage(x = SSD_prec_0.5_Oct_Crop,
                 color = raster_colorPal_prec_SSD) %>%
  addPolygons(data = SSD_Adm_0,
              popup = SSD_Adm_0$NAME_0,
              label = SSD_Adm_0$NAME_0,
              fillOpacity = 0,
              color = "red",
              weight = 3) %>%
  addLegend(title = "October precipitation (mm)<br>(0.5' res)",
            values = values(SSD_prec_0.5_Oct_Crop),
            pal = raster_colorPal_prec_SSD)