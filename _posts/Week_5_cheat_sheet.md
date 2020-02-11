Week 5 Pop quizcheat sheet
================


``` r
# Administrative boundaries (level 0)
KEN_Adm_0 <- raster::getData(name = "GADM",
                             country = "KEN",
                             level = 0)

# Precipitation from worldclim at the 0.5 resolution (minutes of a degree)... South half of Kenya
KEN_prec_0.5_south <- raster::getData(name = "worldclim",
                                      var = "prec",
                                      res = 0.5,
                                      lon = 40,
                                      lat = -5)

# Restrict to January
KEN_prec_0.5_Jan_south <- KEN_prec_0.5_south[[1]]
ETH_prec_0.5_Jan <- ETH_prec_0.5[[1]]

# Merge north and south halves together
KEN_prec_0.5_Jan <- raster::merge(x = KEN_prec_0.5_Jan_south,
                                  y = ETH_prec_0.5_Jan)

# Crop to Kenya extent 
KEN_prec_0.5_Jan_Crop_Unmasked <- raster::crop(x = KEN_prec_0.5_Jan,
                                               y = KEN_Adm_0)

# Aggregate to 1 minute of a degree resolution
KEN_prec_1_Jan_Crop_Unmasked <- raster::aggregate(x = KEN_prec_0.5_Jan_Crop_Unmasked,
                                         fact = 2,
                                         fun = sum)

# Mask to Kenya
KEN_prec_1_Jan_Crop <- raster::mask(x = KEN_prec_1_Jan_Crop_Unmasked,
                                      mask = KEN_Adm_0)

# Change units and plot
KEN_prec_1_Jan_Crop_Inch <- KEN_prec_1_Jan_Crop * 0.0393701

raster_colorPal_prec_KEN <- colorNumeric(palette = topo.colors(64),
                                         domain = KEN_prec_1_Jan_Crop_Inch[],
                                         na.color = NA) # Define palette

leaflet() %>% # Plot
  addProviderTiles("CartoDB.Positron") %>%
  addRasterImage(x = KEN_prec_1_Jan_Crop_Inch,
                 color = raster_colorPal_prec_KEN) %>%
  addPolygons(data = KEN_Adm_0,
              popup = KEN_Adm_0$NAME_0,
              label = KEN_Adm_0$NAME_0,
              fillOpacity = 0,
              color = "red",
              weight = 3) %>%
  addLegend(title = "January precipitation (inches)<br>(1' res)",
            values = KEN_prec_1_Jan_Crop_Inch[],
            pal = raster_colorPal_prec_KEN)
```

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week_5_cheat_sheet_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

