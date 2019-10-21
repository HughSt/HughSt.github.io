Week 5 in class assignment cheat sheet
================

In class assignment 1

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

In class assignment
    2

    ### Many possible solutions, the point of the exercise being to have you struggle with accessing new data online/from R
    
    ## Cheating solution 1:
    # Administrative boundaries (level 0)
    LAO_Adm_0 <- raster::getData(name = "GADM",
                                 country = "LAO",
                                 level = 0)
    
    leaflet() %>% # Plot
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = LAO_Adm_0,
                  popup = LAO_Adm_0$NAME_0,
                  label = LAO_Adm_0$NAME_0,
                  fillOpacity = 0)

    ## Solution 2:
    library(maptools)
    # Path to OSM folder you just downloaded
    path_to_OSM_folder <- "...."
    
    LaosRoads <- rgdal::readOGR(dsn = paste0(path_to_OSM_folder,"/laos-latest-free"),
                                layer = "gis_osm_roads_free_1")
    LaosRoadsPST <- LaosRoads[which(LaosRoads$fclass == "trunk" |
                                    LaosRoads$fclass =="trunk_link" |
                                    LaosRoads$fclass == "primary" |
                                    LaosRoads$fclass == "primary_link" |
                                    LaosRoads$fclass == "secondary" |
                                    LaosRoads$fclass == "secondary_link" |
                                    LaosRoads$fclass == "road" |
                                    LaosRoads$fclass == "tertiary" |
                                    LaosRoads$fclass == "tertiary_link"),]
    
    factpal <- colorFactor(palette = topo.colors(length(levels(LaosRoadsPST$fclass))), domain = levels(LaosRoadsPST$fclass)) # define color palette
    
    leaflet() %>% # Plot
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = LAO_Adm_0,
                  popup = LAO_Adm_0$NAME_0,
                  label = LAO_Adm_0$NAME_0,
                  fillOpacity = 0,
                  color = "black") %>%
      addPolylines(data = LaosRoadsPST,
                   color = ~factpal(fclass),
                   popup = ~fclass,
                   label = ~fclass)

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week_5_cheat_sheet_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

    ## Loading required package: sp

    ## Checking rgeos availability: TRUE

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/sturrockh/Downloads/laos-latest-free", layer: "gis_osm_roads_free_1"
    ## with 70377 features
    ## It has 10 fields
    ## Integer64 fields read as strings:  layer

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week_5_cheat_sheet_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->
