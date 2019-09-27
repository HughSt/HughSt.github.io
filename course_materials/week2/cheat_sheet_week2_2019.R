
## Cheat Sheet Week 2

# Pop quiz 1
ETH_Adm_1_not_amhara <- subset(ETH_Adm_1, ETH_Adm_1$NAME_1!="Amhara")
plot(ETH_Adm_1_not_amhara)

colors <- rep("orange", nrow(ETH_Adm_1))
colors[ETH_Adm_1$NAME_1=="Amhara"] <- "red"
leaflet() %>% addTiles() %>% addPolygons(data = ETH_Adm_1, col = colors)



## Pop quiz 2
# First define a color palette based on prevalence using the wesanderson color package
colorPal <- colorNumeric(topo.colors(64), ETH_Adm_1$prevalence)

# Plot with leaflet
leaflet() %>% addProviderTiles("CartoDB.Positron") %>% addPolygons(data=ETH_Adm_1, 
                                                                   col=colorPal(ETH_Adm_1$prevalence),
                                                                   fillOpacity=0.6) %>%
  addLegend(pal = colorPal, 
            values = ETH_Adm_1$prevalence,
            title = "Prevalence")


# Plotting only provinces with prevalence estimates
library(oro.nifti) # has the tim.colors palette
colorPal <- colorNumeric(tim.colors(64), ETH_Adm_1$prevalence)
ETH_Adm_1_with_data <- subset(ETH_Adm_1, !is.na(ETH_Adm_1$prevalence))
leaflet() %>% addProviderTiles("CartoDB.Positron") %>% addPolygons(data=ETH_Adm_1_with_data, 
                                                                   col=colorPal(ETH_Adm_1_with_data$prevalence),
                                                                   fillOpacity=0.6) %>%
  addLegend(pal = colorPal, 
            values = ETH_Adm_1_with_data$prevalence,
            title = "Prevalence")



# Distance to points using RANN
library(RANN)
library(geosphere)
nn <- nn2(waterbodies_points@coords,ETH_malaria_data_SPDF@coords, 
          k=1)
geo_dist <- distGeo(ETH_malaria_data_SPDF@coords,
        waterbodies_points@coords[nn$nn.idx,])

