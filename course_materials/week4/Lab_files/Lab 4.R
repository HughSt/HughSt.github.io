### Lab 4: Clustering

#Attach libraries for visualisation
install.packages("smacpod")
install.packages("car")
install.packages("SpatialEpi")
install.packages("ape")
install.packages("pgirmess")
library(spatstat)
library(geosphere)
library(rgeos)
library(leaflet)
library(car)
library(SpatialEpi)
library(geoR)
library(spdep) # Spatial Dependence: Weighting Schemes, Statistics and Models
library(ape) # Analyses of Phylogenetics and Evolution
library(pgirmess) # Data Analysis in Ecology
library(smacpod) # Spatial scanning statistic


setwd("")

# Global measures of clustering for point (prevalence etc.) data
# Open BF malaria data
BF_malaria_data <- read.csv("BF_malaria_data.csv",
                            header=T)

# Calc prevalence
BF_malaria_data$prevalence <- BF_malaria_data$positives / BF_malaria_data$examined

# Remind yourself of what data look like - do you see evidence of spatial clustering?
pal = colorNumeric("Oranges", BF_malaria_data$prevalence)

leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% 
  addCircleMarkers(BF_malaria_data$longitude, BF_malaria_data$latitude, fillOpacity =0.7,
  fillColor = pal(BF_malaria_data$prevalence), radius = BF_malaria_data$prevalence*10, weight=1) %>%
  addLegend(pal = pal, values=BF_malaria_data$prevalence)


# SAME AS
leaflet(BF_malaria_data)  %>% addProviderTiles("Stamen.TonerLite") %>% 
  addCircleMarkers(~longitude, ~latitude, fillOpacity=0.7,
   fillColor= ~pal(prevalence), radius=~prevalence*10, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~prevalence)


###########################################################################################################
# Part I - Global clustering
# Moran's i and correlograms: often used as a formal statistical test if there is presence of spatial
# autocorrelation across the study area
# Note that you can do this with multiple packages
###########################################################################################################

# Approach 1: Calculate moran's I using a distance based matrix

#Create a histogram of prevalence
hist(BF_malaria_data$prevalence)

# Use the logistic transformation (log odds) to make distribution more normal
# and compare to previous variogram results
BF_malaria_data$log_odds <- logit(BF_malaria_data$prevalence)

# Generate a distance matrix
BF.dists <- as.matrix(distm(cbind(BF_malaria_data$longitude, BF_malaria_data$latitude)))
dim(BF.dists) # 109 x 109 matrix of distance between all sets of points

# Take the inverse of the matrix values so that closer values have a larger weight and the farther
# values have a smaller weight
BF.dists.inv <- 1/BF.dists

# Replace the diagonal values with zero
diag(BF.dists.inv) <- 0

# Computes Moran's I autocorrelation coefficient of x given a matrix of weights (here based on distance) 
# Moran.I function is part of the "ape" package
Moran.I(BF_malaria_data$log_odds, BF.dists.inv)

# Moran's I statistic ranges from -1 to +1
# -1 is perfect dispersion, i.e. high values 
# are always next to low values and vice versa
# +1 is perfect clustering, i.e. high values are 
# always next to high values.

# Exepcted is calcualted as -1/(N-1) # In this case 
# N is 109 as we have 109 observations

# SD is the standard deviation of the Moran's I estimate

# p value is the probability we observed this result by chance

# What are the four values that this generates? What do each of them mean?

# Create a correlogram to explore moran's i over different distances
# "pgirmess" requires spdep (which also has correlogram options) 
# but is much simplier and user-friendly 


# Calculate the maximum distance between points
# (pgi.color is part of the "pgirmess" package)
# (coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins)
xy=cbind(BF_malaria_data$longitude, BF_malaria_data$latitude)
pgi.cor <- correlog(coords=xy, 
                    z=BF_malaria_data$log_odds, 
                    method="Moran", nbclass=10)
pgi.cor 
# (distclass is midpoint for the bin)

# Plot results
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

# Based on the correlogram, over what spatial lags are there evidence for spatial autocorrelation? 
#    Is this clustering positive or negative?

# Compare the correlogram to the results from the semivariogram in the last class (copied below)
BF_malaria_data_geo<-as.geodata(BF_malaria_data[,c("longitude","latitude","log_odds")])

# Generate and plot a binned variogram (10 bins) NB: have 
# made for full max distance (even though likely inaccurate) for comparison
Vario<-variog(BF_malaria_data_geo,max.dist=7.53,uvec=seq(0.4121237,7.1595572,l=10))
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)

## Calculate moran's I using a binary distance matrix

#Load data
load("Scotland.RData")

# Remind ourselves what this looks like
scot_col_pal <- colorBin(topo.colors(4), scotland$SMR, bins = c(0, 50, 100, 200, 700))
leaflet() %>% addProviderTiles("Stamen.Toner") %>% 
  addPolygons(data=scotland, col=scot_col_pal(scotland$SMR), weight=2, fillOpacity = 0.6) %>% 
  addLegend(labels = c("<50", "50-100", "100-200", ">200"), 
            pal = scot_col_pal, values=scotland$SMR, title = "SMR",
            "topleft")

# Create the neighborhood networks, i.e. identify 
# which counties share borders
ngh <- poly2nb(scotland, row.names=scotland$SP_ID)
ngh[]


# We can plot the neighbourhood network using the centroids
scotland_centroids <- coordinates(gCentroid(scotland, byid=T))
par(mfrow=c(1,1))
plot(scotland)
plot(ngh, scotland_centroids, add=T, col="red")

# Calc morans I
ngh_weights <-  nb2listw(ngh)
moran.test(scotland$SMR, listw=ngh_weights)



###########################################################################################################
#  Part II:  Local measures of clustering 

##########################################################################################################

# Open Namibia malaria case data
CaseControl<-read.csv("CaseControl.csv")

# And boundary file
NAM_Adm0<-raster::getData('GADM',country='NAM',level=0)

# Convert to a SPDF
CaseControl_SPDF <- SpatialPointsDataFrame(coords = CaseControl[,c("long", "lat")],
                                           data = CaseControl[,c("household_id", "case")])

# Let's plot and see what we have
case_color_scheme <- colorNumeric(c("blue", "red"), CaseControl_SPDF$case)
leaflet() %>% addProviderTiles("Stamen.Toner") %>%  
  addCircleMarkers(data=CaseControl_SPDF, 
                   color = case_color_scheme(CaseControl_SPDF$case),
                   weight = 2, radius=5)


# As part of the last lecture, you already generated kernel density estimates and 
# calculatred the ratio of the density estimate of cases:controls
# Now you will formally look for 'hotspots', i.e. places where the
# density of cases is greater than you would expect by chance
# Using kulldorf's scan statistic


# Convert CaseControl to a "PPP" object for spatial scan
CaseControlPPP <- ppp(CaseControl$long, CaseControl$lat, range(CaseControl$long), range(CaseControl$lat), marks = as.factor(CaseControl$case))

# Run Kulldorf's scan statistic

# This function is part of "smacpod" package
out <- spscan.test(CaseControlPPP, nsim = 999, case = 2, alpha = 0.05)
out

# Map results
case_color_scheme <- colorNumeric(c("blue", "red"), CaseControl_SPDF$case)
leaflet() %>% addProviderTiles("Stamen.Toner") %>% 
  addCircleMarkers(data=CaseControl_SPDF, 
                   color = case_color_scheme(CaseControl_SPDF$case), 
                   stroke = FALSE, radius=2, fillOpacity=1) %>% 
  addCircles(lng = out$clusters[[1]]$coords[,1], lat = out$clusters[[1]]$coords[,2], weight = 2,
             radius = out$clusters[[1]]$r*112*1000, color="grey") # Have to multiply by 112 to convert from dd to km and 1000 to m

# You can also use Kulldorf's scan statistic
# for prevalence data, using the kulldorf function
# from the SpatialEpi package
kulldorf_out <- kulldorff(BF_malaria_data[,c("longitude", "latitude")], 
                          cases = BF_malaria_data$positives, 
                          population =BF_malaria_data$examined, 
                          pop.upper.bound=0.5, 
                          n.simulat=999,
                          alpha.level=0.05,
                          plot=TRUE)

# Take a look
# kulldorf_out is a list. You can access elements of the list
# using the name of the element. e.g. to access info
# on the most likely cluster you can use the command
kulldorf_out$most.likely.cluster

# Plot
cluster <- kulldorf_out$most.likely.cluster$location.IDs.included

cluster_colors <- rep("blue",nrow(BF_malaria_data))
cluster_colors[cluster] <- "red"


leaflet(BF_malaria_data) %>% addProviderTiles("Stamen.TonerLite") %>%  
  addCircleMarkers(~longitude, ~latitude, 
    color = cluster_colors, radius=5, stroke=TRUE, weight=1) %>%
  addLegend(colors = c("red", "blue"), labels = c("Hotspot", "Not hotspot"))


# In addition, you can use Poisson data
# such as incidence data (cases per person time)

# As we are working with areal data, we use 
# centroids for Kulldorf. For Poisson data,
# we also need the expected numbers of cases
# assuming there was no variation in risk. These happen
# to be included in the scotland data, but if these are not
# available you can calculate manually or 
# use the 'exepcted' function which
# calculates the overall incidence rate and applies
# that to each area's population
overall_inc <- sum(scotland$COUNT) / sum(scotland$PY)
expected_cases <- scotland$PY * overall_inc # slightly different from those provided prob due to adjustment for covariates

# or using 'expected'
expected(scotland$PY, scotland$COUNT,  1)

# Now we can run Kulldorf
kulldorf_out_pois <- kulldorff(geo = scotland_centroids, 
                               cases = scotland$COUNT,
                               population = scotland$PY,
                               expected.cases = scotland$EXP,
                               pop.upper.bound=0.1, 
                               n.simulat=999,
                               alpha.level=0.05,
                               plot=TRUE)

# Take a look
kulldorf_out_pois$most.likely.cluster

# Plot
cluster_pois <- kulldorf_out_pois$most.likely.cluster$location.IDs.included


# Plot
leaflet() %>% addProviderTiles("Stamen.Toner") %>% 
  addPolygons(data=scotland, col="gray", weight=2, fillOpacity = 0.6) %>% 
  addPolygons(data=scotland[cluster_pois,],
              color = "red")




