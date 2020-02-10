# Week 3 pop quiz answers
library(raster)
library(geoR)
library(sp)

# Load data if not already in memory
ETH_malaria_data <- read.csv("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week1/Lab_files/Data/mal_data_eth_2009_no_dups.csv",header=T)
ETH_Adm_1 <- raster::getData("GADM", country="ETH", level=1)

### Q1
# How could you compare how well the best fitting IDW performs versus kriging? 
# Cross-validation error would be a good way to assess and compare interpolation methods 



### Q2
# Which appears to be more accurate?
# Lets compare MSE between cross-validated values obtained via IDW and kriging
# First perform IDW
oromia <- ETH_Adm_1[ETH_Adm_1$NAME_1=="Oromia",]
oromia_window <- owin(oromia@bbox[1,], oromia@bbox[2,])

#Then define a ppp of the prevalence data
ETH_malaria_data_ppp<-ppp(ETH_malaria_data$longitude,ETH_malaria_data$latitude,
                          marks=ETH_malaria_data$pf_pr,window=oromia_window)

# Get cross-validated values using the 'optimal' power
# as estimated in class
CV_idw_opt <- idw(ETH_malaria_data_ppp, power=1.4, at="points")

# Now Krig
# First fit variogram
ETH_malaria_data$pf_pr_adj <- ETH_malaria_data$pf_pr + 0.001
ETH_malaria_data$pf_pr_logit <- logit(ETH_malaria_data$pf_pr_adj)
ETH_malaria_data_geo_logit <- as.geodata(ETH_malaria_data[,c("longitude","latitude","pf_pr_logit")])

# Fit (spherical) variogram
Vario_logit <- variog(ETH_malaria_data_geo_logit, max.dist = MaxDist)
VarioMod_sph_logit <- variofit(Vario_logit, cov.model = "sph")

# Get CV kriged predictions
xvalid_result_logit <- xvalid(ETH_malaria_data_geo_logit, model = VarioMod_sph_logit)
xvalid_result_inv_logit <- inv.logit(xvalid_result_logit$predicted) 

# Now compare
mse(CV_idw_opt, ETH_malaria_data$pf_pr)
mse(xvalid_result_inv_logit, ETH_malaria_data$pf_pr)


### Q3
# Can you visualize where predictions from IDW differ to kriging?
# Here we can generate rasters of predictions from both methods and 
# visualize the difference
IDW <- idw(ETH_malaria_data_ppp, power=0.2, at="pixels")
pred_grid_x <- rep(IDW$xcol,length(IDW$yrow))
pred_grid_y <- sort(rep(IDW$yrow,length(IDW$xcol)))
pred_grid <- cbind(pred_grid_x,pred_grid_y)

# Now krig to those points
KrigPred_logit <- krige.conv(ETH_malaria_data_geo_logit, loc=pred_grid,
                       krige=krige.control(obj.model=VarioMod_sph_logit))

# Create a raster of inv.logit values
KrigPred_logit_raster <- rasterFromXYZ(data.frame(x=pred_grid_x,
                                            y=pred_grid_y,
                                            z=inv.logit(KrigPred_logit$predict)))
IDW_raster <- raster(IDW)
plot(IDW_raster - KrigPred_logit_raster)


### Q4
# Does inclusion of a trend surface improve kriging estimates?
# Fit (spherical) variogram
Vario_logit_trend <- variog(ETH_malaria_data_geo_logit, max.dist = MaxDist, trend="1st")
VarioMod_sph_logit_trend <- variofit(Vario_logit_trend, cov.model = "sph")

# Get CV kriged predictions
xvalid_result_trend_logit <- xvalid(ETH_malaria_data_geo_logit, model = VarioMod_sph_logit_trend)
xvalid_result_trend_inv_logit <- inv.logit(xvalid_result_trend_logit$predicted) 

# Now compare
mse(xvalid_result_inv_logit, ETH_malaria_data$pf_pr)  
mse(xvalid_result_trend_inv_logit, ETH_malaria_data$pf_pr)  
# Doesn't seem to improve things in this case




  