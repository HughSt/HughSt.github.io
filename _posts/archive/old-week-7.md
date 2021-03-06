---
layout: post
title: Week 7 - Spatial regression of areal data
featured-img: unemployment_USA_2016
---

Aim
---

To discuss methods for spatial regression analysis using areal data

Content
-------

This post provides an introduction to methods for conducting spatial regression analysis of areal, or polygon, data.

Datasets
--------

-   New York leukemia data
-   Scottish lip cancer data

First we will attach the libraries used. Install those you don't already have the standard way; for INLA, you'll need the following code:


{% highlight r %}
install.packages("INLA", repos=c(getOption("repos"), INLA="<https://inla.r-inla-download.org/R/stable>"), dep=TRUE)
library(rgdal)
library(foreign)
library(spdep)
library(ggplot2)
library(RColorBrewer)
library(sp)
library(maptools)
library(coda)
library(ggmap)
library(gridExtra)  #plotting multiple plots in one
library(spatialreg)  #spatial regression functions
library(CARBayes)  #Bayesian CAR models
library(INLA)  #Approximate Bayesian models
{% endhighlight %}

First we're going to load in the leukemia data from New York and explore the data a bit. We'll load in the shape files for New York census tracks and the locations of hazardous waste sites as potential exposures.

{% highlight r %}
NY8<-readOGR("/Users/abennett1/Documents/Teaching/NY_data","NY8_utm18")
{% endhighlight %}

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/abennett1/Documents/Teaching/NY_data", layer: "NY8_utm18"
    ## with 281 features
    ## It has 17 fields

{% highlight r %}
TCE<-readOGR("/Users/abennett1/Documents/Teaching/NY_data","TCE")
{% endhighlight %}

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/abennett1/Documents/Teaching/NY_data", layer: "TCE"
    ## with 11 features
    ## It has 5 fields

{% highlight r %}
cities<-readOGR("/Users/abennett1/Documents/Teaching/NY_data","NY8cities")
{% endhighlight %}

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/abennett1/Documents/Teaching/NY_data", layer: "NY8cities"
    ## with 6 features
    ## It has 1 fields

View the data - what variables are included?

{% highlight r %}
head(NY8@data)
{% endhighlight %}

    ##          AREANAME     AREAKEY        X        Y POP8 TRACTCAS  PROPCAS
    ## 0 Binghamton city 36007000100 4.069397 -67.3533 3540     3.08 0.000870
    ## 1 Binghamton city 36007000200 4.639371 -66.8619 3560     4.08 0.001146
    ## 2 Binghamton city 36007000300 5.709063 -66.9775 3739     1.09 0.000292
    ## 3 Binghamton city 36007000400 7.613831 -65.9958 2784     1.07 0.000384
    ## 4 Binghamton city 36007000500 7.315968 -67.3183 2571     3.06 0.001190
    ## 5 Binghamton city 36007000600 8.558753 -66.9344 2729     1.06 0.000388
    ##   PCTOWNHOME PCTAGE65P        Z  AVGIDIST PEXPOSURE   Cases       Xm
    ## 0  0.3277311 0.1466102  0.14197 0.2373852  3.167099 3.08284 4069.397
    ## 1  0.4268293 0.2351124  0.35555 0.2087413  3.038511 4.08331 4639.371
    ## 2  0.3377396 0.1380048 -0.58165 0.1708548  2.838229 1.08750 5709.063
    ## 3  0.4616048 0.1188937 -0.29634 0.1406045  2.643366 1.06515 7613.831
    ## 4  0.1924370 0.1415791  0.45689 0.1577753  2.758587 3.06017 7315.968
    ## 5  0.3651786 0.1410773 -0.28123 0.1726033  2.848411 1.06386 8558.753
    ##         Ym   Xshift  Yshift
    ## 0 -67353.3 423391.0 4661502
    ## 1 -66861.9 423961.0 4661993
    ## 2 -66977.5 425030.6 4661878
    ## 3 -65995.8 426935.4 4662859
    ## 4 -67318.3 426637.5 4661537
    ## 5 -66934.4 427880.3 4661921

You can find information on this R dataset here.

{% highlight r %}
?NY_data
{% endhighlight %}

Plot the census tracks and hazardous waste sites.

{% highlight r %}
plot(NY8)
plot(TCE,pch=16,cex=2,col="red",add=T)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-5-1.png)

Set the neighbor matrices. This is a review from Week 4.

{% highlight r %}
# Contiguity neighbors - all that share a boundary point
NY8_nb <- poly2nb(NY8)  #queen contiguity

NY8_nbr <- poly2nb(NY8,queen=F)  #rook contiguity

#coordinates
coords<-coordinates(NY8)

#view the neighbors
par(mfrow=c(1,2))
plot(NY8)
plot(NY8_nb,coords,col="blue",add=T)
plot(NY8)
plot(NY8_nbr,coords,col="green",add=T)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-6-1.png)

{% highlight r %}
#highlight the different neighbors
par(mfrow=c(1,1))
plot(NY8)
plot(NY8_nb,coords,col="red",add=T)
plot(NY8_nbr,coords,add=T)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-6-2.png)

{% highlight r %}
# Distance-based neighbors - returns the k nearest points as neighbors
IDs<-row.names(as(NY8, "data.frame"))
NY8_nb_Dist <- knn2nb(knearneigh(coords, k=1),row.names=IDs)
NY8_nb_Dist2<- knn2nb(knearneigh(coords, k=2),row.names=IDs)
NY8_nb_Dist3<- knn2nb(knearneigh(coords, k=3),row.names=IDs)

par(mfrow=c(1,3))
plot(NY8)
plot(NY8_nb_Dist, coords,col="green",add=T)
plot(NY8)
plot(NY8_nb_Dist2, coords,col="green",add=T)
plot(NY8)
plot(NY8_nb_Dist3, coords,col="green",add=T)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-6-3.png)

{% highlight r %}
# Find the distance between points
dsts<-unlist(nbdists(NY8_nb_Dist, coords))
summary(dsts)
{% endhighlight %}

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    82.7   912.8  1801.5  3444.8  4461.0 17033.6

{% highlight r %}
#get the max (or min) distance
max_1nn<-max(dsts)
max_1nn
{% endhighlight %}

    ## [1] 17033.62

{% highlight r %}
# use dnearneigh to find neighbors with an interpoint distance, with distances settings
# upper and lower distance bounds
NY8_nb0.5 <- dnearneigh(coords, d1=0, d2=0.5*max_1nn, row.names=IDs)
NY8_nb1 <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=IDs)
NY8_nb1.5 <- dnearneigh(coords, d1=0, d2=1.5*max_1nn, row.names=IDs)

#par(mfrow=c(1,3))
#plot(NY8)
#plot(NY8_nb0.5, coords, col="green",add=T)
#plot(NY8)
#plot(NY8_nb1, coords, col="green",add=T)
#plot(NY8)
#plot(NY8_nb1.5, coords, col="green",add=T)
{% endhighlight %}

Set the weights for the neighbor matrix. The default is row standardized.

{% highlight r %}
##set weights - contiguity
#weights style W - row standardized
NY8_w<-nb2listw(NY8_nb)
NY8_w
{% endhighlight %}

    ## Characteristics of weights list object:
    ## Neighbour list object:
    ## Number of regions: 281 
    ## Number of nonzero links: 1624 
    ## Percentage nonzero weights: 2.056712 
    ## Average number of links: 5.779359 
    ## 
    ## Weights style: W 
    ## Weights constants summary:
    ##     n    nn  S0       S1       S2
    ## W 281 78961 281 106.6125 1164.157

{% highlight r %}
#weights style B - binary
NY8_wB<-nb2listw(NY8_nb,style="B")
NY8_wB
{% endhighlight %}

    ## Characteristics of weights list object:
    ## Neighbour list object:
    ## Number of regions: 281 
    ## Number of nonzero links: 1624 
    ## Percentage nonzero weights: 2.056712 
    ## Average number of links: 5.779359 
    ## 
    ## Weights style: B 
    ## Weights constants summary:
    ##     n    nn   S0   S1    S2
    ## B 281 78961 1624 3248 41440

{% highlight r %}
##set weights - distance: add zero polic=T if there is a 'no neighbor' error
NY8_dist_w<-nb2listw(NY8_nb1)
NY8_dist_w
{% endhighlight %}

    ## Characteristics of weights list object:
    ## Neighbour list object:
    ## Number of regions: 281 
    ## Number of nonzero links: 17972 
    ## Percentage nonzero weights: 22.7606 
    ## Average number of links: 63.9573 
    ## 
    ## Weights style: W 
    ## Weights constants summary:
    ##     n    nn  S0       S1      S2
    ## W 281 78961 281 34.47232 1136.13

Now we can do an initial check of spatial autocorrelation in the data. Are there any issues with this approach at this point?

{% highlight r %}
##moran's tests of global spatial autocorrelation
moran.test(NY8$Cases,listw=NY8_w)  #using row standardized
{% endhighlight %}

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  NY8$Cases  
    ## weights: NY8_w    
    ## 
    ## Moran I statistic standard deviate = 4.3928, p-value = 5.594e-06
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.156019642      -0.003571429       0.001319861

{% highlight r %}
moran.test(NY8$Cases,listw=NY8_dist_w)  #using row standardized with distance-based neighbors
{% endhighlight %}

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  NY8$Cases  
    ## weights: NY8_dist_w    
    ## 
    ## Moran I statistic standard deviate = 6.528, p-value = 3.334e-11
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      0.1285524699     -0.0035714286      0.0004096452

{% highlight r %}
#can also use moran.mc or lm.morantest
moran.mc(NY8$Cases,listw=NY8_w,nsim=1000)
{% endhighlight %}

    ## 
    ##  Monte-Carlo simulation of Moran I
    ## 
    ## data:  NY8$Cases 
    ## weights: NY8_w  
    ## number of simulations + 1: 1001 
    ## 
    ## statistic = 0.15602, observed rank = 1001, p-value = 0.000999
    ## alternative hypothesis: greater

We can transform the case outcome to a linear one - why might we want to do this?

{% highlight r %}
NY8$Zi<-log((1000*(NY8$Cases+1))/NY8$POP8)
{% endhighlight %}

Plotting the transformed data against the original case data.

{% highlight r %}
p1<-spplot(NY8,"Cases",main=list(label="Cases"))
p2<-spplot(NY8,"Zi",main=list(label="log incidence"))

grid.arrange(p1,p2,nrow=1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-10-1.png)

{% highlight r %}
#we check spatial autocorrelation of the transformed values
moran.test(NY8$Zi, listw=NY8_w)
{% endhighlight %}

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  NY8$Zi  
    ## weights: NY8_w    
    ## 
    ## Moran I statistic standard deviate = 5.5701, p-value = 1.273e-08
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.196324410      -0.003571429       0.001287912

{% highlight r %}
#we can use a linear model formulation and test the residuals
lm.morantest(lm(Zi~1,data=NY8),listw=NY8_w)
{% endhighlight %}

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = Zi ~ 1, data = NY8)
    ## weights: NY8_w
    ## 
    ## Moran I statistic standard deviate = 5.4958, p-value = 1.945e-08
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##      0.196324410     -0.003571429      0.001322980

{% highlight r %}
#adjust for population size - different population sizes can create appearance of spatial autocorrelation 
lm_Zi<-lm(Zi~1,data=NY8,weights=POP8)
lm.morantest(lm_Zi,listw=NY8_w)
{% endhighlight %}

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = Zi ~ 1, data = NY8, weights = POP8)
    ## weights: NY8_w
    ## 
    ## Moran I statistic standard deviate = 3.6678, p-value = 0.0001223
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##      0.129999227     -0.003404529      0.001322863

{% highlight r %}
##what if we now add some covariates- which factors look to be associated with log incidence?
nylm<-lm(Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME,data=NY8)
summary(nylm)
{% endhighlight %}

    ## 
    ## Call:
    ## lm(formula = Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7390 -0.3979 -0.0302  0.3350  4.1388 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.51786    0.15846  -3.268  0.00122 ** 
    ## PEXPOSURE    0.04876    0.03504   1.391  0.16522    
    ## PCTAGE65P    3.95589    0.60514   6.537 3.01e-10 ***
    ## PCTOWNHOME  -0.55966    0.17021  -3.288  0.00114 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6568 on 277 degrees of freedom
    ## Multiple R-squared:  0.1936, Adjusted R-squared:  0.1848 
    ## F-statistic: 22.16 on 3 and 277 DF,  p-value: 6.83e-13

{% highlight r %}
#we can test the residuals of the linear model with covariates - what do you find?  Did adding covariates 
#affect residual autocorrelation?
lm.morantest(nylm,NY8_w)
{% endhighlight %}

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data
    ## = NY8)
    ## weights: NY8_w
    ## 
    ## Moran I statistic standard deviate = 2.4335, p-value = 0.007477
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##      0.077688367     -0.009757607      0.001291261

{% highlight r %}
#we can add the fitted and residuals to the dataset for plotting
NY8$fitted<-predict(nylm)
NY8$lmresid<-residuals(nylm)

p1<-spplot(NY8,"fitted",main=list(label="fitted values"))
p2<-spplot(NY8,"lmresid",main=list(label="residuals"))
grid.arrange(p1,p2,nrow=1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-10-2.png)

{% highlight r %}
moran.test(NY8$lmresid,listw=NY8_w)
{% endhighlight %}

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  NY8$lmresid  
    ## weights: NY8_w    
    ## 
    ## Moran I statistic standard deviate = 2.2578, p-value = 0.01198
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.077688367      -0.003571429       0.001295368

{% highlight r %}
#what if we adjust for population size - does this change the interpretation?
nylm_w<-lm(Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME,data=NY8,weights=POP8)
summary(nylm_w)
{% endhighlight %}

    ## 
    ## Call:
    ## lm(formula = Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
    ##     weights = POP8)
    ## 
    ## Weighted Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -129.048  -14.706    5.775   25.632   70.765 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.77893    0.14111  -5.520 7.79e-08 ***
    ## PEXPOSURE    0.07619    0.02730   2.791  0.00563 ** 
    ## PCTAGE65P    3.86070    0.57105   6.761 8.11e-11 ***
    ## PCTOWNHOME  -0.39833    0.15299  -2.604  0.00972 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 33.48 on 277 degrees of freedom
    ## Multiple R-squared:  0.198,  Adjusted R-squared:  0.1893 
    ## F-statistic: 22.79 on 3 and 277 DF,  p-value: 3.223e-13

{% highlight r %}
lm.morantest(nylm_w,NY8_w)
{% endhighlight %}

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data
    ## = NY8, weights = POP8)
    ## weights: NY8_w
    ## 
    ## Moran I statistic standard deviate = 0.69922, p-value = 0.2422
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##      0.015899696     -0.009228793      0.001291516

Now let's try a simple SAR model.

{% highlight r %}
nysar<-spautolm(Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME,data=NY8,listw=NY8_w)

summary(nysar) #Check the results - Lambda is the spatial coefficient;  AIC gives you model fit
{% endhighlight %}

    ## 
    ## Call: 
    ## spautolm(formula = Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
    ##     listw = NY8_w)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.61754 -0.38762 -0.02746  0.33549  4.05588 
    ## 
    ## Coefficients: 
    ##              Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept) -0.580997   0.174689 -3.3259 0.0008814
    ## PEXPOSURE    0.058304   0.042251  1.3799 0.1676080
    ## PCTAGE65P    3.797752   0.621666  6.1090 1.003e-09
    ## PCTOWNHOME  -0.440550   0.188766 -2.3338 0.0196038
    ## 
    ## Lambda: 0.21501 LR test value: 4.8017 p-value: 0.028432 
    ## Numerical Hessian standard error of lambda: 0.095591 
    ## 
    ## Log likelihood: -276.1629 
    ## ML residual variance (sigma squared): 0.41446, (sigma: 0.64379)
    ## Number of observations: 281 
    ## Number of parameters estimated: 6 
    ## AIC: 564.33

{% highlight r %}
#let's try with distance neighborhood matrix
nysard<-spautolm(Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME,data=NY8,listw=NY8_dist_w)
summary(nysard)
{% endhighlight %}

    ## 
    ## Call: 
    ## spautolm(formula = Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
    ##     listw = NY8_dist_w)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -1.721582 -0.403469 -0.031235  0.347258  4.142989 
    ## 
    ## Coefficients: 
    ##              Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept) -0.499031   0.148616 -3.3579 0.0007855
    ## PEXPOSURE    0.048979   0.028964  1.6911 0.0908268
    ## PCTAGE65P    3.893238   0.587841  6.6229 3.521e-11
    ## PCTOWNHOME  -0.576871   0.166534 -3.4640 0.0005322
    ## 
    ## Lambda: -0.24469 LR test value: 1.5458 p-value: 0.21375 
    ## Numerical Hessian standard error of lambda: 0.19685 
    ## 
    ## Log likelihood: -277.7908 
    ## ML residual variance (sigma squared): 0.42156, (sigma: 0.64927)
    ## Number of observations: 281 
    ## Number of parameters estimated: 6 
    ## AIC: 567.58

{% highlight r %}
#with population weights
nysarw<-spautolm(Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME,data=NY8,listw=NY8_w,weights=POP8)
summary(nysarw)
{% endhighlight %}

    ## 
    ## Call: 
    ## spautolm(formula = Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
    ##     listw = NY8_w, weights = POP8)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -1.478443 -0.269993  0.095428  0.466786  4.292926 
    ## 
    ## Coefficients: 
    ##              Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept) -0.795039   0.144674 -5.4954 3.899e-08
    ## PEXPOSURE    0.078805   0.028713  2.7446  0.006059
    ## PCTAGE65P    3.804453   0.577488  6.5879 4.460e-11
    ## PCTOWNHOME  -0.377317   0.157287 -2.3989  0.016444
    ## 
    ## Lambda: 0.066954 LR test value: 0.49949 p-value: 0.47973 
    ## Numerical Hessian standard error of lambda: 0.093899 
    ## 
    ## Log likelihood: -251.409 
    ## ML residual variance (sigma squared): 1102.3, (sigma: 33.201)
    ## Number of observations: 281 
    ## Number of parameters estimated: 6 
    ## AIC: 514.82

Let's use the same code to run a basic CAR model. Do you see any differences? What do you conclude about the covariate associations?

{% highlight r %}
nycar<-spautolm(Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME,data=NY8,listw=NY8_wB,family="CAR") # for CAR we have to use symmetric weights (B)
summary(nycar)
{% endhighlight %}

    ## 
    ## Call: 
    ## spautolm(formula = Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
    ##     listw = NY8_wB, family = "CAR")
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.52481 -0.36904 -0.02431  0.33211  3.85416 
    ## 
    ## Coefficients: 
    ##              Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept) -0.617512   0.177785 -3.4734  0.000514
    ## PEXPOSURE    0.068991   0.042865  1.6095  0.107504
    ## PCTAGE65P    3.722932   0.625580  5.9512 2.662e-09
    ## PCTOWNHOME  -0.403233   0.193286 -2.0862  0.036961
    ## 
    ## Lambda: 0.073219 LR test value: 4.6101 p-value: 0.031785 
    ## Numerical Hessian standard error of lambda: 0.030703 
    ## 
    ## Log likelihood: -276.2587 
    ## ML residual variance (sigma squared): 0.41079, (sigma: 0.64093)
    ## Number of observations: 281 
    ## Number of parameters estimated: 6 
    ## AIC: 564.52

{% highlight r %}
nycarw<-spautolm(Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME,data=NY8,listw=NY8_wB,weights=POP8,family="CAR")
summary(nycarw)
{% endhighlight %}

    ## 
    ## Call: 
    ## spautolm(formula = Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
    ##     listw = NY8_wB, weights = POP8, family = "CAR")
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -1.497248 -0.275703  0.089067  0.452301  4.254415 
    ## 
    ## Coefficients: 
    ##              Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept) -0.784482   0.142660 -5.4990 3.820e-08
    ## PEXPOSURE    0.078899   0.027934  2.8245  0.004736
    ## PCTAGE65P    3.844877   0.573024  6.7098 1.949e-11
    ## PCTOWNHOME  -0.391774   0.155021 -2.5272  0.011496
    ## 
    ## Lambda: 0.012072 LR test value: 0.12325 p-value: 0.72553 
    ## Numerical Hessian standard error of lambda: 0.03651 
    ## 
    ## Log likelihood: -251.5972 
    ## ML residual variance (sigma squared): 1104.2, (sigma: 33.229)
    ## Number of observations: 281 
    ## Number of parameters estimated: 6 
    ## AIC: 515.19

We can also try a mixed effects model using the package nlme. How does the non-spatial model compare to the spatial model?

{% highlight r %}
library(nlme)
NY8$x<-coordinates(NY8)[,1]/1000
NY8$y<-coordinates(NY8)[,2]/1000
sp1<-corSpatial(1,form=~x+y,type="gaussian")
scor<-Initialize(sp1,as(NY8,"data.frame")[,c("x","y")],nugget=FALSE)

spmodel.ns<-lme(Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME,random=~1|AREAKEY,data=as(NY8,"data.frame"),
             method="ML")
spmodel.s<-lme(Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME,random=~1|AREAKEY,data=as(NY8,"data.frame"),
             correlation=scor,method="ML")
summary(spmodel.ns)
{% endhighlight %}

    ## Linear mixed-effects model fit by maximum likelihood
    ##  Data: as(NY8, "data.frame") 
    ##        AIC      BIC    logLik
    ##   569.1275 590.9576 -278.5637
    ## 
    ## Random effects:
    ##  Formula: ~1 | AREAKEY
    ##         (Intercept)  Residual
    ## StdDev:    0.610549 0.2289559
    ## 
    ## Fixed effects: Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME 
    ##                 Value Std.Error  DF   t-value p-value
    ## (Intercept) -0.517864 0.1584641 277 -3.268024  0.0012
    ## PEXPOSURE    0.048759 0.0350429 277  1.391407  0.1652
    ## PCTAGE65P    3.955893 0.6051428 277  6.537124  0.0000
    ## PCTOWNHOME  -0.559665 0.1702080 277 -3.288121  0.0011
    ##  Correlation: 
    ##            (Intr) PEXPOS PCTAGE
    ## PEXPOSURE  -0.411              
    ## PCTAGE65P  -0.587 -0.075       
    ## PCTOWNHOME -0.741  0.082  0.147
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -0.93641568 -0.21427411 -0.01625644  0.18039800  2.22863961 
    ## 
    ## Number of Observations: 281
    ## Number of Groups: 281

{% highlight r %}
summary(spmodel.s)
{% endhighlight %}

    ## Linear mixed-effects model fit by maximum likelihood
    ##  Data: as(NY8, "data.frame") 
    ##        AIC     BIC    logLik
    ##   571.1275 596.596 -278.5637
    ## 
    ## Random effects:
    ##  Formula: ~1 | AREAKEY
    ##         (Intercept)   Residual
    ## StdDev:   0.6503902 0.04672743
    ## 
    ## Correlation Structure: Gaussian spatial correlation
    ##  Formula: ~x + y | AREAKEY 
    ##  Parameter estimate(s):
    ##      range 
    ## 0.01929072 
    ## Fixed effects: Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME 
    ##                 Value Std.Error  DF   t-value p-value
    ## (Intercept) -0.517864 0.1584641 277 -3.268024  0.0012
    ## PEXPOSURE    0.048759 0.0350429 277  1.391407  0.1652
    ## PCTAGE65P    3.955893 0.6051428 277  6.537124  0.0000
    ## PCTOWNHOME  -0.559665 0.1702080 277 -3.288121  0.0011
    ##  Correlation: 
    ##            (Intr) PEXPOS PCTAGE
    ## PEXPOSURE  -0.411              
    ## PCTAGE65P  -0.587 -0.075       
    ## PCTOWNHOME -0.741  0.082  0.147
    ## 
    ## Standardized Within-Group Residuals:
    ##          Min           Q1          Med           Q3          Max 
    ## -0.191112371 -0.043731041 -0.003317764  0.036817291  0.454841380 
    ## 
    ## Number of Observations: 281
    ## Number of Groups: 281

For disease mapping, we often want to understand things in terms of relative risks. This means we need to model and map the observed against what would be expected if risks/rates were constant. First we create the number of expected cases per area assuming a constant rate, then calculate the area-specific relative risk (similar to a standardized mortality ratio or SMR). We can then plot these using spplot (sp) or ggplot.

{% highlight r %}
NY8$EXP<-NY8$POP8*(sum(NY8$Cases)/sum(NY8$POP8)) #create expected

NY8$SMR<-NY8$Cases/NY8$EXP #create "SMR"

spplot(NY8, "SMR",main=list(label="SMR")) #use spplot in the sp package to plot SMR by area
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-14-1.png)

Using ggplot.

{% highlight r %}
require('plyr') ##to plot areal data using ggplot - we have to use 'fortify' for a spatial polygons data frame
#library(ggmap)
NY8@data$id<-rownames(NY8@data)
NY8.points<-fortify(NY8,region="id")
NY8.df<-join(NY8.points,NY8@data,by="id")
{% endhighlight %}

{% highlight r %}
ggplot(NY8.df)+aes(long,lat,group=group,fill=SMR)+geom_polygon()+scale_fill_continuous("Relative risk")+coord_fixed(1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-16-1.png)

Now let's use the CARBayes package to run some Bayesian models.

{% highlight r %}
NY8$Cases2<-round(NY8$Cases)
W.mat<-nb2mat(NY8_nb,style="B")

#linear model
form1<-Zi~PEXPOSURE+PCTAGE65P+PCTOWNHOME
model.spatial<-S.CARleroux(formula=form1,family="gaussian",data=NY8@data,W=W.mat,burnin=2000,n.sample=10000,thin=10)
{% endhighlight %}

Let's look at the output. How does this compare?

{% highlight r %}
model.spatial
{% endhighlight %}

    ## 
    ## #################
    ## #### Model fitted
    ## #################
    ## Likelihood model - Gaussian (identity link function) 
    ## Random effects model - Leroux CAR
    ## Regression equation - Zi ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME
    ## Number of missing observations - 0
    ## 
    ## ############
    ## #### Results
    ## ############
    ## Posterior quantities and DIC
    ## 
    ##              Median    2.5%   97.5% n.sample % accept n.effective
    ## (Intercept) -0.5217 -0.8483 -0.2208      800    100.0       697.8
    ## PEXPOSURE    0.0490 -0.0224  0.1163      800    100.0       718.8
    ## PCTAGE65P    3.9201  2.7925  5.0483      800    100.0       669.4
    ## PCTOWNHOME  -0.5475 -0.9006 -0.2174      800    100.0       800.0
    ## nu2          0.4200  0.2761  0.4985      800    100.0        55.5
    ## tau2         0.0103  0.0024  0.2137      800    100.0        23.8
    ## rho          0.3262  0.0154  0.8965      800     50.5        44.9
    ##             Geweke.diag
    ## (Intercept)        -0.3
    ## PEXPOSURE           1.5
    ## PCTAGE65P           1.4
    ## PCTOWNHOME         -0.9
    ## nu2                -0.9
    ## tau2               -1.5
    ## rho                 0.1
    ## 
    ## DIC =  563.5231       p.d =  13.98142       LMPL =  -285.52

{% highlight r %}
plot(model.spatial$samples$beta[,2])
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-18-1.png)

{% highlight r %}
plot(model.spatial$samples$beta[,3])
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-18-2.png)

Let's take a look at the fitted versus the raw values.

{% highlight r %}
NY8$fitted1<-model.spatial$fitted.values #get the fitted values

p1<-spplot(NY8,"Zi",main=list(label="log incidence"))
p2<-spplot(NY8,"fitted1",main=list(label="fitted values"))

grid.arrange(p1,p2,nrow=1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-19-1.png)

Now let's run a Poisson model. This one is the 'BYM' form that includes spatial and non-spatial effects.

{% highlight r %}
form2<-Cases2~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(EXP))

model.spatial<-S.CARbym(formula=form2,family="poisson",data=NY8@data,W=W.mat,burnin=2000,n.sample=10000,thin=10)

NY8$fitted2<-model.spatial$fitted.values
NY8$RR<-exp(log(NY8$fitted2)-log(NY8$EXP))
{% endhighlight %}

{% highlight r %}
model.spatial
{% endhighlight %}

    ## 
    ## #################
    ## #### Model fitted
    ## #################
    ## Likelihood model - Poisson (log link function) 
    ## Random effects model - BYM CAR
    ## Regression equation - Cases2 ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME + offset(log(EXP))
    ## Number of missing observations - 0
    ## 
    ## ############
    ## #### Results
    ## ############
    ## Posterior quantities and DIC
    ## 
    ##              Median    2.5%   97.5% n.sample % accept n.effective
    ## (Intercept) -0.6789 -1.1109 -0.2602      800     53.2       561.3
    ## PEXPOSURE    0.1478  0.0636  0.2265      800     53.2       202.3
    ## PCTAGE65P    4.0263  2.6217  5.2799      800     53.2       800.0
    ## PCTOWNHOME  -0.3783 -0.7930  0.0291      800     53.2       621.8
    ## tau2         0.0156  0.0036  0.1698      800    100.0        13.2
    ## sigma2       0.0239  0.0043  0.1324      800    100.0        17.6
    ##             Geweke.diag
    ## (Intercept)         1.7
    ## PEXPOSURE          -0.3
    ## PCTAGE65P          -0.8
    ## PCTOWNHOME         -0.6
    ## tau2               -0.7
    ## sigma2             -3.0
    ## 
    ## DIC =  953.8041       p.d =  26.85413       LMPL =  -478.75

Let's take a look at the plots of the raw 'SMR' compared to the fitted 'RR'.

{% highlight r %}
p3<-spplot(NY8,"SMR",main=list(label="raw SMR"))
p4<-spplot(NY8,"RR",main=list(label="fitted RR"))

grid.arrange(p3,p4,nrow=1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-22-1.png)

The INLA package allows for some flexible options that 'approximate' Bayesian outputs.

{% highlight r %}
#INLA
row.names(NY8@data)<-seq(1:nrow(NY8@data))
NY8_nb<-poly2nb(NY8)
nb_NY8<-nb2INLA(file="NY8_nb.adj",NY8_nb)

Observed<-NY8$Cases2
PEXPOSURE<-NY8$PEXPOSURE
PCTAGE65P<-NY8$PCTAGE65P
PCTOWNHOME<-NY8$PCTOWNHOME
Expected<-NY8$EXP
ID<-as.numeric(row.names(as(NY8, "data.frame")))
data<-list(Observed=Observed,PEXPOSURE=PEXPOSURE,PCTAGE65P=PCTAGE65P,PCTOWNHOME=PCTOWNHOME,Expected=Expected,ID=ID)
{% endhighlight %}

Run the BYM model in INLA and compare to CARBayes. The BYM model includes both spatial and non-spatial random effects, and other options include the 'Besag' model (spatial only) or 'iid' model (non-spatial random effects only). We can compare models using DIC.

{% highlight r %}
#which model is best?  
#bym model  = UH + CH
#besag model = correlated heterogeneity (spatial model)
#iid model = UH

formula.bym<-Observed ~ 1 + PEXPOSURE + PCTAGE65P + PCTOWNHOME + f(ID,model="bym",graph="NY8_nb.adj") 

model.inla<-inla(formula.bym,family="poisson",data=data,control.compute=list(dic=T,graph=T),
                 E=Expected)

#how does this compare to CARBayes?
summary(model.inla)
{% endhighlight %}

    ## 
    ## Call:
    ##    c("inla(formula = formula.bym, family = \"poisson\", data = data, 
    ##    ", " E = Expected, control.compute = list(dic = T, graph = T))" ) 
    ## Time used:
    ##     Pre = 1.73, Running = 1.45, Post = 0.141, Total = 3.32 
    ## Fixed effects:
    ##               mean    sd 0.025quant 0.5quant 0.975quant   mode kld
    ## (Intercept) -0.686 0.191     -1.068   -0.684     -0.316 -0.680   0
    ## PEXPOSURE    0.152 0.035      0.084    0.152      0.220  0.152   0
    ## PCTAGE65P    4.029 0.627      2.780    4.035      5.243  4.047   0
    ## PCTOWNHOME  -0.369 0.199     -0.757   -0.370      0.023 -0.372   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     ID BYM model
    ## 
    ## Model hyperparameters:
    ##                                         mean      sd 0.025quant 0.5quant
    ## Precision for ID (iid component)     1724.11 1779.35      92.58  1181.29
    ## Precision for ID (spatial component) 1801.71 1812.72     110.29  1258.85
    ##                                      0.975quant   mode
    ## Precision for ID (iid component)        6484.99 235.12
    ## Precision for ID (spatial component)    6624.06 293.32
    ## 
    ## Expected number of effective parameters(stdev): 11.62(13.18)
    ## Number of equivalent replicates : 24.18 
    ## 
    ## Deviance Information Criterion (DIC) ...............: 956.64
    ## Deviance Information Criterion (DIC, saturated) ....: 389.68
    ## Effective number of parameters .....................: 11.66
    ## 
    ## Marginal log-Likelihood:  -447.73 
    ## Posterior marginals for the linear predictor and
    ##  the fitted values are computed

{% highlight r %}
model.inla$dic$dic
{% endhighlight %}

    ## [1] 956.6404

{% highlight r %}
NY8$fitted3<-model.inla$summary.fitted.values$mean

#how do the fitted compare to CARBayes?
p5<-spplot(NY8,"RR",main=list(label="fitted RR (CARBayes)"))
p6<-spplot(NY8,"fitted3",main=list(label="fitted RR (INLA)"))

grid.arrange(p5,p6,nrow=1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-24-1.png)

{% highlight r %}
#lets compare to a non-spatial model (iid)

formula.iid<-Observed ~ 1 + PEXPOSURE + PCTAGE65P + PCTOWNHOME + f(ID,model="iid",graph="NY8_nb.adj") 

model.inla<-inla(formula.iid,family="poisson",data=data,control.compute=list(dic=T,graph=T),
                 E=Expected)

summary(model.inla)
{% endhighlight %}

    ## 
    ## Call:
    ##    c("inla(formula = formula.iid, family = \"poisson\", data = data, 
    ##    ", " E = Expected, control.compute = list(dic = T, graph = T))" ) 
    ## Time used:
    ##     Pre = 1.63, Running = 0.341, Post = 0.133, Total = 2.1 
    ## Fixed effects:
    ##               mean    sd 0.025quant 0.5quant 0.975quant   mode kld
    ## (Intercept) -0.686 0.186     -1.057   -0.684     -0.326 -0.680   0
    ## PEXPOSURE    0.152 0.032      0.089    0.152      0.214  0.152   0
    ## PCTAGE65P    4.048 0.608      2.838    4.053      5.228  4.064   0
    ## PCTOWNHOME  -0.364 0.194     -0.741   -0.365      0.020 -0.367   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     ID IID model
    ## 
    ## Model hyperparameters:
    ##                      mean       sd 0.025quant 0.5quant 0.975quant  mode
    ## Precision for ID 18222.98 17205.01     149.08 12987.76   64238.91 11.38
    ## 
    ## Expected number of effective parameters(stdev): 5.47(6.86)
    ## Number of equivalent replicates : 51.35 
    ## 
    ## Deviance Information Criterion (DIC) ...............: 958.10
    ## Deviance Information Criterion (DIC, saturated) ....: 391.14
    ## Effective number of parameters .....................: 4.98
    ## 
    ## Marginal log-Likelihood:  -493.82 
    ## Posterior marginals for the linear predictor and
    ##  the fitted values are computed

{% highlight r %}
model.inla$dic$dic
{% endhighlight %}

    ## [1] 958.1022

### IN-class and homework

1.  Load the Scottish Lip Cancer dataset

Observed=number of lip cancer cases Expected=expected number of lip cancer cases (population standardized) PcAFF=proportion of population working in agriculture, forestry, or fisheries (why might this be important?)

1.  Test for spatial autocorrelation - you should know how to do this

2.  Compare a spatial model to a non-spatial model - which is better and why?

3.  Is the effect of PcAFF the same in both models? What is your interpretation of this covariate in the best model?

4.  Run a spatial model and map the fitted compared to the observed

{% highlight r %}
##predicting the log relative risk
Scot<-readOGR("/Users/abennett1/Documents/Teaching/","scot")
{% endhighlight %}

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/abennett1/Documents/Teaching", layer: "scot"
    ## with 56 features
    ## It has 2 fields
    ## Integer64 fields read as strings:  ID

{% highlight r %}
scot_dat <- read.csv("/Users/abennett1/Documents/Teaching/scotland.csv", header=T, stringsAsFactors=FALSE)

names(scot_dat) <- c("District","Observed", "Expected", "PcAFF",
                     "Latitude", "Longitude")

row.names(scot_dat) <- formatC(scot_dat$District, width = 2,
                               flag = "0")
ID <- formatC(Scot$ID, width = 2, flag = "0")
scot_LL <- spChFIDs(Scot, ID)
scot_LL <- spCbind(scot_LL, scot_dat[match(ID, row.names(scot_dat)), ])
plot(scot_LL)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-25-1.png)

{% highlight r %}
# Look at maps of observed and expected counts
O<-spplot(scot_LL,"Observed",main=list(label="Observed"),colorkey=list(at=seq(0,100,10)))
E<-spplot(scot_LL,"Expected",main=list(label="Expected"),colorkey=list(at=seq(0,100,10)))

grid.arrange(O,E,nrow=1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-25-2.png)

{% highlight r %}
#create SMR
scot_LL$SMR<-scot_LL$Observed/scot_LL$Expected

Scot_nb<-poly2nb(scot_LL)
Scot_nb_wb<-nb2WB(Scot_nb)

#plot in spplot
spplot(scot_LL,"SMR")
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-25-3.png)

{% highlight r %}
#plot in ggplot2
require('plyr')
scot_LL@data$id<-rownames(scot_LL@data)
scot.points<-fortify(scot_LL,region="id")
scot.df<-join(scot.points,scot_LL@data,by="id")
ggplot(scot.df)+aes(long,lat,group=group,fill=SMR)+geom_polygon()+scale_fill_continuous("SMR")
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-7-Areal-spatial-regression_files/figure-markdown_github/unnamed-chunk-25-4.png)

Key Readings
------------

Bivand R, Pebesma E, Gomez-Rubio V. (2013). Applied Spatial Data Analysis with R. Use R! Springer: New York (particularly chapter 9 on areal data)

Other resources
---------------

S. Banerjee, B.P. Carlin and A.E. Gelfand (2003). Hierarchical Modeling and Analysis for Spatial Data. Chapman & Hall.

To learn more about INLA: <http://www.r-inla.org/>

D.J. Spiegelhalter, N.G. Best, B.P. Carlin and A. Van der Linde (2002). Bayesian Measures of Model Complexity and Fit (with Discussion), Journal of the Royal Statistical Society, Series B 64(4), 583-616.

L.A. Waller and C.A. Gotway (2004). Applied Spatial Statistics for Public Health Data. Wiley & Sons.
