---
layout: post
title: Week 8 - Spatial regression of areal data continued
featured-img: unemployment_USA_2016
---

Week 8 Spatial regression with areal data continued
================

This week will be a short week with a bit of review and then an introduction to include time in space-time models using INLA. We will be using a new source for the Scotland lip cancer data set, as well as some malaria data

We will start by loading libraries.

{% highlight r %}
library(rgdal)
library(foreign)
library(spdep)
library(ggplot2)
library(RColorBrewer)
library(sp)
library(maptools)
library(coda)
library(gridExtra)  #plotting multiple plots in one
library(INLA)  #Appr
library(SpatialEpi)
{% endhighlight %}

We will use the Scotland data that are available in the Spatial Epi package.

These well known data include observed and expected cases of lip cancer for counties of Scotland, as well as a variable for the percentage of the population engaged in agriculture, forestry, or fisheries. The scotland data includes a data frame (scotland-data) and a polygon file (scotland-spatial.polygon). We first need to link these into a Spatial Polygons Data Frame.

{% highlight r %}
data(scotland)

scot_dat<-scotland$data[,c("county.names","cases","expected","AFF")]
names(scot_dat) <- c("county", "Observed", "Expected", "PcAFF")
scot_dat$SMR <- scot_dat$Observed / scot_dat$Expected

map <- scotland$spatial.polygon
sapply(slot(map, "polygons"), function(x){slot(x, "ID")})
{% endhighlight %}

    ##  [1] "skye-lochalsh" "banff-buchan"  "caithness"     "berwickshire" 
    ##  [5] "ross-cromarty" "orkney"        "moray"         "shetland"     
    ##  [9] "lochaber"      "gordon"        "western.isles" "sutherland"   
    ## [13] "nairn"         "wigtown"       "NE.fife"       "kincardine"   
    ## [17] "badenoch"      "ettrick"       "inverness"     "roxburgh"     
    ## [21] "angus"         "aberdeen"      "argyll-bute"   "clydesdale"   
    ## [25] "kirkcaldy"     "dunfermline"   "nithsdale"     "east.lothian" 
    ## [29] "perth-kinross" "west.lothian"  "cumnock-doon"  "stewartry"    
    ## [33] "midlothian"    "stirling"      "kyle-carrick"  "inverclyde"   
    ## [37] "cunninghame"   "monklands"     "dumbarton"     "clydebank"    
    ## [41] "renfrew"       "falkirk"       "clackmannan"   "motherwell"   
    ## [45] "edinburgh"     "kilmarnock"    "east.kilbride" "hamilton"     
    ## [49] "glasgow"       "dundee"        "cumbernauld"   "bearsden"     
    ## [53] "eastwood"      "strathkelvin"  "tweeddale"     "annandale"

{% highlight r %}
rownames(scot_dat) <- scot_dat$county
map <- SpatialPolygonsDataFrame(map, scot_dat, match.ID = TRUE)
{% endhighlight %}

As previously, create a neigborhood matrix and output the INLA file.

{% highlight r %}
nb <- poly2nb(map)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
{% endhighlight %}

First we can test for spatial autocorrelation, we may need to use the zero.policy command for areas with no neighbors.

{% highlight r %}
#create weights
scot_w<-nb2listw(nb,zero.policy=T)

#moran test
moran.test(map$SMR,scot_w,zero.policy=T)
{% endhighlight %}

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  map$SMR  
    ## weights: scot_w  n reduced by no-neighbour observations
    ##   
    ## 
    ## Moran I statistic standard deviate = 5.6068, p-value = 1.03e-08
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.497024704      -0.019230769       0.008478093

For the models, we create two separate ids so that we can separate spatial from non-spatial effects.

{% highlight r %}
map$id.struct <- 1:nrow(map@data)
map$id <- 1:nrow(map@data)
{% endhighlight %}

Then we run a non-spatial model and look at the results.

{% highlight r %}
formula <- Observed ~ PcAFF +
  f(id, model = "iid")

res <- inla(formula,
            family = "poisson", data = map@data,
            E = Expected, control.predictor = list(compute = TRUE),control.compute=list(dic=T,graph=T))

summary(res)
{% endhighlight %}

    ## 
    ## Call:
    ##    c("inla(formula = formula, family = \"poisson\", data = map@data, 
    ##    ", " E = Expected, control.compute = list(dic = T, graph = T), ", 
    ##    " control.predictor = list(compute = TRUE))") 
    ## Time used:
    ##     Pre = 1.78, Running = 0.282, Post = 0.107, Total = 2.17 
    ## Fixed effects:
    ##               mean    sd 0.025quant 0.5quant 0.975quant   mode kld
    ## (Intercept) -0.488 0.157     -0.802   -0.486     -0.184 -0.483   0
    ## PcAFF        6.822 1.399      4.063    6.821      9.578  6.821   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     id IID model
    ## 
    ## Model hyperparameters:
    ##                  mean    sd 0.025quant 0.5quant 0.975quant mode
    ## Precision for id 3.09 0.901       1.69     2.96       5.20 2.74
    ## 
    ## Expected number of effective parameters(stdev): 39.12(2.93)
    ## Number of equivalent replicates : 1.43 
    ## 
    ## Deviance Information Criterion (DIC) ...............: 310.76
    ## Deviance Information Criterion (DIC, saturated) ....: 102.79
    ## Effective number of parameters .....................: 39.22
    ## 
    ## Marginal log-Likelihood:  -185.66 
    ## Posterior marginals for the linear predictor and
    ##  the fitted values are computed

{% highlight r %}
res$dic$dic
{% endhighlight %}

    ## [1] 310.7649

We compare the results to a spatial model - which includes the non-spatial random effect + the spatial ('besag') effect. We could also run this as a 'BYM' model but it's a bit more difficult to separate out the separate effects. Between the spatial and non-spatial, which is better?

{% highlight r %}
formula <- Observed ~ PcAFF +
  f(id.struct, model = "besag", graph = g,scale.model=T) +
  f(id, model = "iid")

res <- inla(formula,
            family = "poisson", data = map@data,
            E = Expected, control.predictor = list(compute = TRUE),control.compute=list(dic=T,graph=T))
summary(res)
{% endhighlight %}

    ## 
    ## Call:
    ##    c("inla(formula = formula, family = \"poisson\", data = map@data, 
    ##    ", " E = Expected, control.compute = list(dic = T, graph = T), ", 
    ##    " control.predictor = list(compute = TRUE))") 
    ## Time used:
    ##     Pre = 1.79, Running = 0.558, Post = 0.111, Total = 2.45 
    ## Fixed effects:
    ##               mean    sd 0.025quant 0.5quant 0.975quant   mode kld
    ## (Intercept) -0.305 0.120     -0.539   -0.306     -0.068 -0.307   0
    ## PcAFF        4.330 1.277      1.744    4.356      6.770  4.408   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     id.struct Besags ICAR model
    ##    id IID model
    ## 
    ## Model hyperparameters:
    ##                             mean       sd 0.025quant 0.5quant 0.975quant
    ## Precision for id.struct     4.15     1.45       2.02     3.91       7.63
    ## Precision for id        19340.23 19385.83    1347.00 13600.84   70978.11
    ##                            mode
    ## Precision for id.struct    3.49
    ## Precision for id        3679.30
    ## 
    ## Expected number of effective parameters(stdev): 28.55(3.53)
    ## Number of equivalent replicates : 1.96 
    ## 
    ## Deviance Information Criterion (DIC) ...............: 299.28
    ## Deviance Information Criterion (DIC, saturated) ....: 91.31
    ## Effective number of parameters .....................: 28.84
    ## 
    ## Marginal log-Likelihood:  -189.69 
    ## Posterior marginals for the linear predictor and
    ##  the fitted values are computed

{% highlight r %}
res$dic$dic
{% endhighlight %}

    ## [1] 299.2819

We can plot the outputs of the model, first the non-spatial random effects (uncorrelated) and second the spatial random effects (correlated). We can also plot the fitted RR versus the raw SMR. How do they differ?

{% highlight r %}
sum<-res$summary.random
RE1<-sum$id[1:56,2]   #uncorrelated RE
RE2<-sum$id.struct[1:56,2]   #correlated RE

map$RE1 <- RE1
map$RE2 <- RE2

map$RR <- res$summary.fitted.values[, "mean"]

p1<-spplot(map,"RE1",main=list(label="uncorrelated"))
p2<-spplot(map,"RE2",main=list(label="correlated"))

grid.arrange(p1,p2,nrow=1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-8-1.png)

{% highlight r %}
p1<-spplot(map,"SMR",main=list(label="raw SMR"))
p2<-spplot(map,"RR",main=list(label="fitted RR"))
           
grid.arrange(p1,p2,nrow=1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-8-2.png)

The previous outputs did not take into account uncertainty, Bayesian/INLA models allow one to assess probability statements that incorporate uncertainty. One way to do this is to look at 'exceedence probabilities', or the probability that the RR exceeds a predefined threshhold in a given area. We do this by using the marginal distribution of the output from the model for each area. We evaluate the probability that the RR is greater than 2 for each area.

{% highlight r %}
marginal<-res$marginals.fitted.values[[1]]
1-inla.pmarginal(q=2,marginal=marginal)
{% endhighlight %}

    ## [1] 0.9975116

We can apply the inla.pmarginal across the dataset and then map it.

{% highlight r %}
exc<-sapply(res$marginals.fitted.values, FUN=function(marginal){1-inla.pmarginal(q=2,marginal=marginal)})

map$exc<-exc

spplot(map,"exc",main=list(label="probability of RR>2"))
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-10-1.png)

Space-time data
---------------

Now we're going to look at how to include a temporal effect in a space-time model. We're going to use a dataset on monthly confirmed cases of malaria at the district level from Zambia over a three year period.

{% highlight r %}
#read in shape file for districts in Zambia
geoZAM<-readOGR("/Users/abennett1/Documents/Teaching/Spatial Epi 2017/Nov 15 17/data/","admin2")
{% endhighlight %}

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/abennett1/Documents/Teaching/Spatial Epi 2017/Nov 15 17/data", layer: "admin2"
    ## with 72 features
    ## It has 3 fields

{% highlight r %}
plot(geoZAM)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-11-1.png)

{% highlight r %}
setwd("/Users/abennett1/Documents/Teaching/Spatial Epi 2017/Nov 15 17/data/")
{% endhighlight %}

The data include a variable indicating month (monthid) and a variable indicating district (id). For INLA, we want the data to be in long format (each observation is a district-month). We have data per district-month on the number of cases reported, and we also have some climate variables and the numbers of insecticide-treated nets available per hh.

{% highlight r %}
#read in data in district-time long format - data are malaria cases per district and month from 
#Jan 2009 - Dec 2011... covariates are standardized and include temperature, rainfall, vegetation, bed nets
zaminla<-read.table(file="/Users/abennett1/Documents/Teaching/Spatial Epi 2017/Nov 15 17/data/newHMIS3_27.txt",sep=",",header=TRUE)
zam.df<-as.data.frame(zaminla)
head(zam.df)
{% endhighlight %}

    ##   monthid id cases mrfe2_3stan maxtm2cstan mintm2cstan evim1cat
    ## 1       1  1   463   0.3524341   0.9424822   0.5849355        2
    ## 2       2  1   525   1.2442990   0.3186475   1.0741860        2
    ## 3       3  1   547   1.3818860   0.1414023   1.2691490        1
    ## 4       4  1   759   1.1743930  -0.6149052   1.2340820        1
    ## 5       5  1   561   0.6897768  -0.3706206   1.1656470        1
    ## 6       6  1   391  -0.0288701   0.2601520   0.5349405        1
    ##   itnperhhanom quarter        E
    ## 1   -0.0192643       0 1557.739
    ## 2   -0.0192643       0 1557.739
    ## 3   -0.0192643       0 1557.739
    ## 4   -0.0192643       1 1557.739
    ## 5   -0.0192643       1 1557.739
    ## 6   -0.0192643       1 1557.739

We're going to use the expected (E) as an offset in this model.

{% highlight r %}
adjpoly<-poly2nb(geoZAM)

nb2INLA(file="ZM.adj",adjpoly)

#add log E for offset 
zam.df$logE<-log(zam.df$E)
zam.df$monthid2<-zam.df$monthid

zam.df$id.struct<-zam.df$id
{% endhighlight %}

Now we'll run an inla model that includes both spatial and temporal random effects. As with spatial effects, there are a number of options for how we model the temporal random effect. These include 'ar1', which is an autoregressive model based on the previous value. We can also consider a purely 'iid' model. We're going to use what's known as a 'random walk' prior ('rw1') where each value is random but anchored to the previous. You can find more about what models are available using inla.list.models(latent), and there is documentation for specific models using inla.doc('rw1') (for example).

We index the temporal effect using monthid. We include as covariates the itns available per hh, and also a variable on vegetation to reflect climate variability. We then run the model and look at the model summary.

{% highlight r %}
formula.ST<-cases ~ 1 + offset(logE) + itnperhhanom + as.factor(evim1cat) + f(id.struct,model="besag",graph="ZM.adj") + f(id,model="iid") + f(monthid,model="rw1")
model.inla.ST<-inla(formula.ST,family="nbinomial",data=zam.df,control.compute=list(dic=T,graph=T))

#check the fixed results
summary(model.inla.ST)
{% endhighlight %}

    ## 
    ## Call:
    ##    c("inla(formula = formula.ST, family = \"nbinomial\", data = 
    ##    zam.df, ", " control.compute = list(dic = T, graph = T))") 
    ## Time used:
    ##     Pre = 2.3, Running = 15.6, Post = 0.344, Total = 18.2 
    ## Fixed effects:
    ##                        mean    sd 0.025quant 0.5quant 0.975quant   mode
    ## (Intercept)          -0.627 0.032     -0.689   -0.627     -0.565 -0.627
    ## itnperhhanom         -0.118 0.017     -0.151   -0.118     -0.086 -0.119
    ## as.factor(evim1cat)1  0.195 0.030      0.136    0.195      0.253  0.195
    ## as.factor(evim1cat)2  0.359 0.046      0.269    0.359      0.449  0.359
    ## as.factor(evim1cat)3  0.420 0.056      0.310    0.420      0.529  0.420
    ##                      kld
    ## (Intercept)            0
    ## itnperhhanom           0
    ## as.factor(evim1cat)1   0
    ## as.factor(evim1cat)2   0
    ## as.factor(evim1cat)3   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     id.struct Besags ICAR model
    ##    id IID model
    ##    monthid RW1 model
    ## 
    ## Model hyperparameters:
    ##                                                            mean       sd
    ## size for the nbinomial observations (1/overdispersion) 6.89e+00 1.94e-01
    ## Precision for id.struct                                3.14e-01 5.30e-02
    ## Precision for id                                       1.87e+04 1.84e+04
    ## Precision for monthid                                  1.54e+01 3.85e+00
    ##                                                        0.025quant 0.5quant
    ## size for the nbinomial observations (1/overdispersion)      6.521 6.89e+00
    ## Precision for id.struct                                     0.223 3.09e-01
    ## Precision for id                                         1263.082 1.33e+04
    ## Precision for monthid                                       9.128 1.50e+01
    ##                                                        0.975quant     mode
    ## size for the nbinomial observations (1/overdispersion)   7.28e+00    6.883
    ## Precision for id.struct                                  4.31e-01    0.299
    ## Precision for id                                         6.74e+04 3449.056
    ## Precision for monthid                                    2.42e+01   14.192
    ## 
    ## Expected number of effective parameters(stdev): 108.56(0.477)
    ## Number of equivalent replicates : 23.88 
    ## 
    ## Deviance Information Criterion (DIC) ...............: 38595.55
    ## Deviance Information Criterion (DIC, saturated) ....: 2766.76
    ## Effective number of parameters .....................: 109.55
    ## 
    ## Marginal log-Likelihood:  -19596.89 
    ## Posterior marginals for the linear predictor and
    ##  the fitted values are computed

{% highlight r %}
exp(model.inla.ST$summary.fixed)
{% endhighlight %}

    ##                           mean       sd 0.025quant  0.5quant 0.975quant
    ## (Intercept)          0.5342126 1.032182  0.5020610 0.5341900  0.5685245
    ## itnperhhanom         0.8883174 1.016693  0.8600058 0.8882817  0.9177382
    ## as.factor(evim1cat)1 1.2147538 1.030402  1.1453706 1.2147570  1.2882489
    ## as.factor(evim1cat)2 1.4317175 1.047036  1.3081270 1.4317331  1.5667561
    ## as.factor(evim1cat)3 1.5212075 1.057586  1.3627504 1.5212415  1.6977016
    ##                           mode      kld
    ## (Intercept)          0.5341460 1.000001
    ## itnperhhanom         0.8882117 1.000001
    ## as.factor(evim1cat)1 1.2147665 1.000000
    ## as.factor(evim1cat)2 1.4317702 1.000000
    ## as.factor(evim1cat)3 1.5213177 1.000000

{% highlight r %}
model.inla.ST$dic$dic
{% endhighlight %}

    ## [1] 38595.55

We can look at the random effect by area - both correlated and uncorrelated.

{% highlight r %}
#look at the random effects by area
sum<-model.inla.ST$summary.random
RE1<-sum$id[1:length(unique(zam.df$id)),2]        #uncorrelated
RE2<-sum$id.struct[1:length(unique(zam.df$id.struct)),2]    #correlated

#add the random effects to the spdf
geoZAM$RE1<-RE1
geoZAM$RE2<-RE2

#plot spatial effects using spplot - you can also try ggplot
p1<-spplot(geoZAM,"RE1")
p2<-spplot(geoZAM,"RE2")

grid.arrange(p1,p2,nrow=1)   
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-15-1.png)

We can plot the temporal effects and upper and lower bound as well using the monthid index.

{% highlight r %}
plot(sum$monthid[1:36,1],sum$monthid[1:36,2],type="l")
points(sum$monthid[1:36,4],type="l",lty=2)
points(sum$monthid[1:36,6],type="l",lty=2)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-16-1.png)

We can also add an interaction term for space x time interactions.

{% highlight r %}
zam.df$ID.area.month <- seq(1,length(unique(zam.df$id)))
{% endhighlight %}

We run the same model as before but with the interaction term and compare the results. Does this improve the model at all?

{% highlight r %}
formula.STint<- cases ~ 1 + itnperhhanom + as.factor(evim1cat) + f(id,model="bym",graph="ZM.adj") +
  f(monthid,model="rw1") +f(ID.area.month, model="iid")
model.inla.STint <- inla(formula.STint,family="nbinomial",data=zam.df, E=E,control.compute=list(dic=T,graph=T))

summary(model.inla.STint)
{% endhighlight %}

    ## 
    ## Call:
    ##    c("inla(formula = formula.STint, family = \"nbinomial\", data = 
    ##    zam.df, ", " E = E, control.compute = list(dic = T, graph = T))") 
    ## Time used:
    ##     Pre = 2.37, Running = 15, Post = 0.569, Total = 18 
    ## Fixed effects:
    ##                        mean    sd 0.025quant 0.5quant 0.975quant   mode
    ## (Intercept)          -0.627 0.032     -0.689   -0.627     -0.564 -0.627
    ## itnperhhanom         -0.118 0.017     -0.151   -0.119     -0.086 -0.119
    ## as.factor(evim1cat)1  0.194 0.030      0.136    0.194      0.253  0.194
    ## as.factor(evim1cat)2  0.359 0.046      0.268    0.359      0.449  0.359
    ## as.factor(evim1cat)3  0.419 0.056      0.309    0.419      0.529  0.419
    ##                      kld
    ## (Intercept)            0
    ## itnperhhanom           0
    ## as.factor(evim1cat)1   0
    ## as.factor(evim1cat)2   0
    ## as.factor(evim1cat)3   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     id BYM model
    ##    monthid RW1 model
    ##    ID.area.month IID model
    ## 
    ## Model hyperparameters:
    ##                                                            mean       sd
    ## size for the nbinomial observations (1/overdispersion) 6.90e+00 1.94e-01
    ## Precision for id (iid component)                       1.90e+03 1.85e+03
    ## Precision for id (spatial component)                   3.14e-01 5.30e-02
    ## Precision for monthid                                  1.54e+01 3.86e+00
    ## Precision for ID.area.month                            1.93e+04 1.86e+04
    ##                                                        0.025quant 0.5quant
    ## size for the nbinomial observations (1/overdispersion)      6.521     6.89
    ## Precision for id (iid component)                          127.827  1351.86
    ## Precision for id (spatial component)                        0.223     0.31
    ## Precision for monthid                                       9.129    14.97
    ## Precision for ID.area.month                              1501.824 13944.01
    ##                                                        0.975quant     mode
    ## size for the nbinomial observations (1/overdispersion)   7.29e+00    6.889
    ## Precision for id (iid component)                         6.80e+03  349.682
    ## Precision for id (spatial component)                     4.31e-01    0.302
    ## Precision for monthid                                    2.42e+01   14.152
    ## Precision for ID.area.month                              6.83e+04 4221.055
    ## 
    ## Expected number of effective parameters(stdev): 109.40(1.42)
    ## Number of equivalent replicates : 23.69 
    ## 
    ## Deviance Information Criterion (DIC) ...............: 38595.70
    ## Deviance Information Criterion (DIC, saturated) ....: 2769.03
    ## Effective number of parameters .....................: 110.46
    ## 
    ## Marginal log-Likelihood:  -19529.00 
    ## Posterior marginals for the linear predictor and
    ##  the fitted values are computed

{% highlight r %}
exp(model.inla.STint$summary.fixed)
{% endhighlight %}

    ##                           mean       sd 0.025quant  0.5quant 0.975quant
    ## (Intercept)          0.5342863 1.032491  0.5018276 0.5342654  0.5689354
    ## itnperhhanom         0.8882756 1.016691  0.8599681 0.8882399  0.9176916
    ## as.factor(evim1cat)1 1.2146562 1.030395  1.1452959 1.2146581  1.2881327
    ## as.factor(evim1cat)2 1.4313419 1.047016  1.3078613 1.4313446  1.5663290
    ## as.factor(evim1cat)3 1.5207478 1.057566  1.3624246 1.5207672  1.6971704
    ##                           mode      kld
    ## (Intercept)          0.5342252 1.000001
    ## itnperhhanom         0.8881700 1.000001
    ## as.factor(evim1cat)1 1.2146652 1.000000
    ## as.factor(evim1cat)2 1.4313555 1.000000
    ## as.factor(evim1cat)3 1.5208131 1.000000

{% highlight r %}
model.inla.STint$dic$dic
{% endhighlight %}

    ## [1] 38595.7

We can use the spacetime package to create plots of cases for all districts over each month.

{% highlight r %}
##ST-Plots for district
library(spacetime)
{% endhighlight %}

    ## Registered S3 method overwritten by 'xts':
    ##   method     from
    ##   as.zoo.xts zoo

{% highlight r %}
library("maps")
library("plm")
library(RColorBrewer)
library(zoo)
{% endhighlight %}

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

{% highlight r %}
#time variable
monthid=1:length(unique(zam.df$monthid))
time=yearmon(2009 + (monthid-1)/12)

#space-time data frame - needs to be ordered with spatial index moving fastest
spacetime=STFDF(geoZAM,time,zam.df[order(zam.df[1],zam.df[2]),])

#plotting cases 
stplot(spacetime[,1:12,"cases"],names.attr=monthid[1:12],col.regions=brewer.pal(9,"YlOrRd"),cuts=9)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-19-1.png)

If we use an example where there are many more spatial units, in this case all of the facilities in Zambia, we can see greater differences between the spatial and non-spatial components.

{% highlight r %}
setwd("/Users/abennett1/Documents/Teaching/Spatial Epi 2017/Nov 15 17/data/")

#add facility tesselation - polygon created for every health facility point
geoZAM1<-readOGR("/Users/abennett1/Documents/Teaching/Spatial Epi 2017/Nov 15 17/data/","zamvoronoiclip3sjoin2")
{% endhighlight %}

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/abennett1/Documents/Teaching/Spatial Epi 2017/Nov 15 17/data", layer: "zamvoronoiclip3sjoin2"
    ## with 1369 features
    ## It has 12 fields
    ## Integer64 fields read as strings:  confcaseto

{% highlight r %}
#we will duplicate it for use later
geoZAM2<-geoZAM1

#plot facility tesselation
plot(geoZAM2)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-20-1.png)

{% highlight r %}
#prepare inla objects
adjpoly2<-poly2nb(geoZAM2)

nb2INLA(file="ZMhf.adj",adjpoly2)

#add facility level data - case data by facility and month for 2009-2012
zam.hf<-read.table(file="zamHF09_12confimp.txt",sep=",",header=TRUE)
zam.hf.df<-as.data.frame(zam.hf)

#structured id
zam.hf.df$id.struct<-zam.hf.df$id

#create expected counts for each facility based on mean over the entire period
A<-c()
for (i in 1:1369) {
  A[i]<-as.numeric(with(subset(zam.hf.df, id==i), mean(totalconfcaseST, na.rm=TRUE)))
}

E<-c()
for (i in 1:1369) {
  E<-c(E, rep(A[i], times=48, na.rm=FALSE))
}

#add expected counts
zam.hf.df$E<-E

#subset data - for speed
zam.hf.sub.df<-subset(zam.hf.df, monthid>24)

#run besag + iid model
formula.ST1<- totalconfcaseST ~ 1 + f(id.struct,model="besag",graph="ZMhf.adj") +
  f(id,model="iid") + f(monthid,model="rw1")
model.inla.ST1 <- inla(formula.ST1,family="poisson",data=zam.hf.sub.df, E=E)

summary(model.inla.ST1)
{% endhighlight %}

    ## 
    ## Call:
    ##    c("inla(formula = formula.ST1, family = \"poisson\", data = 
    ##    zam.hf.sub.df, ", " E = E)") 
    ## Time used:
    ##     Pre = 2.27, Running = 41.4, Post = 0.163, Total = 43.9 
    ## Fixed effects:
    ##               mean    sd 0.025quant 0.5quant 0.975quant   mode kld
    ## (Intercept) -0.041 0.004     -0.048   -0.041     -0.034 -0.041   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     id.struct Besags ICAR model
    ##    id IID model
    ##    monthid RW1 model
    ## 
    ## Model hyperparameters:
    ##                          mean   sd 0.025quant 0.5quant 0.975quant  mode
    ## Precision for id.struct 17.25 2.14      13.35    17.15      21.75 16.97
    ## Precision for id        68.58 7.57      55.08    68.09      84.79 67.03
    ## Precision for monthid    9.23 2.63       5.01     8.93      15.26  8.34
    ## 
    ## Expected number of effective parameters(stdev): 1322.52(2.84)
    ## Number of equivalent replicates : 24.84 
    ## 
    ## Marginal log-Likelihood:  -782008.71

{% highlight r %}
#check the random effects
sum<-model.inla.ST1$summary.random
RE1<-sum$id[1:length(unique(zam.hf.sub.df$id)),2]   # uncorrelated RE
RE2<-sum$id.struct[1:length(unique(zam.hf.sub.df$id.struct)),2]   # correlated RE

#add to the hf spdf
geoZAM2$RE1<-RE1
geoZAM2$RE2<-RE2

#plot using spplot
p1<-spplot(geoZAM2,"RE1")
p2<-spplot(geoZAM2,"RE2")

grid.arrange(p1,p2,nrow=1)
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-20-2.png)

{% highlight r %}
#plot using ggplot
require('plyr')
{% endhighlight %}

    ## Loading required package: plyr

    ## 
    ## Attaching package: 'plyr'

    ## The following object is masked from 'package:maps':
    ## 
    ##     ozone

{% highlight r %}
geoZAM2@data$id<-rownames(geoZAM2@data)
geoZAM2.points<-fortify(geoZAM2,region="id")
geoZAM2.df<-join(geoZAM2.points,geoZAM2@data,by="id")
g1<-ggplot(geoZAM2.df)+aes(long,lat,group=group,fill=RE1)+geom_polygon()+scale_fill_continuous("UH")+coord_fixed()
g2<-ggplot(geoZAM2.df)+aes(long,lat,group=group,fill=RE2)+geom_polygon()+scale_fill_continuous("CH")+coord_fixed()

grid.arrange(g1,g2,nrow=1)  #compare the UH and CH
{% endhighlight %}

![](https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/_posts/Week-8-Areal-spatial-regression-continued_files/figure-markdown_github/unnamed-chunk-20-3.png)

Key Readings
------------

Bivand R, Pebesma E, Gomez-Rubio V. (2013). Applied Spatial Data Analysis with R. Use R! Springer: New York (particularly chapter 9 on areal data)

Other resources
---------------

Moraga, Paula. 2018. “Small Area Disease Risk Estimation and Visualization Using R.” The R Journal 10 (1): 495–506. <https://journal.r-project.org/archive/2018/RJ-2018-036/index.html>.

Rue, Havard, Finn Lindgren, Daniel Simpson, Sara Martino, Elias Teixeira Krainski, Haakon Bakka, Andrea Riebler, and Geir-Arne Fuglstad. 2018. INLA: Full Bayesian Analysis of Latent Gaussian Models Using Integrated Nested Laplace Approximations.

