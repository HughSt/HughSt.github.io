
library("ggplot2")
options(repr.plot.width=5, repr.plot.height=4)

publish_gg <- function(gg) {
  "Display a ggplot"
  gg_bundle <- IRdisplay::prepare_mimebundle(gg)
  IRdisplay::publish_mimebundle(gg_bundle$data, gg_bundle$metadata)
}

?spData::nc.sids

# Load North Carolina Data
nc <- rgdal::readOGR(system.file("shapes/sids.shp", package = "spData")[1])

# Easy plot
sp::plot(nc, axes=TRUE, main="North Carolina")
text(sp::coordinates(nc), label = nc$CNTY_ID, cex = .3)

class(nc)

head(nc@data)

# ggplot
ncfort <- fortify(nc, region = "NAME")
head(ncfort)

#?dplyr::left_join

# Join variables
ncplot <- dplyr::left_join(ncfort, nc@data[, -c(19,20)], by=c("id"="NAME"))

head(ncplot)

plt <- ggplot(ncplot, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill="white", col="black") +
    geom_text(data=nc@data, aes(x=lon, y=lat, group=0, label=CNTY_ID), size=1.5) +
    coord_equal() + 
    theme_void()
publish_gg(plt)

plt <- ggplot(ncplot, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = SID74)) +
    geom_path(col = 'black', alpha = 0.5, size = 0.1) +
    viridis::scale_fill_viridis() +
    coord_equal() + 
    theme_void()
publish_gg(plt)

plt <- ggplot(ncplot, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = BIR74)) +
    geom_path(col = 'black', alpha = 0.5, size = 0.1) +
    viridis::scale_fill_viridis() +
    coord_equal() + 
    theme_void()
publish_gg(plt)

plt <- ggplot(ncplot, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = SID74 / BIR74)) +
    geom_path(col = 'black', alpha = 0.5, size = 0.1) +
    viridis::scale_fill_viridis() +
    coord_equal() + 
    theme_void()
publish_gg(plt)

#Neighbors
nc_neighbours <- spdep::poly2nb(nc, row.names=nc@data$NAME)
names(nc_neighbours) <- attr(nc_neighbours, "region.id")

nc_neighbours

sp::plot(nc)
sp::plot(nc_neighbours, sp::coordinates(nc), col="blue", add=TRUE)

ncdf <- nc@data
ncdf$xx <- nc@data$SID74 / nc@data$BIR74

head(ncdf)

m1 <- mgcv::gam(xx ~ s(NAME, bs = "mrf", xt = list(nb = nc_neighbours)),
          data = ncdf, weights=BIR74, method = "REML", family = binomial)

summary(m1)

ncdf$prev_estimate <- mgcv::predict.gam(m1, newdata = ncdf, type="response")

plt <- ggplot(ncdf, aes(SID74, prev_estimate * BIR74)) +
        geom_abline(intercept = 0, slope = 1, col="grey") +
        geom_point(col="steelblue") +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))

publish_gg(plt)

ncplot <- dplyr::left_join(ncfort, ncdf[, -c(19,20)], by=c("id"="NAME"))

plt <- ggplot(ncplot, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = prev_estimate * BIR74)) +
    geom_path(col = 'black', alpha = 0.5, size = 0.1) +
    viridis::scale_fill_viridis() +
    coord_equal() +
    theme_void()
publish_gg(plt)

plt <- ggplot(ncplot, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = prev_estimate)) +
    geom_path(col = 'black', alpha = 0.5, size = 0.1) +
    viridis::scale_fill_viridis() +
    coord_equal() +
    theme_void()
publish_gg(plt)

# Now with a Poisson model
m2 <- mgcv::gam(SID74 ~ offset(log(BIR74)) + s(NAME, bs = "mrf", xt = list(nb = nc_neighbours)),
          data = ncdf, method = "REML", family = poisson)

# y = beta_0 + beta_1 x + z
# y ~ x + offset(z)

ncdf$poisson_estimate <- mgcv::predict.gam(m2, newdata = nc@data, type="response")

plt <- ggplot(ncdf, aes(SID74, poisson_estimate)) +
        geom_abline(intercept = 0, slope = 1, col="grey") +
        geom_point(col="steelblue") +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))

publish_gg(plt)

ncplot <- dplyr::left_join(ncfort, ncdf[, -c(19,20)], by=c("id"="NAME"))
plt <- ggplot(ncplot, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = poisson_estimate/BIR74)) +
    geom_path(col = 'black', alpha = 0.5, size = 0.1) +
    viridis::scale_fill_viridis() +
    coord_equal() +
    theme_void()
publish_gg(plt)
