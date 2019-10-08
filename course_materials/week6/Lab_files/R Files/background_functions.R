# Background functions needed for the R markdown

univariate_lm <- function(n_data=40, seed=1) {
  set.seed(seed)
  x <- runif(n_data, -0.5, 2.5 * pi)
  x <- sort(x)
  y <- -8 + .25 * x + rnorm(n_data, 0, .1)
  dataset <- as.data.frame(cbind(x, y))
  return(dataset)
}

univariate_glm <- function(n_data=40, seed=1) {
  set.seed(seed)
  x <- runif(n_data, -0.5, 2.5 * pi)
  x <- sort(x)
  f <- -8 + .25 * x + rnorm(n_data, 0, .1)
  y <- 1/(1+exp(-f))
  dataset <- as.data.frame(cbind(x, y))
  return(dataset)
}

random_points <- function(n_points=10, xmax=399, ymax=399, seed=1) {
  "Generate a set of random points in the space [0, xmax] x [0, ymax]"
  set.seed(seed)
  lng <- runif(n_points, 0, xmax)
  lat <- runif(n_points, 0, ymax)
  pattern <- cbind(lng, lat)
  return(pattern)
}

soil_data <- function(n_data = 40, n_peaks=2, seed=1) {
  "Generate random data of organic matter measurements"
  set.seed(seed)
  sampled_pts <- as.data.frame(random_points(n_points=n_data, seed=seed))
  peak_values <- as.data.frame(random_points(n_points=n_peaks, seed=seed))

  r_mat <- as.matrix(rdist(as.matrix(sampled_pts), as.matrix(peak_values)))
  f <- exp(-  .01 * apply(r_mat, FUN=min, MARGIN=1))
  covariate <- runif(length(f), min=-1, max=2)
  eta <- 4 + .5 * covariate + f  + rnorm(n_data, 0, .1)
  sampled_pts$humidity <- covariate
  sampled_pts$OM <- exp(eta)
  return(sampled_pts)
}


make_grid <- function(size=10, xmax=399, ymax=399, seed=1) {
  "Generate a grid of size x size points in the space [0, xmax] x [0, ymax]"
  set.seed(seed)
  gridx <- seq(0, xmax, length.out=size)
  gridy <- seq(0, ymax, length.out=size)
  lng <- rep(gridx, size)
  lat <- sort(rep(gridy, size))
  grid <- cbind(lng, lat)
  return(grid)
}
