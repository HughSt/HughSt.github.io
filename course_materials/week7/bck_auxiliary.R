# Functions needed for the sessions

cov_example <- function(n_points=10, seed=1) {
  "Generate points ordered in space (moving in south-east direction)."
  set.seed(seed)
  lon <- sort(runif(n_points, 0, 1), decreasing=TRUE)
  lat <- rep(0, n_points)
  a <- 0
  for (i in 1:n_points) {
    lat[i] <- runif(1, a, 1 - lon[i])
    a <- 1 - lon[i]
  }
  
  pts <- data.frame(lon=lon * 3.6 - 81.5,
                    lat=lat * 2.85 + 34,
                    tag=n_points:1)
  #colnames(pts) <- c("lon", "lat")
  
  return(pts)
}





lm_data <- function(n_data=40, seed=1) {
  "Generate random data for a 1D linear regression example"
  set.seed(seed)
  x <- runif(n_data, 0.5, 2.5 * pi)
  y = exp(-3 + .25 * sin(x) + rnorm(n_data, 0, .1))
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


soil_data <- function(n_data = 40, n_peaks=3, seed=1) {
  "Generate random data of soiliness measurements"
  set.seed(seed)
  sampled_pts <- as.data.frame(random_points(n_points=n_data, seed=seed))
  peak_values <- as.data.frame(random_points(n_points=n_peaks, seed=seed))

  r_mat <- pracma::distmat(as.matrix(sampled_pts), as.matrix(peak_values))
  eta = exp(-  .01 * apply(r_mat, FUN=min, MARGIN=1) + rnorm(n_data, 0, .2))
  sampled_pts$soiliness <- 1/(1 + exp(-eta)) 
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


rbf <- function(X1, X2, sigma=1, lengthscale=1){
  "Exponentiated quadratic kernel"
  r <- pracma::distmat(as.matrix(X1), as.matrix(X2))
  k <- sigma^2 * exp( - .5 * r^2 / lengthscale^2)
  return(k)
}


rqd <- function(X1, X2, sigma=1, lengthscale=1, alpha=1){
  "Rational quadratic kernel"
  r <- pracma::distmat(X1, X2)
  k <- sigma^2 * (1 + r^2 / (2 * alpha * lengthscale^2))^-alpha 
  return(k)
}


pow <- function(X1, X2, sigma=1, lengthscale=1, gamma=1){
  "Power exponential kernel"
  r <- pracma::distmat(X1, X2)
  k <- sigma^2 * exp(- r^gamma / lengthscale^gamma)
  return(k)
}
 

k_folds <- function(X, Y, k, iter){
  "Split the data into a training and validation set"
  ix = 1:length(Y)
  
}


random_points_ordered <- function(n_points=5, seed=1){
  "Generate points ordered in space (moving in south-east direction)."
  set.seed(seed)
  
  lng <- sort(runif(n_points, 0, 399), decreasing=TRUE)
  lat <- rep(0, n_points)
  a <- 0
  for (i in 1:n_points) {
    lat[i] <- runif(1, a, 400 - lng[i])
    a <- 400 - lng[i]
  }
  pts <- cbind(lng, lat)
  return(pts)
}


gp_regression_rbf <- function(X, Y, X_pred, k_var, k_len, noise_var) {
  "Compute predictive mean and variance using a GP regression model"
  
  # Compute matrices
  Kxx <- rbf(X, X, sigma=sqrt(k_var), lengthscale=k_len)
  Kss <- rbf(X_pred, X_pred, sigma=sqrt(k_var), lengthscale=k_len)
  Ksx <- rbf(X_pred, X, sigma=sqrt(k_var), lengthscale=k_len)
  Kxx_Ie <- Kxx + diag(x=noise_var, nrow=nrow(X))
  W <- chol2inv(chol(Kxx_Ie))
  
  # Predictive mean and variance
  gp_mean <- Ksx %*% W %*% Y
  gp_var <- diag(Kss - Ksx %*% W %*% t(Ksx)) # We will store only the diagonal
  
  return(list("gp_mean"=gp_mean, "gp_var"=gp_var))
}


gp_regression_rqd <- function(X, Y, X_pred, k_var, k_len, noise_var, alpha=1) {
  "Compute predictive mean and variance using a GP regression model"
  
  # Compute matrices
  Kxx <- rqd(X, X, sigma=sqrt(k_var), lengthscale=k_len, alpha=alpha)
  Kss <- rqd(X_pred, X_pred, sigma=sqrt(k_var), lengthscale=k_len, alpha=alpha)
  Ksx <- rqd(X_pred, X, sigma=sqrt(k_var), lengthscale=k_len, alpha=alpha)
  Kxx_Ie <- Kxx + diag(x=noise_var, nrow=nrow(X))
  W <- chol2inv(chol(Kxx_Ie))
  
  # Predictive mean and variance
  gp_mean <- Ksx %*% W %*% Y
  gp_var <- diag(Kss - Ksx %*% W %*% t(Ksx)) # We will store only the diagonal
  
  return(list("gp_mean"=gp_mean, "gp_var"=gp_var))
}
