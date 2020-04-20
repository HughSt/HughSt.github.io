kulldorff2 <- function (geo, cases, population, expected.cases = NULL, pop.upper.bound, 
          n.simulations, alpha.level, plot = TRUE) 
{
  if (is.null(expected.cases)) {
    type <- "binomial"
    denominator <- population
    expected.cases <- sum(cases) * (denominator/sum(denominator))
  }
  else {
    type <- "poisson"
    denominator <- expected.cases
  }
  geo.results <- zones(geo, population, pop.upper.bound)
  nearest.neighbors <- geo.results$nearest.neighbors
  cluster.coords <- geo.results$cluster.coords
  n.zones <- nrow(cluster.coords)
  lkhd <- computeAllLogLkhd(cases, denominator, nearest.neighbors, 
                            n.zones, type)
  cluster.index <- which.max(lkhd)
  center <- cluster.coords[cluster.index, 1]
  end <- cluster.coords[cluster.index, 2]
  cluster <- nearest.neighbors[[center]]
  cluster <- cluster[1:which(cluster == end)]
  perm <- rmultinom(n.simulations, round(sum(cases)), prob = denominator)
  sim.lambda <- kulldorffMC(perm, denominator, nearest.neighbors, 
                            n.zones, type)
  combined.lambda <- c(sim.lambda, max(lkhd))
  p.value <- 1 - mean(combined.lambda < max(lkhd, na.rm=T), na.rm=T)
  if (plot) {
    hist(combined.lambda, main = "Monte Carlo Distribution of Lambda", 
         xlab = expression(log(lambda)))
    abline(v = max(lkhd), col = "red")
    legend("top", c(paste("Obs. log(Lambda) = ", round(max(lkhd), 
                                                       3), sep = ""), paste("p-value = ", round(p.value, 
                                                                                                log10(n.simulations + 1)), sep = "")), lty = c(1, 
                                                                                                                                               1), col = c("red", "white"), bty = "n")
  }
  most.likely.cluster = list(location.IDs.included = cluster, 
                             population = sum(population[cluster]), number.of.cases = sum(cases[cluster]), 
                             expected.cases = sum(expected.cases[cluster]), SMR = sum(cases[cluster])/sum(expected.cases[cluster]), 
                             log.likelihood.ratio = lkhd[cluster.index], monte.carlo.rank = sum(combined.lambda >= 
                                                                                                  lkhd[cluster.index]), p.value = p.value)
  current.cluster <- cluster
  secondary.clusters <- NULL
  indices <- order(lkhd, decreasing = TRUE)
  for (i in 2:length(indices)) {
    new.cluster.index <- indices[i]
    new.center <- cluster.coords[new.cluster.index, 1]
    new.end <- cluster.coords[new.cluster.index, 2]
    new.cluster <- nearest.neighbors[[new.center]]
    new.cluster <- new.cluster[1:which(new.cluster == new.end)]
    if (length(intersect(new.cluster, current.cluster)) == 
        0) {
      new.secondary.cluster <- list(location.IDs.included = new.cluster, 
                                    population = sum(population[new.cluster]), number.of.cases = sum(cases[new.cluster]), 
                                    expected.cases = sum(expected.cases[new.cluster]), 
                                    SMR = sum(cases[new.cluster])/sum(expected.cases[new.cluster]), 
                                    log.likelihood.ratio = lkhd[new.cluster.index], 
                                    monte.carlo.rank = sum(combined.lambda >= lkhd[new.cluster.index]), 
                                    p.value = 1 - mean(combined.lambda < lkhd[new.cluster.index], na.rm=T))
      
      #browser()
      
      if (new.secondary.cluster$p.value > alpha.level) {
        break
      }
      secondary.clusters <- c(secondary.clusters, list(new.secondary.cluster))
      current.cluster <- unique(c(current.cluster, new.cluster))
    }
  }
  results <- list(most.likely.cluster = most.likely.cluster, 
                  secondary.clusters = secondary.clusters, type = type, 
                  log.lkhd = lkhd, simulated.log.lkhd = sim.lambda)
  return(results)
}