library(terra)

random_region <- function(source_polys, p){
  # takes a collection of source_polys and a probability
  # p for each poly to be included in a random aggregate
  source_polys <- source_polys[runif(length(source_polys)) < p]
  x <- aggregate(source_polys)
  x$value <- sum(source_polys$value)
  x$count <- length(source_polys)
  return(x)
}

random_partition <- function(source_polys, lambda){
  # randomly aggregates source polys into larger units
  source_polys$count <- 1
  
  # n = number of aggregates
  n <- rpois(1, lambda)
  sizes <- rpois(n, length(source_polys)/n)
  return(sum(sizes))
}
