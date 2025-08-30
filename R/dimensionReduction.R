#' t-Distributed Stochastic Neighbor Embedding Through Rtsne
#'
#' @param dist a distance matrix
#' @param ... other parameters expected to be passed to dimReduction
#'
#' @returns list containing a 2 x n matrix of reduced dimension data
#' @export
#'
tSNE <- function(dist,...) {
  Rtsne::Rtsne(dist, is_distance=TRUE)
}

#' Uniform Manifold Approximation and Projection Embedding Through uwot
#'
#' @param dist a distance matrix
#' @param ... other parameters expected to be passed to dimReduction
#'
#' @returns list containing a 2 x n matrix of reduced dimension data
#' @export
#'
umap <- function(dist,...) {
  ret <- list()
  ret$Y <- uwot::umap(stats::as.dist(dist))
  ret
}
