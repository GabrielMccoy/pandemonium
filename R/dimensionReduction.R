#' t-Distributed Stochastic Neighbor Embedding
#'
#' Computes non-linear dimension reduction with Rtsne and default parameters.
#'
#' @param dist a distance matrix
#' @param ... other parameters expected to be passed to dimReduction
#'
#' @returns list containing a n x 2 matrix of reduced dimension data in Y
#' @export
#'
#' @examples
#' head(tSNE(getDists(Bikes$space1, "euclidean"))$Y)
#'
tSNE <- function(dist, ...) {
  Rtsne::Rtsne(dist, is_distance = TRUE)
}

#' Uniform Manifold Approximation and Projection Embedding
#'
#' Computes non-linear dimension reduction with uwot and default parameters.
#'
#' @param dist a distance matrix
#' @param ... other parameters expected to be passed to dimReduction
#'
#' @returns list containing a 2 x n matrix of reduced dimension data
#' @export
#'
#' @examples
#' head(umap(getDists(Bikes$space1, "euclidean"))$Y)
#'
umap <- function(dist, ...) {
  ret <- list()
  ret$Y <- uwot::umap(stats::as.dist(dist))
  ret
}

#' Principal Component Analysis
#'
#' Computes and returns first two principal components of a given matrix using
#' stats::prcomp
#'
#' @param mat a coordinate matrix
#' @param ... other parameters expected to be passed to dimReduction
#'
#' @returns list containing a 2 x n matrix of reduced dimension data
#' @export
#'
#' @examples
#' head(pca(Bikes$space1)$Y)
#'
pca <- function(mat, ...) {
  ret <- list()
  ret$Y <- stats::prcomp(mat)$x[,1:2]
  ret
}
