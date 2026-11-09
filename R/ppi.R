
#' Gap Index
#'
#' projection pursuit index for use in tourr::guidedtour()
#' for finding a view with origin maximally separated
#'
#' @param origin vector like?? origin to separate
#'  if none is given 0,0,... is used
#'
#' @export
#'
gap_pp <- function(origin = NULL) {
  function(mat) {
    if (!is.null(origin)) mat <- sweep(mat, 2, origin)
    min(sqrt(rowSums(mat^2)))
  }
}


#' nearest hull?
#'
#' @param X matrix like
#' @param it max number if iterations
#' @param tol tolerance
#'
#' @keywords internal
#'
hull_nearest <- function(X, it = 20000, tol = 1e-12) {
  X <- as.matrix(X)
  x <- X[which.min(rowSums(X^2)), ]
  for (k in seq_len(it)) {
    d <- X[which.min(X %*% x), ] - x
    g <- -sum(x * d) / sum(d * d)
    if (g <= tol) break
    x <- x + min(1, g) * d
  }
  x
}


#' Gap Starting Projection
#'
#' optimised initial projection for use with gap_pp
#'
#' @param X matrix like data for projection
#'
#' @returns starting projection
#' @export
#'
gap_start <- function(X) {
  X <- as.matrix(X); p <- ncol(X)
  xh <- hull_nearest(X)
  if (sqrt(sum(xh^2)) < 1e-8) return(tourr::basis_random(p, 2))
  w <- xh / sqrt(sum(xh^2))
  R <- X - (X %*% w) %*% t(w)
  v <- stats::prcomp(R)$rotation[, 1]
  v <- v - sum(v * w) * w
  cbind(w, v / sqrt(sum(v^2)))
}
