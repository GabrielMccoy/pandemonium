#' Compute distances between all points
#'
#' @param coord matrix with coordinate representation of all points
#' @param metric name of distance metric to be used
#' @param user_dist user distance returned with metric=user
#' @return distances between all points
#' @export
#'
#' @examples
#' getDists(Bikes$space1,"euclidean")
#' getDists(Bikes$space1,"maximum")
#'
getDists <- function(coord, metric, user_dist=NULL){
  if(metric == "user") return(stats::as.dist(user_dist))
  stats::dist(coord, method = metric)
}


#' Compute cluster information
#'
#' The returned tibble contains the id of the cluster benchmark,
#' the cluster radius and diameter, and group number for each cluster.
#'
#' @param dmat distance matrix
#' @param groups groups resulting from clustering
#' @return data frame with cluster information
#' @export
#'
#' @examples
#' dists <- getDists(Bikes$space1,"euclidean")
#' fit <- stats::hclust(dists, "ward.D2")
#' groups <- stats::cutree(fit, k = 4)
#' getBenchmarkInformation(as.matrix(dists), groups)
#'
getBenchmarkInformation <- function(dmat, groups){
  k <- length(unique(groups))
  ret <- tibble::tibble(id = numeric(length = k),
                        r = numeric(length = k), d = numeric(length = k),
                        group = numeric(length = k))
  i <- 1
  for (gr in unique(groups)){
    idx <- which(groups==gr)
    d_gr <- dmat[idx, idx]
    if(length(idx) == 1) d_vec <- 0
    else d_vec <- colSums(d_gr^2)
    id <- idx[which.min(d_vec)]
    d <- max(d_gr)
    r <- max(d_gr[which.min(d_vec)])
    ret[i,] <- t(c(id, r, d, gr))
    i <- i+1
  }
  ret
}

#' Compute cluster distance summaries
#'
#' The returned tibble contains the id of the cluster pairs,
#' with benchmark distance (d1), minimum (d2) and maximum (d3) distances
#' between any points in the two clusters.
#'
#' @param dmat distance matrix
#' @param groups groups resulting from clustering
#' @param benchmarks data frame with benchmark id and group number
#' @return data frame with distance information
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' dists <- getDists(Bikes$space1,"euclidean")
#' fit <- stats::hclust(dists, "ward.D2")
#' groups <- stats::cutree(fit, k = 4)
#' bm <- getBenchmarkInformation(as.matrix(dists), groups)
#' getClusterDists(as.matrix(dists), groups, bm)
#'
getClusterDists <- function(dmat, groups, benchmarks){
  k <- length(unique(groups))
  n <- choose(k, 2)
  ret <- tibble::tibble(grA = numeric(length = n),  grB = numeric(length = n),
                        d1 = numeric(length = n), d2 = numeric(length = n),
                        d3 = numeric(length = n))

  ni <- 1
  for (i in 1:(k-1)){
    for (j in (i+1):k){

      id1 <- dplyr::filter(benchmarks, .data$group == i)$id
      id2 <- dplyr::filter(benchmarks, .data$group == j)$id

      d1 <- dmat[id1, id2]

      idx1 <- which(groups==i)
      idx2 <- which(groups==j)


      d2 <- min(dmat[idx1, idx2])
      d3 <- max(dmat[idx1, idx2])


      ret[ni,] <- t(c(i, j, d1, d2, d3))
      ni <- ni + 1
    }
  }
  ret
}

#' Compute cluster statistics
#'
#' For number of clusters k between two and kmax, evaluate cluster
#' statistics collected in output tibble.
#'
#' @param dist distances
#' @param fit result from hclust
#' @param chivals vector with chi2 values
#' @param kmax maximum number of clusters considered
#' @return data frame with cluster statistics
#' @keywords internal
getClusterStats <- function(dist, fit, chivals, kmax=10){
  ret <- tibble::tibble(k = numeric(length = kmax-1),
                        within.cluster.ss = numeric(length = kmax-1),
                        wb.ratio = numeric(length = kmax-1),
                        ch = numeric(length = kmax-1),
                        pearsongamma = numeric(length = kmax-1),
                        dunn = numeric(length = kmax-1),
                        dchi2rand = numeric(length = kmax-1),
                        rmax = numeric(length = kmax-1),
                        dmax = numeric(length = kmax-1),
                        dmin = numeric(length = kmax-1))
  for(k in 2:kmax){
    gr <- stats::cutree(fit, k)
    chibins <- chi2bins(chivals, 2, k)

    x <- fpc::cluster.stats(dist, gr, alt.clustering = chibins)
    bmInfo <- getBenchmarkInformation(as.matrix(dist), gr)
    bmDists <- getClusterDists(as.matrix(dist), gr, bmInfo)
    bmMinDist <- min(bmDists$d1)

    ret[k-1,] <- t(c(k, x$within.cluster.ss, x$wb.ratio,
                     x$ch, x$pearsongamma, x$dunn,
                     x$corrected.rand,
                     max(bmInfo$r), max(bmInfo$d),
                     bmMinDist))
  }
  ret
}

cstat_names <- c(
  "within.cluster.ss" = "Cluster SS",
  "wb.ratio" = "WB ratio",
  "pearsongamma" = "Normalized gamma",
  "dunn" = "Dunn index",
  "ch" = "Calinski and Harabasz index",
  "rmax" = "Maximum radius",
  "dmax" = "Maximum diameter",
  "dmin" = "Minimum benchmark distance",
  "dchi2rand" = "ARI with CI binning"
)

#' function for labeling cluster statistics on statistics page of pandemonium GUI
#'
#' @keywords internal
#'
cstat_labeller <- function(){
  return(ggplot2::labeller(stat = cstat_names))
}

#' function for assigning colouring, palette and labels
#'
#' @param choice choice of colouring for an output
#' @param rv reactive variables
#'
#' @returns list containing colour assignment, palette and labels for use in plotting
#' @keywords internal
#'
colourHelper <- function(choice,rv){
  ret <- list()
  ret$colour<- switch(choice,
                      "clustering"  = rv$groups,
                      "user"        = rv$user.group,
                      "bins"        = rv$colSig,
                      "score"       = rank(rv$value$score))
  ret$pal <-    switch(choice,
                       "clustering"  = rv$pal,
                       "user"        = rv$user.pal,
                       "bins"        = rv$palSig,
                       "score"       = rv$scorecol)
  ret$label <- switch(choice,
                      "clustering"  = paste(rv$groups,rv$label,rv$value$interest),
                      "user"        = paste(rv$user.group,rv$label,rv$value$interest),
                      "bins"        = paste(rv$value$bins,rv$label,rv$value$interest),
                      "score"       = paste(rv$label,rv$value$interest))
  ret
}
