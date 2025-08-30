#' Write space2, coordinates and cluter assingment to a CSV file
#'
#' Settings include: metric, linkage, k, plotType
#'
#' @param space1 cluster space matrix
#' @param cov covariance matrix
#' @param covInv inverse covariance matrix
#' @param space2 space2 matrix
#' @param exp observable reference value (e.g. experimental measurement)
#' @param settings list specifying parameters usually selected in the app
#' @param filename path to write the results file to
#' @param user_coord input coordinate matrix (optional)
#' @param user_dist input distance matrix (optional)
#' @param getCoords function to calculate coordinates
#' @export
writeResults <- function(space1, cov, covInv, space2, exp, settings, filename,
                         user_coord=NULL, user_dist=NULL, getCoords = normCoords){
  n <- nrow(space1)
  chi2 <- computeChi2(space1, covInv, exp)
  sig <- computeSigma(chi2, 2)
  bf <- which.min(chi2)
  sm <- which.min(rowSums(abs(space2)))
  x <- colnames(space2)[1]
  y <- colnames(space2)[2]
  cond <- 1:nrow(space2)
  coord <- getCoords(space1, cov, covInv, exp, user_coord)
  dists <- getDists(coord, settings$metric, user_dist)
  fit <- stats::hclust(dists, settings$linkage)
  groups <- stats::cutree(fit, k=settings$k)
  lvl <- unique(groups[stats::order.dendrogram(stats::as.dendrogram(fit))])
  cluster <- as.numeric(factor(groups, levels= lvl))
  benchmarks <- getBenchmarkInformation(as.matrix(dists), groups)
  isBenchmark <- rep(0, nrow(space2))
  isBenchmark[benchmarks$id] <- 1
  coord <- as.data.frame(coord)
  colnames(coord) <- paste0("O", 1:ncol(coord))
  space2 %>%
    cbind(coord) %>%
    cbind(cluster) %>%
    cbind(isBenchmark) %>%
    utils::write.csv(filename, row.names = FALSE, quote = FALSE)

}
