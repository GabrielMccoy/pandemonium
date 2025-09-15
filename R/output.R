#' Write coordinates and cluster assignment to a CSV file
#'
#' Settings used: metric, linkage, k
#'
#' @param space1 cluster space matrix
#' @param cov covariance matrix
#' @param covInv inverse covariance matrix
#' @param exp observable reference value (e.g. experimental measurement)
#' @param space2 space2 matrix
#' @param space2.cov covariance matrix
#' @param space2.covInv inverse covariance matrix
#' @param space2.exp observable reference value (e.g. experimental measurement)
#' @param settings list specifying parameters usually selected in the app
#' @param filename path to write the results file to
#' @param user_dist input distance matrix (optional)
#' @param getCoords.space1 function to calculate coordinates on clustering space
#' @param getCoords.space2 function to calculate coordinates on linked space
#'
#'@examples
#'file<-tempfile()
#'writeResults(space1 = Bikes$space1, space2 = Bikes$space2,
#' settings = list(metric="euclidean",linkage="ward.D2",k=4), filename = file)
#'file.remove(file)
#'
#'
#' @export
writeResults <- function(space1, cov=NULL, covInv=NULL, exp=NULL,
                         space2, space2.cov=NULL, space2.covInv=NULL, space2.exp=NULL,
                         settings, filename, user_dist=NULL,
                         getCoords.space1 = normCoords, getCoords.space2 = rawCoords){

  #coordinate calculations
  coord <- getCoords.space1(df=space1, cov=cov, covInv=covInv, exp=exp)
  coord_2 <- getCoords.space2(df=space2, cov=space2.cov, covInv=space2.covInv, exp=space2.exp)

  #clustering calculations
  dists <- getDists(coord, settings$metric, user_dist)
  fit <- stats::hclust(dists, settings$linkage)
  groups <- stats::cutree(fit, k=settings$k)
  lvl <- unique(groups[stats::order.dendrogram(stats::as.dendrogram(fit))])
  cluster <- as.numeric(factor(groups, levels= lvl))
  benchmarks <- getBenchmarkInformation(as.matrix(dists), groups)
  isBenchmark <- rep(0, nrow(space2))
  isBenchmark[benchmarks$id] <- 1

  #output building
  coord <- as.data.frame(coord)
  output <- dplyr::bind_cols(coord,coord_2,clstr=cluster,BM=isBenchmark)

  #output
  if(requireNamespace("readr", quietly=TRUE)){
    readr::write_csv(output,filename)
  } else {
    utils::write.csv(output,filename, row.names = FALSE, quote = FALSE)
  }

}
