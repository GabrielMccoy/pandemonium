
#' Bin points based on chi2
#'
#' Map to values of sigma and compute equidistant binning in sigma.
#'
#' @param chivals vector with chi2 values
#' @param ndf number of parameters (degrees of freedom of the chi2 distribution)
#' @param k number of bins
#' @returns bin assignment for each point
#' @keywords internal
chi2bins <- function(chivals, ndf, k){
  chimin <- min(chivals)
  # map chivals to sigmas
  sigvals <- sqrt(stats::qchisq(stats::pchisq(chivals-chimin, ndf), 1))
  sigvals <- pmin(sigvals, 5)
  # get bins in sigma
  sigmabins <- seq(0, max(sigvals), length.out = k+1)
  sigbinned <- cut(sigvals, sigmabins,
                   include.lowest = TRUE,
                   labels = FALSE)
  sigbinned
}

#' Compute sigma
#'
#' Map chi2 to sigma, with cutoff (overflow) at 5 sigma
#'
#' @param chivals vector with chi2 values
#' @param ndf number of parameters (degrees of freedom of the chi2 distribution)
#' @returns vector with sigma values
#' @keywords internal
computeSigma <- function(chivals, ndf){
  chimin <- min(chivals)
  # map chivals to sigmas, cutoff at 5
  pmin(sqrt(stats::qchisq(stats::pchisq(chivals-chimin, ndf), 1)), 5)
}

#' Compute chi2 value for all points
#'
#' @param pred matrix of predicted values for all points
#' @param covInv inverse covariance matrix
#' @param exp experimentally observed values
#' @returns vector with chi2 values
#' @keywords internal
computeChi2 <- function(pred, covInv, exp){
  chi2 <- double(nrow(pred))
  for (i in 1:nrow(pred)){
    chi2[i] <- as.matrix(exp$value - pred[i,]) %*% covInv %*% t(as.matrix(exp$value - pred[i,]))
  }
  return(chi2)
}

#' Chi-squared scores function
#'
#' Can be used as getScores input in pandemonium.
#' Returns chi-squared values as the score and sigma bins as the bins.
#'
#' @param space1 dataframe with variables in space1
#' @param covinv inverse covariance matrix from space1
#' @param exp reference point from space 1
#' @param ... other expected values of getScore
#'
#' @returns named list containing scores for use in pandemonium
#'
#' @export
#'
#' @examples
#' chi2score(Bikes$space1,solve(cov(Bikes$space1)),
#'             data.frame(value = colMeans(Bikes$space1)))
#'
#'
chi2score <- function(space1, covinv, exp, ...){
  ret <-list()
  n<- nrow(space1)
  ndf<-ncol(space1)
  ret$score<- computeChi2(space1, covinv, exp)
  sig <- floor(computeSigma(ret$score,ndf))+1
  ret$bins <- factor(sig,labels = c("1","2","3","4","5","5+")[sort(unique(sig))])

  ret$interest <- rep("",n)
  ret$interest[which.min(ret$bins)]<- "bf"
  ret$is.interest <- which(ret$interest!="")
  ret$scoreName <- "chi2"
  ret$binName   <- "sigma"
  ret
}


#' Using externally computed score values
#'
#' Can be used as getScores input in pandemonium, to use score values
#' that are computed externally.
#' Returns scores values as the score, and bins computed
#' as below, between or above the first and third quartile.
#'
#' @param scores external scores to be passed to the app.
#' @param scoreName name for scores
#'
#' @returns named list containing scores for use in pandemonium
#'
#' @export
#'
#' @examplesIf interactive()
#' pandemonium(df = Bikes$space1, space2 = Bikes$space2,
#'               getScore = outsidescore(Bikes$other$res,"Residual"))
#'
#'
outsidescore <- function(scores,scoreName = NULL){
  function(space1, ...){
    ret <- list()
    n<- nrow(space1)
    ret$score <- scores
    ret$bins <- cut(scores, stats::quantile(scores,c(0,0.25,0.75,1))-c(1,0,0,0), labels=c("lower","inner","upper"))
    ret$interest <- rep("",n)
    ret$is.interest <- which(ret$interest!="")
    ret$scoreName <- as.character(scoreName)
    ret$binName   <- "Quartile"
    ret
  }
}
