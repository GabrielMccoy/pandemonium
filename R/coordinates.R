#' Chi-Squared Loss Function Coordinates
#'
#' @param df data frame
#' @param covInv inverse covariance matrix
#' @param exp reference values
#' @param ... other expected values of getCoords
#' @returns matrix with coordinate representation of all points
#' @export
#'
pullCoords <- function(df, covInv, exp,...){

  n <- nrow(df)
  df <- as.matrix(df)
  nc <- ncol(df)
  coord_mat <- matrix(nrow = n, ncol = nc)

  for (i in 1:n){
    for (j in 1:nc){
      coord_mat[i, j] <- sum(covInv[j,] * (df[i,] - exp$value)) / sqrt(covInv[j, j])
    }
  }
  colnames(coord_mat) <- colnames(df)
  return(coord_mat)
}

#' Generic Loss Function Coordinates
#'
#' @param df data frame
#' @param cov covariance matrix
#' @param exp reference values
#' @param ... other expected values of getCoords
#'
#' @returns matrix with coordinate representation of all points
#' @export
#'
pullCoordsNoCov <- function(df, cov, exp, ...){

  n <- nrow(df)
  df <- as.matrix(df)
  nc <- ncol(df)
  coord_mat <- matrix(nrow = n, ncol = nc)

  for (i in 1:n){
    for (j in 1:nc){
      coord_mat[i, j] <- as.numeric((df[i, j] - exp$value[j]) / sqrt(cov[j, j]))
    }
  }
  colnames(coord_mat) <- colnames(df)
  return(coord_mat)
}

#' Normalisation coordinates
#'
#' @param df data frame
#' @param ... other expected values of getCoords
#'
#' @returns matrix with coordinate representation of all points
#' @export
#'
normCoords <- function(df, ...){
  return(scale(df))
}

#' Raw coordinates
#'
#' returns df, used when other coordinates may fail.
#' It is not recommended that these coordinates be used as distances may not be logical.
#'
#' @param df data frame
#' @param ... other expected values of getCoords
#'
#' @returns matrix with coordinate representation of all points
#' @export
#'
rawCoords <- function(df, ...){
  return(df)
}


#' User defined coordinate function
#'
#' Closure of a coordinate function that returns user defined coordinates.
#' Used where coordinates have already been computed. Only used where variables are not moved out of spaces.
#'
#' @param user_coords coordinate matrix the size of the space it will be used on
#'
#' @returns function that returns the user defined coordinates user_coords
#' @export
#'
userCoords <- function(user_coords){
  function(...){
    coord<- user_coords
    colnames(coord) <- colnames(df)
    return(user_coords)
  }
}
