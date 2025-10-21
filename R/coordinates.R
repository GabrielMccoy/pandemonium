#' Chi-Squared Loss Function Coordinates
#'
#' Computes coordinate values by comparing observed values
#' to the reference, using the covariance matrix as when
#' computing the chi-squared loss.
#'
#' @param df data frame
#' @param covInv inverse covariance matrix
#' @param exp reference values
#' @param ... other expected values of getCoords
#' @returns matrix with coordinate representation of all points
#' @export
#'
#'@examples
#' head(pullCoords(Bikes$space2,solve(cov(Bikes$space2)),
#'             data.frame(value = colMeans(Bikes$space2))))
#'
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
#' Coordinates are computed as centered by the reference value and
#' scaled with the standard deviation.
#' Uses the i,ith entry of the covariance matrix as the standard deviation of the ith variable.
#'
#' @param df data frame
#' @param cov covariance matrix
#' @param exp reference values
#' @param ... other expected values of getCoords
#'
#' @returns matrix with coordinate representation of all points
#' @export
#'
#' @examples
#' head(pullCoordsNoCov(Bikes$space2,cov(Bikes$space2),
#'                 data.frame(value = colMeans(Bikes$space2))))
#'
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

#' Scaled coordinates
#'
#' Using scale to center and scale the coordinates.
#'
#' @param df data frame
#' @param ... other expected values of getCoords
#'
#' @returns matrix with coordinate representation of all points
#' @export
#'
#' @examples
#' head(normCoords(Bikes$space2))
#'
#'
normCoords <- function(df, ...){
  return(scale(df))
}

#' Raw coordinates
#'
#' Returns the input data frame. This is used when other coordinate computations fail.
#' In general, scaling of the inputs is recommended before clustering.
#'
#' Externally calculated coordinates can be used through userCoords or
#' as input data with rawCoords used as the coordinate function.
#' The use of userCoords over rawCoords is in the treatment of input data.
#' As pandemonium displays the input data in many plots the use of coordinates
#' as input data will result in these plots being less meaningful for interpretation.
#' Use userCoords where coordinates are necessary to calculate distances but
#' interpretation from plots of clustering space is necessary.
#'
#'
#' @param df data frame
#' @param ... other expected values of getCoords
#'
#' @returns matrix with coordinate representation of all points
#' @export
#'
#' @examples
#' head(rawCoords(Bikes$space2))
#'
#'
rawCoords <- function(df, ...){
  return(df)
}


#' User defined coordinate function
#'
#' Allows the use of externally calculated coordinates in the app.
#' Can only be used when variables are not reassigned between the two spaces.
#'
#' Externally calculated coordinates can be used through userCoords or
#' as input data with rawCoords used as the coordinate function.
#' The use of userCoords over rawCoords is in the treatment of input data.
#' As pandemonium displays the input data in many plots the use of coordinates
#' as input data will result in these plots being less meaningful for interpretation.
#' Use userCoords where coordinates are necessary to calculate distances but
#' interpretation from plots of clustering space is necessary.
#'
#' @param user_coords coordinate matrix the size of the space it will be used on
#'
#' @returns function that returns the user defined coordinates user_coords
#' @export
#'
#' @examplesIf interactive()
#' pandemonium(df = Bikes$space1, space2 = Bikes$space2,
#'               coords = list(normalised = normCoords, space2 = userCoords(Bikes$space2)))
#'
#'
userCoords <- function(user_coords){
  function(df,...){
    coord<- user_coords
    colnames(coord) <- colnames(df)
    return(user_coords)
  }
}
