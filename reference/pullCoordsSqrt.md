# Chi-Squared Loss Function Coordinates

Computes coordinate values by comparing observed values to the
reference, using the square root inverse covariance matrix as when
computing the chi-squared loss.

## Usage

``` r
pullCoordsSqrt(df, covInv, exp, ...)
```

## Arguments

- df:

  data frame

- covInv:

  inverse covariance matrix

- exp:

  reference values

- ...:

  other expected values of getCoords

## Value

matrix with coordinate representation of all points

## Examples

``` r
head(pullCoordsSqrt(
  Bikes$space2, solve(cov(Bikes$space2)),
  data.frame(value = colMeans(Bikes$space2))
))
#> Error in loadNamespace(x): there is no package called ‘expm’
```
