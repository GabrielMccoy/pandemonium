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
#>              yr       temp weathersit      atemp         hum  windspeed
#> [1,] -0.9477742 -1.2121585  0.9609633  0.2737209  0.86682987 -0.3469276
#> [2,] -0.9656762 -0.4010617  1.0685957 -0.4315946  0.01318555  0.6317704
#> [3,] -1.0148715 -0.6964289 -0.6985819 -1.7010524 -0.97924528  0.3783792
#> [4,] -0.9902481 -1.1873067 -0.8540061 -1.2402666  0.26317188 -0.5248996
#> [5,] -1.0217558 -0.8919491 -0.6651273 -1.1990644 -1.13116742 -0.4283720
#> [6,] -1.0095445 -1.6138316 -0.7366407 -0.6105821 -0.55138005 -1.5907236
```
