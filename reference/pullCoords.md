# Chi-Squared Loss Function Coordinates

Computes coordinate values by comparing observed values to the
reference, using the covariance matrix as when computing the chi-squared
loss.

## Usage

``` r
pullCoords(df, covInv, exp, ...)
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
head(pullCoords(
  Bikes$space2, solve(cov(Bikes$space2)),
  data.frame(value = colMeans(Bikes$space2))
))
#>              yr         temp weathersit       atemp         hum  windspeed
#> [1,] -0.8292029 -1.046511064  0.3768171  0.94035509  0.60997604 -0.1313423
#> [2,] -0.9130219  0.003682338  0.7378488 -0.06234677  0.01749241  0.5709120
#> [3,] -1.0428131  0.682771257 -0.3578216 -0.87471321 -0.63015414  0.1022673
#> [4,] -0.9163204  0.029964266 -0.9802563 -0.26735866  0.38279082 -0.5303732
#> [5,] -1.0908367  0.212017821 -0.1012415 -0.38494219 -0.98100221 -0.6324212
#> [6,] -1.0442625 -0.687876640 -0.2763956  0.47016164 -0.68068108 -1.5712841
```
