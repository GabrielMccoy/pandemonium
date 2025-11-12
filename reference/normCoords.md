# Scaled coordinates

Using scale to center and scale the coordinates.

## Usage

``` r
normCoords(df, ...)
```

## Arguments

- df:

  data frame

- ...:

  other expected values of getCoords

## Value

matrix with coordinate representation of all points

## Examples

``` r
head(normCoords(Bikes$space2))
#>             yr       temp weathersit      atemp        hum   windspeed
#> [1,] -1.000684 -0.8260965  1.1096668 -0.6794808  1.2493159 -0.38762628
#> [2,] -1.000684 -0.7206013  1.1096668 -0.7401455  0.4787852  0.74908882
#> [3,] -1.000684 -1.6335382 -0.7255514 -1.7485698 -1.3383576  0.74612099
#> [4,] -1.000684 -1.6136748 -0.7255514 -1.6091685 -0.2630015 -0.38956182
#> [5,] -1.000684 -1.4664099 -0.7255514 -1.5039409 -1.3405763 -0.04627497
#> [6,] -1.000684 -1.5899219 -0.7255514 -1.4797695 -0.7697378 -1.30224238

```
