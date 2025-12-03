# t-Distributed Stochastic Neighbor Embedding

Computes non-linear dimension reduction with Rtsne and default
parameters.

## Usage

``` r
tSNE(dist, ...)
```

## Arguments

- dist:

  a distance matrix

- ...:

  other parameters expected to be passed to dimReduction

## Value

list containing a n x 2 matrix of reduced dimension data in Y

## Examples

``` r
head(tSNE(getDists(Bikes$space1, "euclidean"))$Y)
#>          [,1]       [,2]
#> [1,] 8.109459  10.499913
#> [2,] 9.429616   7.290623
#> [3,] 5.396701 -16.590592
#> [4,] 6.565829 -15.070396
#> [5,] 7.072085 -16.278762
#> [6,] 7.475196 -14.741429
```
