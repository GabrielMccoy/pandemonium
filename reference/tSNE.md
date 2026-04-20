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
#>           [,1]       [,2]
#> [1,]  6.178729  12.048674
#> [2,]  7.773660   8.938407
#> [3,]  8.458022 -16.521862
#> [4,]  9.619268 -15.078421
#> [5,] 10.104621 -16.276274
#> [6,] 10.529553 -14.810654
```
