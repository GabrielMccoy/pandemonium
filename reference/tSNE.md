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
#>           [,1]      [,2]
#> [1,] -5.923845  8.903472
#> [2,] -2.816945 10.261091
#> [3,] 19.149796  4.951020
#> [4,] 17.792176  6.221425
#> [5,] 18.909975  6.630759
#> [6,] 17.574394  7.032094
```
