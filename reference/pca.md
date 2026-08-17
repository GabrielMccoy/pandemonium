# Principal Component Analysis

Computes and returns first two principal components of a given matrix
using stats::prcomp

## Usage

``` r
pca(mat, ...)
```

## Arguments

- mat:

  a coordinate matrix

- ...:

  other parameters expected to be passed to dimReduction

## Value

list containing a 2 x n matrix of reduced dimension data

## Examples

``` r
head(pca(Bikes$space1)$Y)
#>             PC1        PC2
#> [1,] -1.8108701  0.3639296
#> [2,] -1.2759202 -0.1750922
#> [3,] -0.4840057 -2.0557848
#> [4,] -0.5314133 -1.7968083
#> [5,] -0.4237427 -1.8702038
#> [6,] -0.4883908 -1.7369140
```
