# Uniform Manifold Approximation and Projection Embedding

Computes non-linear dimension reduction with uwot and default
parameters.

## Usage

``` r
umap(dist, ...)
```

## Arguments

- dist:

  a distance matrix

- ...:

  other parameters expected to be passed to dimReduction

## Value

list containing a 2 x n matrix of reduced dimension data

## Examples

``` r
head(umap(getDists(Bikes$space1, "euclidean"))$Y)
#>           [,1]      [,2]
#> [1,] -3.665122  7.880971
#> [2,] -3.357169  7.185910
#> [3,] -4.742634 -7.705166
#> [4,] -5.412978 -7.456743
#> [5,] -5.420894 -7.477645
#> [6,] -5.629749 -7.428062
```
