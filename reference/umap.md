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
#> [1,] -1.364196 10.163737
#> [2,] -1.931106  9.703583
#> [3,] -8.226957 -9.111165
#> [4,] -8.611597 -8.459163
#> [5,] -8.651150 -8.444756
#> [6,] -8.895537 -8.097649
```
