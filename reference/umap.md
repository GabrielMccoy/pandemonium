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
head(umap(getDists(Bikes$space1,"euclidean"))$Y)
#>           [,1]      [,2]
#> [1,] -3.371390  7.899499
#> [2,] -3.605518  7.194001
#> [3,] -6.843035 -6.904671
#> [4,] -7.101922 -6.299586
#> [5,] -7.114751 -6.268647
#> [6,] -7.148083 -6.023505

```
