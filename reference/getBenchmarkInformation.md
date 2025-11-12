# Compute cluster information

The returned tibble contains the id of the cluster benchmark, the
cluster radius and diameter, and group number for each cluster.

## Usage

``` r
getBenchmarkInformation(dmat, groups)
```

## Arguments

- dmat:

  distance matrix

- groups:

  groups resulting from clustering

## Value

data frame with cluster information

## Examples

``` r
dists <- getDists(Bikes$space1,"euclidean")
fit <- stats::hclust(dists, "ward.D2")
groups <- stats::cutree(fit, k = 4)
getBenchmarkInformation(as.matrix(dists), groups)
#> # A tibble: 4 × 4
#>      id     r     d group
#>   <dbl> <dbl> <dbl> <dbl>
#> 1   300 0.914  4.36     1
#> 2   363 1.75   2.99     2
#> 3   244 1.21   2.61     3
#> 4   485 1.56   3.96     4
```
