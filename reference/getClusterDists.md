# Compute cluster distance summaries

The returned tibble contains the id of the cluster pairs, with benchmark
distance (d1), minimum (d2) and maximum (d3) distances between any
points in the two clusters.

## Usage

``` r
getClusterDists(dmat, groups, benchmarks)
```

## Arguments

- dmat:

  distance matrix

- groups:

  groups resulting from clustering

- benchmarks:

  data frame with benchmark id and group number

## Value

data frame with distance information

## Examples

``` r
dists <- getDists(Bikes$space1, "euclidean")
fit <- stats::hclust(dists, "ward.D2")
groups <- stats::cutree(fit, k = 4)
bm <- getBenchmarkInformation(as.matrix(dists), groups)
getClusterDists(as.matrix(dists), groups, bm)
#> # A tibble: 6 × 5
#>     grA   grB    d1     d2    d3
#>   <dbl> <dbl> <dbl>  <dbl> <dbl>
#> 1     1     2  2.69 0.397   5.76
#> 2     1     3  1.96 0.571   5.16
#> 3     1     4  2.72 0.422   6.00
#> 4     2     3  1.99 0.0643  3.82
#> 5     2     4  2.23 0.212   4.71
#> 6     3     4  1.56 0.793   3.81
```
