# Compute cluster statistics

For number of clusters k between two and kmax, evaluate cluster
statistics collected in output tibble.

## Usage

``` r
getClusterStats(dist, fit, chivals, kmax = 10)
```

## Arguments

- dist:

  distances

- fit:

  result from hclust

- chivals:

  vector with chi2 values

- kmax:

  maximum number of clusters considered

## Value

data frame with cluster statistics
