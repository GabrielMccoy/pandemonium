# Plot chi2

Parameter values no longer need to be on a regular grid pattern for this
plot.(modified)

## Usage

``` r
plotChi2(wc, chi2, x, y, scoreName = NULL, cond = NULL)
```

## Arguments

- wc:

  parameter values as matrix

- chi2:

  vector with chi2 values

- x, y:

  variables names (as string) to map to x and y axis

- scoreName:

  name for title

- cond:

  row numbers of points used for conditioning

## Value

ggplot
