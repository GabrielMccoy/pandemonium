# Make coordinate plot

Parameter values no longer need to be on a regular grid pattern for this
plot.(modified)

## Usage

``` r
plotObs(coord, x, y, wc, obs, cond = NULL)
```

## Arguments

- coord:

  coordinate representation of points

- x, y:

  variables names (as string) to map to x and y axis

- wc:

  parameter values as matrix

- obs:

  observable to plot

- cond:

  row numbers of points used for conditioning

## Value

ggplot
