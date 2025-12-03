# Show clusters in parameter space

Show clusters in parameter space

## Usage

``` r
plotWC(
  wc,
  x,
  y,
  interest,
  bmID,
  col,
  cond = NULL,
  groups = NULL,
  pal = NULL,
  a = 0.2,
  showalpha = TRUE
)
```

## Arguments

- wc:

  parameter values as matrix

- x, y:

  variables names (as string) to map to x and y axis

- interest:

  index values for the intersting points

- bmID:

  index values of benchmarks

- col:

  color vector according to cluster assignment

- cond:

  row numbers of points used for conditioning

- groups:

  grouping assignments used to make alphahull

- pal:

  pallete used for group colouring of alphahull

- a:

  alpha value for alpha hull

- showalpha:

  boolean value to calculate and show alpha hulls

## Value

ggplot
