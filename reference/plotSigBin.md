# Plot sigma bins in parameter space

Plot sigma bins in parameter space

## Usage

``` r
plotSigBin(
  wc,
  interest,
  bmID,
  sigmabins,
  x,
  y,
  binName,
  cond = NULL,
  colourSet = "Set2"
)
```

## Arguments

- wc:

  parameter values as matrix

- interest:

  logical vector showing that points are intersting

- bmID:

  index values for the benchmark points

- sigmabins:

  binning in sigma

- x, y:

  variables names (as string) to map to x and y axis

- binName:

  name for title

- cond:

  row numbers of points used for conditioning

- colourSet:

  RColorBrewer set for colouring

## Value

ggplot
