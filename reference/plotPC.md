# Make parallel coordinate plot

Make parallel coordinate plot

## Usage

``` r
plotPC(
  coord,
  groups,
  benchmarkIds,
  filt,
  c = TRUE,
  s = TRUE,
  a = 0.2,
  pal = NULL
)
```

## Arguments

- coord:

  coordinate representation of points

- groups:

  grouping from clustering is numeric or can be made numeric by
  as.numeric

- benchmarkIds:

  index values of benchmarks

- filt:

  filter of groups

- c:

  centre

- s:

  rescale (default=TRUE)

- a:

  alpha transarancy for drawing non-benchmark points (default=0.2)

- pal:

  pallete for colour assignment

## Value

ggplot
