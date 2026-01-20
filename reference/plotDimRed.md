# Plot dimension reduction plot

Plot dimension reduction plot

## Usage

``` r
plotDimRed(
  coord1,
  coord2,
  d_mat1,
  d_mat2,
  data,
  colouring,
  dimReduction,
  algorithm,
  group,
  score,
  user_group,
  pch,
  interactive,
  seed = NULL
)
```

## Arguments

- coord1:

  coordinates in space 1

- coord2:

  coordinates in space 2

- d_mat1:

  distance matrix in space 1

- d_mat2:

  distance matrix in space 2

- data:

  either "space1" or "space2"

- colouring:

  either "clustering", "user", "bins" or "score"

- dimReduction:

  function to calculate dimension reduction with \$Y being the new n x 2
  matrix

- algorithm:

  name for algorithm used for labeling plot

- group:

  grouping of points from clustering

- score:

  score values and bins

- user_group:

  user defined grouping

- pch:

  factor with 2 levels 1 will be plotted as a circle 2 will be plotted
  as an o

- interactive:

  TRUE or FALSE if true returns plotly plot if false returns ggplot2
  plot

- seed:

  sets the seed

## Value

plotly or ggplot2 plot
