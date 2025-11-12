# function to make tours

function to make tours

## Usage

``` r
tourMaker(
  coord1,
  coord2,
  group,
  score,
  user_group,
  tourspace,
  colouring,
  out_dim,
  tour_path,
  display,
  radial_start = NULL,
  radial_var = NULL,
  slice_width = NULL,
  seed = NULL
)
```

## Arguments

- coord1:

  coordinate matrix in space 1

- coord2:

  coordinate matrix in space 2

- group:

  grouping assignment

- score:

  score assignments

- user_group:

  user defined grouping

- tourspace:

  space to show tour of

- colouring:

  colouring to use in plot

- out_dim:

  dimension of output tour

- tour_path:

  tour path and type to use, one of
  ("grand","cmass","holes","lda","pda","dcor","spline","radial","anomaly")

- display:

  display type, one of ("scatter","slice")

- radial_start:

  projection to use as start of radial tour, one of
  ("random","cmass","holes","lda","pda","dcor","spline")

- radial_var:

  variable to remove by radial tour

- slice_width:

  width of slice

- seed:

  sets the seed

## Value

detour
