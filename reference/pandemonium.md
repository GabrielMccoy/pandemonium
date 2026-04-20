# Shiny app for exploring clustering solutions

Opening the GUI to cluster the data points based on values in linked.
Coordinates and distances are computed on the fly, or can be entered in
the function call.

## Usage

``` r
pandemonium(
  df,
  cov = NULL,
  is.inv = FALSE,
  exp = NULL,
  linked = NULL,
  linked.cov = NULL,
  linked.exp = NULL,
  group = NULL,
  label = NULL,
  user_dist = NULL,
  dimReduction = list(tSNE = tSNE, umap = umap),
  getCoords = list(normal = normCoords),
  getScore = NULL
)
```

## Arguments

- df:

  data frame of data, assumes space 1 but variables can be re-assigned
  in the app

- cov:

  covariance matrix (optional)

- is.inv:

  is the covariance matrix an inverse default FALSE

- exp:

  observable reference value (e.g. experimental measurement)

- linked:

  data frame assumed to be in space 2 but variables can be re-assigned
  in the app

- linked.cov:

  covariance matrix (optional)

- linked.exp:

  observable reference value (e.g. experimental measurement)

- group:

  grouping assignments

- label:

  point labels

- user_dist:

  input distance matrix (optional)

- dimReduction:

  named list of functions used for dimension reduction

- getCoords:

  named list containing functions to calculate coordinates

- getScore:

  named list containing functions to calculate scores to be plotted as
  bins and continuous value.

## Value

No return value, called to initiate 'shiny' app
