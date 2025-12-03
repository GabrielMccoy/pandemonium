# Generate a specified plot outside the GUI

An interface to generate a specific graph seen when using the GUI.
Settings include: metric, linkage, k, plotType, for details see the
vignette on using this function.

## Usage

``` r
makePlots(
  space1,
  settings,
  cov = NULL,
  covInv = NULL,
  exp = NULL,
  space2 = NULL,
  space2.cov = NULL,
  space2.covInv,
  space2.exp = NULL,
  user_dist = NULL,
  getCoordsSpace1 = normCoords,
  getCoordsSpace2 = normCoords,
  getScore = NULL
)
```

## Arguments

- space1:

  dataframe of variables in cluster space

- settings:

  list specifying parameters usually selected in the app

- cov:

  covariance matrix for space 1

- covInv:

  inverse covariance matrix for space 1

- exp:

  reference point in space 1

- space2:

  dataframe of variables in linked space

- space2.cov:

  covariance matrix for space 2

- space2.covInv:

  inverse covariance matrix for space 2

- space2.exp:

  reference point in space 2

- user_dist:

  user defined distances

- getCoordsSpace1:

  function to calculate coordinates in space 1

- getCoordsSpace2:

  function to calculate coordinates in space 2

- getScore:

  function to calculate scores and bins

## Value

ggplot, plotly or detourr plot depending on settings\$plotType

## Examples

``` r
makePlots(
  space1 = Bikes$space1,
  settings = list(
    plotType = "WC", x = "hum", y = "temp", k = 4, metric = "euclidean",
    linkage = "ward.D2", WCa = 0.5, showalpha = TRUE
  ), cov = cov(Bikes$space1),
  space2 = Bikes$space2, getScore = outsidescore(Bikes$other$res, "Residual")
)
#> Error in outsidescore(Bikes$other$res, "Residual"): could not find function "outsidescore"

makePlots(
  space1 = Bikes$space1,
  settings = list(
    plotType = "tour", k = 4, metric = "euclidean", linkage = "ward.D2",
    tourspace = "space1", colouring = "clustering", out_dim = 2, tour_path = "grand",
    display = "scatter", radial_start = NULL, radial_var = NULL, slice_width = NULL, seed = 2025
  ),
  cov = cov(Bikes$space1), space2 = Bikes$space2,
  getScore = outsidescore(Bikes$other$res, "Residual")
)
#> Error in outsidescore(Bikes$other$res, "Residual"): could not find function "outsidescore"
```
