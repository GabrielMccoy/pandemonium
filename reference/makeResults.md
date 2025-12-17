# Generate Results for makePlots

Settings are: metric, linkage, k. for details see the vignette on
makePlots

## Usage

``` r
makeResults(
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

list of results to be passed to makePlots

## Examples

``` r
r <- makeResults(space1 = Bikes$space1, settings = list(k = 4,
   metric = "euclidean", linkage = "ward.D2"), cov = cov(Bikes$space1),
   space2 = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual"))
makePlots(space1 = Bikes$space1, settings = list(plotType = "Obs",
   x = "hum", y = "temp", obs = "A1"), cov = cov(Bikes$space1),
   space2 = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual"),
   results = r)

```
