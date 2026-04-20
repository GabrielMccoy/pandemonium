# Generate Results for makePlots

Settings are: metric, linkage, k. for details see the vignette on
makePlots

## Usage

``` r
makeResults(
  cluster,
  settings,
  cov = NULL,
  covInv = NULL,
  exp = NULL,
  linked = NULL,
  linked.cov = NULL,
  linked.covInv,
  linked.exp = NULL,
  user_dist = NULL,
  getCoordsSpace1 = normCoords,
  getCoordsSpace2 = normCoords,
  getScore = NULL
)
```

## Arguments

- cluster:

  dataframe of variables in cluster space

- settings:

  list specifying parameters usually selected in the app

- cov:

  covariance matrix for space 1

- covInv:

  inverse covariance matrix for space 1

- exp:

  reference point in space 1

- linked:

  dataframe of variables in linked space

- linked.cov:

  covariance matrix for space 2

- linked.covInv:

  inverse covariance matrix for space 2

- linked.exp:

  reference point in space 2

- user_dist:

  user defined distances

- getCoordsSpace1:

  function to calculate coordinates in cluster space

- getCoordsSpace2:

  function to calculate coordinates in linked space

- getScore:

  function to calculate scores and bins

## Value

list of results to be passed to makePlots

## Examples

``` r
r <- makeResults(cluster = Bikes$space1, settings = list(k = 4,
   metric = "euclidean", linkage = "ward.D2"), cov = cov(Bikes$space1),
   linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual"))
makePlots(cluster = Bikes$space1, settings = list(plotType = "Obs",
   x = "hum", y = "temp", obs = "A1"), cov = cov(Bikes$space1),
   linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual"),
   results = r)

```
