# Write coordinates and cluster assignment to a CSV file

For working with the results outside the app. Settings used: metric,
linkage, k

## Usage

``` r
writeResults(
  space1,
  cov = NULL,
  covInv = NULL,
  exp = NULL,
  space2,
  space2.cov = NULL,
  space2.covInv = NULL,
  space2.exp = NULL,
  settings,
  filename,
  user_dist = NULL,
  getCoords.space1 = normCoords,
  getCoords.space2 = rawCoords
)
```

## Arguments

- space1:

  cluster space matrix

- cov:

  covariance matrix

- covInv:

  inverse covariance matrix

- exp:

  observable reference value (e.g. experimental measurement)

- space2:

  space2 matrix

- space2.cov:

  covariance matrix

- space2.covInv:

  inverse covariance matrix

- space2.exp:

  observable reference value (e.g. experimental measurement)

- settings:

  list specifying parameters usually selected in the app

- filename:

  path to write the results file to

- user_dist:

  input distance matrix (optional)

- getCoords.space1:

  function to calculate coordinates on clustering space

- getCoords.space2:

  function to calculate coordinates on linked space

## Value

No return value, called for writing file

## Examples

``` r
file <- tempfile()
writeResults(
  space1 = Bikes$space1, space2 = Bikes$space2,
  settings = list(metric = "euclidean", linkage = "ward.D2", k = 4), filename = file
)
file.remove(file)
#> [1] TRUE
```
