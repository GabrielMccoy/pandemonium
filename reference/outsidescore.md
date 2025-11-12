# Using externally computed score values

Can be used as getScores input in pandemonium, to use score values that
are computed externally. Returns scores values as the score, and bins
computed as below, between or above the first and third quartile.

## Usage

``` r
outsidescore(scores, scoreName = NULL)
```

## Arguments

- scores:

  external scores to be passed to the app.

- scoreName:

  name for scores

## Value

named list containing scores for use in pandemonium

## Examples

``` r
if (FALSE) { # interactive()
pandemonium(df = Bikes$space1, space2 = Bikes$space2,
              getScore = outsidescore(Bikes$other$res,"Residual"))

}
```
