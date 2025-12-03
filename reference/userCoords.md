# User defined coordinate function

Allows the use of externally calculated coordinates in the app. Can only
be used when variables are not reassigned between the two spaces.

## Usage

``` r
userCoords(user_coords)
```

## Arguments

- user_coords:

  coordinate matrix the size of the space it will be used on

## Value

function that returns the user defined coordinates user_coords

## Details

Externally calculated coordinates can be used through userCoords or as
input data with rawCoords used as the coordinate function. The use of
userCoords over rawCoords is in the treatment of input data. As
pandemonium displays the input data in many plots the use of coordinates
as input data will result in these plots being less meaningful for
interpretation. Use userCoords where coordinates are necessary to
calculate distances but interpretation from plots of clustering space is
necessary.

## Examples

``` r
if (FALSE) { # interactive()
pandemonium(
  df = Bikes$space1, space2 = Bikes$space2,
  coords = list(normalised = normCoords, space2 = userCoords(Bikes$space2))
)
}
```
