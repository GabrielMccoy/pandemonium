# Raw coordinates

Returns the input data frame. This is used when other coordinate
computations fail. In general, scaling of the inputs is recommended
before clustering.

## Usage

``` r
rawCoords(df, ...)
```

## Arguments

- df:

  data frame

- ...:

  other expected values of getCoords

## Value

matrix with coordinate representation of all points

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
head(rawCoords(Bikes$space2))
#>   yr     temp weathersit    atemp      hum windspeed
#> 1  0 0.344167          2 0.363625 0.805833 0.1604460
#> 2  0 0.363478          2 0.353739 0.696087 0.2485390
#> 3  0 0.196364          1 0.189405 0.437273 0.2483090
#> 4  0 0.200000          1 0.212122 0.590435 0.1602960
#> 5  0 0.226957          1 0.229270 0.436957 0.1869000
#> 6  0 0.204348          1 0.233209 0.518261 0.0895652

```
