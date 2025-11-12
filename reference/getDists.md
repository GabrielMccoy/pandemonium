# Compute distances between all points

Compute distances between all points

## Usage

``` r
getDists(coord, metric, user_dist = NULL)
```

## Arguments

- coord:

  matrix with coordinate representation of all points

- metric:

  name of distance metric to be used in stats::dist

- user_dist:

  user distance returned with metric=user

## Value

distances between all points

## Examples

``` r
getDists(Bikes$space1[1:5,],"euclidean")
#>           1         2         3         4
#> 2 0.9652271                              
#> 3 2.8380077 2.1357739                    
#> 4 2.5636606 1.8901896 0.3659190          
#> 5 2.7035401 1.9682619 0.2672703 0.2627526
getDists(Bikes$space1[1:5,],"maximum")
#>           1         2         3         4
#> 2 0.6336683                              
#> 3 1.6802070 1.4197401                    
#> 4 1.5152043 1.1647712 0.2549689          
#> 5 1.6802070 1.1524698 0.2672703 0.2041120
```
