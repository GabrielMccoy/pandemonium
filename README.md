
# pandemonium

<!-- badges: start -->

<img src="vignettes/Images/logo.png" height="300px" align="right"/>
<!-- badges: end -->

pandemonium is a package that performs high dimensional cluster analysis
within a shiny GUI. pandemonium performs data preparation, clustering
and visualisation within a dynamic GUI. With interactive methods
allowing the user to change settings all without having to to leave the
GUI.

## Installation

You can install the development version of pandemoniumedits from
[GitHub](https://github.com/GabrielMccoy/pandemonium) with:

``` r
# install.packages("pak")
pak::pak("GabrielMccoy/pandemonium")
```

## Getting Started

A good example of how to use the app can be shown in he `Bikes` data
example. To load the app with this data the following can be used.

``` r
library(pandemonium)
pandemonium(df = Bikes$space1, space2 = Bikes$space2, 
                getScore = outsidescore(Bikes$other$res,"Residual"))
```

By loading the app with the clustering space in df and the linked space
in space2 the data is automatically selected in the data page. From here
pressing load app will load into the analysis tab.

Within the analysis tab you can explore different visuals and their
options in each of the enclosed tabs.
