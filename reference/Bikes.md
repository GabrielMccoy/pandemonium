# Bike sharing data with model information

The dataset contains daily counts of bikes rented with corresponding
weather and seasonal information. The data is provided by Hadi Fanaee-T
and available from https://doi.org/10.24432/C5W894. Additionally, model
information from a single hidden layer neural network with eight nodes
in the hidden layer has been added: the values of the activations for
all observations (variables A1 to A8 in the cluster space) and the model
prediction (pred) and residual (res) in the other variables.

## Usage

``` r
Bikes
```

## Format

a list of 4 dataframes

- df:

  dataframe 731 obs of 18 variables containing the entire bikes data set

- space1:

  dataframe 731 obs of 8 variables (cluster space)

- space2:

  dataframe 731 obs of 6 variables (linked space, predictors used in the
  model)

- other:

  dataframe 731 obs of 4 variables (other variables, including observed
  and predicted counts)
