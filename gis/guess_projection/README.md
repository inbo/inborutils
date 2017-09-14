
# Check coordinate system of point data

## Introduction

When retrieving a number of data-points, but the kind of projection is not provided/known (for any reason), this utility plots the data for different projections on a map to make comparison possible. The main function is `guess_projection ` which creates the map with different options. The `reproject_points` function can be useful as well, as it supports reprojection of coordinate columns in a `data.frame`.


## Aim

The aim is to quickly check the coordinate system by plotting the coordinates on a leaflet map with a list of given coordinate systems


## Functionality

### Dependencies

In order to run the functionalities, some R packags need to be installed. The set of required libraries is given on top of the R-script:

```R
library(sp)
library(rgdal)
library(dplyr)
library(leaflet)
library(assertthat)
```

Installing these packages can be done with the following command:

```
install.packages(c('sp', 'rgdal', 'dplyr', 'leaflet', 'assertthat'))
```

### Projection function

The main R function is called `guess_projection`. As this is not (yet) available as an R package, making the function available can be done by sourcing the file (either in another script or in the R Console):

```
source('guess_projection.R')
```

By doing so, the function `guess_projection` can be used. The `guess_projection` function requires as minimal  inputs 
1.  a `data.frame`
2.  the longitude column name
3.  the latitude column name. 

For example
```
# my_dataframe has the columns 'longitude' and 'latitude'
guess_projection(my_dataframe, 'longitude', 'latitude')
```

Furthermore, the optional argument `belgium` is a boolean value (TRUE/FALSE) defining if the data is expectd to be in Belgium. Finally, the `epsg` codes of the corresponding coordinate systems to check can be customised. 

By default, the following CRS-systems are tested:
  * "epsg:4326"
  * "epsg:31370"
  * "epsg:28992"
  * "epsg:32631"
  * "epsg:3812"
  * "epsg:3035"

which can be overridden by adapting the `projections` argument:
```
guess_projection <- function(my_dataframe, 'longitude', 'latitude', 								belgium = TRUE, projections=c("EPSG:2000", "EPSG:2001"))
```
Check the [spatial reference](http://spatialreference.org) for the available codes.