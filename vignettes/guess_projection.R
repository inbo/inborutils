## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)

## ------------------------------------------------------------------------
library(inborutils)

## ----fig.width=7, message=FALSE------------------------------------------
# my_dataframe has the columns "longitude" and "latitude"
guess_projection(coordinate_example, "longitude", "latitude")

## ----fig.width=7, message=FALSE------------------------------------------
guess_projection(coordinate_example, "longitude", "latitude", belgium = TRUE, 
                 projections = c("epsg:2000", "epsg:2805", "epsg:4326"))

