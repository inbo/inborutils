#
# INBO R-utils
# Author: S. Van Hoey
#
#
# Projection checker
#
# When retrieving a number of data-points, but the kind of projection is not
# provided/known (for any reason), this utility plots the data for different
# projections on a map to make comparison possible. The main function is
# `guess_projection ` which creates the map with different options.
#
# Custom projections can be provided by the user as well.
####

library(sp)
library(rgdal)
library(dplyr)
library(leaflet) # used for easy background maps
library(assertthat)

#' reproject XY coordinates from dframe columns
#'
#' @param df data.frame with a x and y coordinate column
#' @param col_long (char) name of the x (longitude) column
#' @param col_lang (char) name of the y (latitude) column
#' @param project_input projection string of class CRS-class defining the
#' current projection
#' @param project_output projection string of class CRS-class defining the
#' projection to convert to
#'
#' @return data.frame with the same columns, but adapted coordinates for the
#' x and y column values
#'
reproject_points <- function(df, col_long, col_lat,
                             project_input, project_output){
    df_spat <- SpatialPoints(df[c(col_long, col_lat)],
                        proj4string = project_input)
    df_reproj <- spTransform(df_spat, project_output)
    df[c(col_long, col_lat)] <- as.data.frame(df_reproj)[c(col_long, col_lat)]
    # rename the columns to have them in the data as well
    return(df)
}


#' plot x/y coordinates on a map, from given projection
#'
#' This function first converts the x/y coordinates of the given the data.frame
#' to GWS84 in order to map the data with leaflet
#'
#' @param df data.frame with a x and y coordinate column
#' @param col_long (char) name of the x (longitude) column
#' @param col_lang (char) name of the y (latitude) column
#' @param projection projection string of class CRS-class defining the
#' current projection
#'
#' @return leaflet map with coordinates added as dots
#'
#' @import leaflet
plot_on_map <- function(df, col_long, col_lat, projection){
    data_proj <- reproject_points(df, col_long, col_lat,
                                  projection,
                                  CRS("+init=epsg:4326"))
    mapt <- leaflet(data = data_proj) %>%
        addTiles() %>%
        addCircleMarkers(data_proj[[col_long]], data_proj[[col_lat]],
                         stroke = FALSE)
    return(mapt)
}


#' Take a dataset and plot in different projections on a leaflet map to
#' check the expected projection
#'
#' For each of the given projections, the coordinates columns are given the
#' the specified projection. In a next step, these projections are converted to
#' wgs84 and plotted on a openstreetmap background with leaflet. The interactive
#' map provides the possibility to select/unselect specific layers.
#'
#' @param df data.frame with a x and y coordinate column
#' @param col_long (char) name of the x (longitude) column
#' @param col_lang (char) name of the y (latitude) column
#' @param belgium (bool) If TRUE, coordinates are expected to be in Belgium
#' @param projections (list) epsg codes of the different projections to evaluate
#' By default, the following six projections are added to the map:
#' \itemize{
#'  \item{"epsg:4326"}{WGS 84,
#'  http://spatialreference.org/ref/epsg/wgs-84/}
#'  \item{"epsg:31370"}{Belge 1972/Belgian Lambert 72,
#'  http://spatialreference.org/ref/epsg/31370/}
#'  \item{"epsg:28992"}{Amersfoort/Rijksdriehoek nieuw,
#'  http://spatialreference.org/ref/epsg/28992/}
#'  \item{"epsg:32631"}{WGS 84 / UTM zone 31N,
#'  http://spatialreference.org/ref/epsg/32631/}
#'  \item{"epsg:3812"}{ETRS89/Belgian Lambert 2008,
#'  http://spatialreference.org/ref/epsg/3812/}
#'  \item{"epsg:3035"}{ETRS89 / ETRS-LAEA,
#'  http://spatialreference.org/ref/epsg/3035/}
#' }
#'
#' @examples
#' data <- read.csv("data/example2.csv")
#' guess_projection(data, "x", "y")
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers addLayerControls addLegend setView
#' @importFrom assertthat is.string is.flag noNA
#' @importFrom dplyr sample_n
guess_projection <- function(df, col_long, col_lat, belgium = TRUE,
                     projections=c("epsg:4326", "epsg:31370",
                                   "epsg:28992", "epsg:32631",
                                   "epsg:3812", "epsg:3035")){
    assert_that(inhertis(df, "data.frame"))
    assert_that(is.string(col_long))
    assert_that(is.string(col_lat))
    assert_that(has_name(df, col_long))
    assert_that(has_name(df, col_lat))
    assert_that(is.flag(belgium))
    assert_that(noNA(belgium))
    assert_that(is.character(projections))
    assert_that(noNA(projections))
    # Create a color palette for the different projections
    color_pal <- colorFactor(palette = "RdYlBu",
                             levels = factor(projections))

    # Limit the number of dots to plot to 200
    max_dots <- 200
    if (nrow(df) > max_dots) {
        df <- dplyr::sample_n(df, max_dots)
        print("Number of dots on map limited to 200 random sampled records")
    }

    # set up a leaflet map and add background
    mapt <- leaflet() %>%
        addTiles()

    # add the circle markers for each projection on the map
    for (prj in projections) {
        message(prj)
        data_proj <- reproject_points(df, col_long, col_lat,
                                      CRS(paste("+init=", prj, sep = "")),
                                      CRS("+init=epsg:4326"))

        mapt <- addCircleMarkers(mapt, data_proj[[col_long]],
                                 data_proj[[col_lat]], stroke = FALSE,
                                 color = color_pal(prj), opacity = 1,
                                 group = prj)
    }

    # create a menu to (un)select specific layers
    mapt <- addLayersControl(mapt, overlayGroups = projections,
                             options = layersControlOptions(collapsed = FALSE))

    # add a color legend for the individual projections
    mapt <- addLegend(mapt, pal = color_pal, values = projections, opacity = 1)

    # Put the center of the map on Belgium when Belgian data points are expected
    if (belgium == TRUE) {
        mapt <- setView(mapt, lng = 4.5, lat = 50.5, zoom = 6)
        }
    return(mapt)
}


