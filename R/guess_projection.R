
#' Take a dataset and plot in different projections on a leaflet map to
#' check the expected projection
#'
#' When retrieving a number of data-points, but the kind of projection is not
#' provided/known (for any reason), this utility plots the data for different
#' projections on a map to make comparison possible. The main function is
#' `guess_projection ` which creates the map with different options.
#' Custom projections can be provided by the user as well.
#'
#' For each of the given projections, the coordinates columns are given the
#' the specified projection. In a next step, these projections are converted to
#' wgs84 and plotted on a openstreetmap background with leaflet. The interactive
#' map provides the possibility to select/unselect specific layers.
#'
#' @param df data.frame with a x and y coordinate column
#' @param col_long (char) name of the x (longitude) column
#' @param col_lat (char) name of the y (latitude) column
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
#' @family GIS_utilities
#'
#' @examples
#'\dontrun{
#' guess_projection(data, "x", "y")
#' }
#'
#' @export
#' @importFrom leaflet leaflet addTiles addCircleMarkers addLayersControl
#' addLegend setView colorFactor layersControlOptions
#' @importFrom assertthat is.string is.flag noNA has_name
#' @importFrom dplyr sample_n
#' @importFrom sp CRS
#'
guess_projection <- function(df, col_long, col_lat, belgium = TRUE,
                     projections = c("epsg:4326", "epsg:31370",
                                     "epsg:28992", "epsg:32631",
                                     "epsg:3812", "epsg:3035")) {
    assert_that(inherits(df, "data.frame"))
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
        data_proj <- reproject_coordinates(df, col_long, col_lat,
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


