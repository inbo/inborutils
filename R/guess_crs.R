#' Take a dataset and plot in different coordinate reference systems (CRS) on a
#' leaflet map to check the expected CRS
#'
#' When retrieving a number of data-points, but the kind of CRS is not
#' provided/known (for any reason), this utility plots the data for different
#' CRS on a map to make comparison possible. The main function is
#' `guess_crs` which creates the map with different options.
#' Custom projections can be provided by the user as well.
#'
#' For each of the given CRS, the coordinates columns are given the
#' the specified CRS In a next step, these CRS are converted to
#' wgs84 and plotted on a openstreetmap background with leaflet. The interactive
#' map provides the possibility to select/unselect specific layers.
#'
#' @param df data.frame with a x and y coordinate column
#' @param col_x (char) name of the x (longitude) column
#' @param col_y (char) name of the y (latitude) column
#' @param belgium (bool) If TRUE, coordinates are expected to be in Belgium
#' @param crs_try (list) EPSG codes of the different CRS to evaluate
#' By default, the following six CRS are added to the map:
#' \itemize{
#'  \item{"EPSG:4326"}{WGS 84,
#'  https://spatialreference.org/ref/epsg/wgs-84/}
#'  \item{"EPSG:31370"}{Belge 1972/Belgian Lambert 72,
#'  https://spatialreference.org/ref/epsg/31370/}
#'  \item{"EPSG:28992"}{Amersfoort/Rijksdriehoek nieuw,
#'  https://spatialreference.org/ref/epsg/28992/}
#'  \item{"EPSG:32631"}{WGS 84 / UTM zone 31N,
#'  https://spatialreference.org/ref/epsg/32631/}
#'  \item{"EPSG:3812"}{ETRS89/Belgian Lambert 2008,
#'  https://spatialreference.org/ref/epsg/3812/}
#'  \item{"EPSG:3035"}{ETRS89 / ETRS-LAEA,
#'  https://spatialreference.org/ref/epsg/3035/}
#' }
#'
#' @family GIS_utilities
#' @seealso crsuggest::guess_crs() for a (better) alternative
#'
#' @examples
#' \dontrun{
#' guess_crs(data, "x", "y")
#' }
#'
#' @export
#' @importFrom leaflet leaflet addTiles addCircleMarkers addLayersControl
#' addLegend setView colorFactor layersControlOptions
#' @importFrom assertthat is.string is.flag noNA has_name
#' @importFrom dplyr slice_sample
#' @importFrom sf st_crs
#'
guess_crs <- function(df, col_x, col_y, belgium = TRUE,
                      crs_try = c(
                        "EPSG:4326", "EPSG:31370",
                        "EPSG:28992", "EPSG:32631",
                        "EPSG:3812", "EPSG:3035"
                      )) {
  assert_that(inherits(df, "data.frame"))
  assert_that(is.string(col_x))
  assert_that(is.string(col_y))
  assert_that(has_name(df, col_x))
  assert_that(has_name(df, col_y))
  assert_that(is.flag(belgium))
  assert_that(noNA(belgium))
  assert_that(is.character(crs_try))
  assert_that(noNA(crs_try))
  # Create a color palette for the different projections
  color_pal <- colorFactor(
    palette = "RdYlBu",
    levels = factor(crs_try)
  )

  # Limit the number of dots to plot to 200
  max_dots <- 200
  if (nrow(df) > max_dots) {
    df <- slice_sample(df, n = max_dots)
    print("Number of dots on map limited to 200 random sampled records")
  }

  # set up a leaflet map and add background
  mapt <- leaflet() %>%
    addTiles()

  # add the circle markers for each projection on the map
  for (prj in crs_try) {
    message(prj)
    data_proj <- transform_coordinates(
      df, col_x, col_y,
      st_crs(prj),
      st_crs(4326)
    )

    mapt <- addCircleMarkers(mapt, data_proj[[col_x]],
      data_proj[[col_y]],
      stroke = FALSE,
      color = color_pal(prj), opacity = 1,
      group = prj
    )
  }

  # create a menu to (un)select specific layers
  mapt <- addLayersControl(mapt,
    overlayGroups = crs_try,
    options = layersControlOptions(collapsed = FALSE)
  )

  # add a color legend for the individual projections
  mapt <- addLegend(mapt, pal = color_pal, values = crs_try, opacity = 1)

  # Put the center of the map on Belgium when Belgian data points are expected
  if (belgium) {
    mapt <- setView(mapt, lng = 4.5, lat = 50.5, zoom = 6)
  }
  return(mapt)
}
