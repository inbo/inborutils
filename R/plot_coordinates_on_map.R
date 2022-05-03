#' Plot x/y coordinates on a map
#'
#' This function plots x/y coordinates from a data.frame on a (leaflet) map. The
#' coordinates are first converted to WGS84 in order to map them correctly on
#' the leaflet map (@seealso [reproject_coordinates()]). To do this, the
#' original Coordinate Reference System (CRS) is asked.
#'
#' @param df A data.frame with a x and y coordinate columns.
#' @param col_long,col_lat Column names or positions of the x (longitude) and y
#'   (latitude) column. They are passed to \code{\link[tidyselect]{vars_pull}}.
#'   These arguments are passed by expression and support
#'   \code{\link{quasiquotation}} (you can unquote column names or column
#'   positions).
#' @param projection Projection string of class CRS-class (\code{sp} objects) or
#'   crs-class (\code{sf} objects) defining the current projection.
#' @param ... Additional arguments passed on to
#'   \code{\link{addCircleMarkers}} to customize points.
#'
#' @return Leaflet map with coordinates added as dots.
#'
#' @family GIS_utilities
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(sp)
#' data_pts <- data.frame(
#'   id = c(1, 2, 3),
#'   lat = c(51.23031, 50.76931, 50.21439),
#'   lon = c(5.083980, 3.829593, 3.289044),
#'   stringsAsFactors = FALSE
#' )
#'
#' # projection is of class CRS-class (sp)
#' proj_crs_sp <- CRS("+init=epsg:4269")
#' plot_coordinates_on_map(data_pts, "lon", "lat", proj_crs_sp)
#'
#' # projection is of class crs-class (sf)
#' proj_crs_sf <- st_crs("+init=epsg:4269")
#' plot_coordinates_on_map(data_pts, "lon", "lat", proj_crs_sf)
#'
#' # customize circles
#' plot_coordinates_on_map(data_pts, "lon", "lat", proj_crs_sf,
#'   radius = 5, color = "red", stroke = FALSE, fillOpacity = 0.75)
#' }
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom sf st_crs
#' @importFrom dplyr %>%
#' @importFrom tidyselect vars_pull enquo
#' @importFrom rlang !!
#' @importFrom purrr map_lgl
plot_coordinates_on_map <- function(df, col_long, col_lat, projection, ...) {

  assert_that(is.data.frame(df))
  assert_that(
    class(projection) %in% c("CRS", "crs"),
    msg = "Input projection should be an object of class \"CRS\" or \"crs\"."
  )

  col_long <- vars_pull(names(df), !! enquo(col_long))
  col_lat <- vars_pull(names(df), !! enquo(col_lat))

  assert_that(isTRUE(all(map_lgl(df[[col_long]],  ~ is.numeric(.)))),
              msg = "x coordinates (longitude) should be numbers.")
  assert_that(isTRUE(all(map_lgl(df[[col_lat]],  ~ is.numeric(.)))),
              msg = "y coordinates (latitude) should be numbers.")
  data_proj <- reproject_coordinates(df, col_long, col_lat, projection,
                                     st_crs("+init=epsg:4326"))

  mapt <- leaflet(data = data_proj) %>%
      addTiles() %>%
      addCircleMarkers(data_proj[[col_long]], data_proj[[col_lat]], ...)
  return(mapt)
}
