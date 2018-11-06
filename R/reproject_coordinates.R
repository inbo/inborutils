#' Reproject x-y coordinates from data.frame columns.
#'
#'
#' This function extracts x-y coordinates from a data.frame by means of the
#' given coordinate reference system (CRS), converts them to the new CRS and
#' assign them back to the data.frame columns.
#'
#' @param df A data.frame with a x and y coordinate column.
#' @param col_long,col_lat Column names or positions of the x (longitude) and y
#'   (latitude) column. They are passed to \code{\link[tidyselect]{vars_pull}}.
#'   These arguments are passed by expression and support
#'   \code{\link[rlang]{quasiquotation}} (you can unquote column names or column
#'   positions).
#' @param crs_input Projection string of class CRS-class (\code{sp} compatible)
#'   or crs-class (\code{sf} compatible) defining the current projection.
#' @param crs_output Projection string of class CRS-class (\code{sp} compatible)
#'   or crs-class (\code{sf} compatible) defining the projection to convert to.
#'
#' @return A data.frame with the same columns, but adapted coordinates for the x
#'   and y column values.
#'
#' @examples
#' library(sp)
#' library(sf)
#' data_pts <- data.frame(
#'   id = c(1, 2),
#'   lat = c(51.23031, 50.76931),
#'   lon = c(5.083980, 3.829593),
#'   stringsAsFactors = FALSE
#' )
#'
#' # CRS-class (use sp package)
#' sp_crs1 <- CRS("+init=epsg:4269")
#' sp_crs2 <- CRS("+init=epsg:3857")
#' reproject_coordinates(data_pts, col_long = "lon", col_lat = "lat",
#'   crs_input = sp_crs1, crs_output = sp_crs2)
#' # crs-class (use sf package)
#' sf_crs1 <- st_crs("+init=epsg:4269")
#' sf_crs2 <- st_crs("+init=epsg:3857")
#' reproject_coordinates(data_pts, col_long = "lon", col_lat = "lat",
#'   crs_input = sf_crs1, crs_output = sf_crs2)
#' # input projection is CRS-class (sp) and output projection crs-class (sf)
#' reproject_coordinates(data_pts, col_long = "lon", col_lat = "lat",
#'   crs_input = sp_crs1, crs_output = sf_crs2)
#' # input projection is crs-class (spf and output projection CRS-class (sp)
#' reproject_coordinates(data_pts, col_long = "lon", col_lat = "lat",
#'   crs_input = sf_crs1, crs_output = sp_crs2)
#'
#' # use names (character) of x-y columns
#' reproject_coordinates(data_pts, col_long = "lon", col_lat = "lat",
#'   crs_input = sf_crs1, crs_output = sf_crs2)
#' # use NSE of x-y columns
#' reproject_coordinates(data_pts, col_long = lon, col_lat = lat,
#'   crs_input = sf_crs1, crs_output = sf_crs2)
#' # use position of x-y columns
#' reproject_coordinates(data_pts, col_long = 3, col_lat = 2,
#'   crs_input = sf_crs1, crs_output = sf_crs2)
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom sp SpatialPoints spTransform
#' @importFrom sf st_coordinates st_as_sf st_transform
#' @importFrom methods as
#' @importFrom tidyselect vars_pull enquo
#' @importFrom rlang !!
#' @importFrom purrr map_lgl
#'
reproject_coordinates <- function(df, col_long, col_lat,
                                  crs_input, crs_output){
  assert_that(is.data.frame(df))
  assert_that(
    class(crs_input) %in% c("CRS", "crs"),
    msg = "Input projection should be an object of class \"CRS\" or \"crs\"."
  )
  assert_that(
    class(crs_output) %in% c("CRS", "crs"),
    msg = "Output projection should be an object of class \"CRS\" or \"crs\"."
  )

  col_long <- vars_pull(names(df), !! enquo(col_long))
  col_lat <- vars_pull(names(df), !! enquo(col_lat))

  assert_that(isTRUE(all(map_lgl(df[[col_long]],  ~ is.numeric(.)))),
              msg = "x coordinates (longitude) should be numbers.")
  assert_that(isTRUE(all(map_lgl(df[[col_lat]],  ~ is.numeric(.)))),
              msg = "y coordinates (latitude) should be numbers.")

  if ((all(c(class(crs_input), class(crs_output)) == "CRS")) |
       all(c(class(crs_input), class(crs_output)) == "crs")) {
    if (class(crs_input) == "CRS") {
      if (crs_input@projargs == crs_output@projargs) {
        warning(paste("Input projection equal to output projection.",
                      "No reprojection performed."))
        return(df)
      }
    } else {
      if (crs_input == crs_output) {
        warning(paste("Input projection equal to output projection.",
                      "No reprojection performed."))
        return(df)
      }
    }
  }

  if (class(crs_input) == "CRS") {
    df_spat <- SpatialPoints(df[c(col_long, col_lat)],
                             proj4string = crs_input)
    if (class(crs_output) == "CRS") {
      df_reproj <- spTransform(df_spat, crs_output)
    } else {
      df_spat <- st_as_sf(df_spat, crs = crs_input)
      df_reproj <- st_transform(df_spat, crs_output)
    }
  } else{
    df_spat <- st_as_sf(df[c(col_long, col_lat)],
                        coords = c(col_long, col_lat),
                        crs = crs_input)
    if (class(crs_output) == "CRS") {
      df_spat <- as(df_spat, "Spatial")
      # convert from SpatialPointDataframe to SpatialPoints
      df_spat <- as(df_spat, "SpatialPoints")
      colnames(df_spat@coords) <- c(col_long, col_lat)

      df_reproj <- spTransform(df_spat, crs_output)
    } else {
      df_reproj <- st_transform(df_spat, crs_output)
    }
  }

  if (class(crs_output) == "CRS") {
    df[c(col_long, col_lat)] <- as.data.frame(df_reproj)[c(col_long, col_lat)]
  } else {
    df[c(col_long, col_lat)] <- st_coordinates(df_reproj)
  }
  return(df)
}
