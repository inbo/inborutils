#' Transform x-y coordinates from `data.frame` columns.
#'
#'
#' This function extracts x-y coordinates from a `data.frame` by means of the
#' given coordinate reference system (`CRS`), transforms them to the new `CRS`
#' and assign them back to the `data.frame` columns.
#'
#' @param df A `data.frame` with a x and y coordinate column.
#' @param col_x,col_y Column names or positions of the x and y
#'    column. They are passed to \code{\link[tidyselect]{vars_pull}}.
#'   These arguments are passed by expression and support
#'   \code{\link{quasiquotation}}
#'   (you can unquote column names or column positions).
#' @param crs_input Projection string of class `CRS-class`
#' (\code{sp} compatible) or `crs-class` (\code{sf} compatible) defining the
#' current `CRS`.
#' @param crs_output Projection string of class `CRS-class`
#' (\code{sp} compatible) or `crs-class` (\code{sf} compatible) defining the
#' `CRS` to convert to.
#'
#' @return A `data.frame` with the same columns, but transformed coordinates for
#'  the x and y column values.
#'
#' @family GIS_utilities
#'
#' @examples
#' library(sf)
#' data_pts <- data.frame(
#'   id = c(1, 2),
#'   lat = c(51.23031, 50.76931),
#'   lon = c(5.083980, 3.829593),
#'   stringsAsFactors = FALSE
#' )
#'
#' # CRS-class (use sp package)
#' if (requireNamespace("sp")) {
#'   sp_crs1 <- sp::CRS("+init=epsg:4269")
#'   sp_crs2 <- sp::CRS("+init=epsg:3857")
#'   transform_coordinates(data_pts,
#'     col_x = "lon", col_y = "lat",
#'     crs_input = sp_crs1, crs_output = sp_crs2
#'   )
#' }
#' # crs-class (use sf package)
#' sf_crs1 <- st_crs(4269)
#' sf_crs2 <- st_crs(3857)
#' transform_coordinates(data_pts,
#'   col_x = "lon", col_y = "lat",
#'   crs_input = sf_crs1, crs_output = sf_crs2
#' )
#'
#' if (requireNamespace("sp")) {
#'   # input projection is CRS-class (sp) and output projection crs-class (sf)
#'   transform_coordinates(data_pts,
#'     col_x = "lon", col_y = "lat",
#'     crs_input = sp_crs1, crs_output = sf_crs2
#'   )
#'   # input projection is crs-class (spf and output projection CRS-class (sp)
#'   transform_coordinates(data_pts,
#'     col_x = "lon", col_y = "lat",
#'     crs_input = sf_crs1, crs_output = sp_crs2
#'   )
#' }
#'
#' # use names (character) of x-y columns
#' transform_coordinates(data_pts,
#'   col_x = "lon", col_y = "lat",
#'   crs_input = sf_crs1, crs_output = sf_crs2
#' )
#' # use NSE of x-y columns
#' transform_coordinates(data_pts,
#'   col_x = lon, col_y = lat,
#'   crs_input = sf_crs1, crs_output = sf_crs2
#' )
#' # use position of x-y columns
#' transform_coordinates(data_pts,
#'   col_x = 3, col_y = 2,
#'   crs_input = sf_crs1, crs_output = sf_crs2
#' )
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_coordinates st_as_sf st_transform st_crs
#' @importFrom tidyselect vars_pull enquo
#' @importFrom rlang !!
#' @importFrom purrr map_lgl
#'
transform_coordinates <- function(df, col_x, col_y,
                                  crs_input, crs_output) {
  assert_that(is.data.frame(df))
  assert_that(
    class(crs_input) %in% c("CRS", "crs"),
    msg = "Input CRS should be an object of class \"CRS\" or \"crs\"."
  )
  assert_that(
    class(crs_output) %in% c("CRS", "crs"),
    msg = "Output CRS should be an object of class \"CRS\" or \"crs\"."
  )

  col_x <- vars_pull(names(df), !!enquo(col_x))
  col_y <- vars_pull(names(df), !!enquo(col_y))

  assert_that(isTRUE(all(map_lgl(df[[col_x]], ~ is.numeric(.)))),
    msg = "x coordinates (longitude) should be numbers."
  )
  assert_that(isTRUE(all(map_lgl(df[[col_y]], ~ is.numeric(.)))),
    msg = "y coordinates (latitude) should be numbers."
  )

  crs_input <- st_crs(crs_input)
  crs_output <- st_crs(crs_output)

  if (crs_input == crs_output) {
    warning(paste(
      "Input CRS equal to output CRS",
      "No transform performed."
    ))
    return(df)
  }

  df_spat <- st_as_sf(df[c(col_x, col_y)],
    coords = c(col_x, col_y),
    crs = crs_input
  )
  df_reproj <- st_transform(df_spat, crs_output)

  df[c(col_x, col_y)] <- as.data.frame(st_coordinates(df_reproj))
  return(df)
}

#' Transform x-y coordinates from data.frame columns.
#'
#'
#' This function extracts x-y coordinates from a data.frame by means of the
#' given coordinate reference system (CRS), transforms them to the new CRS and
#' assign them back to the data.frame columns.
#'
#' @param df A data.frame with a x and y coordinate column.
#' @param col_long,col_lat Column names or positions of the x and y
#'    column. They are passed to \code{\link[tidyselect]{vars_pull}}.
#'   These arguments are passed by expression and support
#'   \code{\link{quasiquotation}}
#'   (you can unquote column names or column positions).
#' @param crs_input Projection string of class CRS-class (\code{sp} compatible)
#'   or crs-class (\code{sf} compatible) defining the current CRS.
#' @param crs_output Projection string of class CRS-class (\code{sp} compatible)
#'   or crs-class (\code{sf} compatible) defining the CRS to convert to.
#'
#' @return A data.frame with the same columns, but transformed coordinates for
#'  the x and y column values.
#'
#' @family GIS_utilities
reproject_coordinates <- function(df, col_long, col_lat,
                                  crs_input, crs_output) {
  .Deprecated("transform_coordinates")
  return(
    transform_coordinates(
      df = df,
      col_x = col_long,
      col_y = col_lat,
      crs_input = crs_input,
      crs_output = crs_output
    )
  )
}
