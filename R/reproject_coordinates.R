
#' reproject XY coordinates from dframe columns
#'
#' @param df data.frame with a x and y coordinate column
#' @param col_long (char) name of the x (longitude) column
#' @param col_lat (char) name of the y (latitude) column
#' @param crs_input projection string of class CRS-class defining the
#' current projection
#' @param crs_output projection string of class CRS-class defining the
#' projection to convert to
#'
#' @return data.frame with the same columns, but adapted coordinates for the
#' x and y column values
#'
#' @export
#' @importFrom sp SpatialPoints spTransform
#'
reproject_coordinates <- function(df, col_long, col_lat,
                                  crs_input, crs_output){
    df_spat <- SpatialPoints(df[c(col_long, col_lat)],
                             proj4string = crs_input)
    df_reproj <- spTransform(df_spat, crs_output)

    # rename the columns to have them in the data as well
    df[c(col_long, col_lat)] <- as.data.frame(df_reproj)[c(col_long, col_lat)]
    return(df)
}
