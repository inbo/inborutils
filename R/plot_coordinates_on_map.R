
#' plot x/y coordinates on a map, from given projection
#'
#' This function first converts the x/y coordinates of the given the data.frame
#' to GWS84 in order to map the data with leaflet
#'
#' @param df data.frame with a x and y coordinate column
#' @param col_long (char) name of the x (longitude) column
#' @param col_lat (char) name of the y (latitude) column
#' @param projection projection string of class CRS-class defining the
#' current projection
#'
#' @return leaflet map with coordinates added as dots
#'
#' @export
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
plot_coordinates_on_map <- function(df, col_long, col_lat, projection) {
    data_proj <- reproject_coordinates(df, col_long, col_lat, projection,
                                       CRS("+init=epsg:4326"))
    mapt <- leaflet(data = data_proj) %>%
        addTiles() %>%
        addCircleMarkers(data_proj[[col_long]], data_proj[[col_lat]],
                         stroke = FALSE)
    return(mapt)
}
