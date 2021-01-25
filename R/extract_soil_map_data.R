#' @title Extract soil properties from Flemish soil map
#'
#' @description This function queries the
#' \href{https://www.geopunt.be/catalogus/datasetfolder/5c129f2d-4498-4bc3-8860-01cb2d513f8f}{the Flemish soil map}
#' attributes at a given coordinate,
#' by using the affiliated WFS service provided by DOV. The user can pick the
#' properties of interest. A full list of properties is available at \url{
#' https://www.dov.vlaanderen.be/geoserver/bodemkaart/bodemtypes/wfs?request=DescribeFeatureType}.
#' Coordinates should be given in the 'Belge 1972 / Belgian Lambert 72'
#' (\href{https://epsg.io/31370}{EPSG:31370}) coordinate reference system.
#' When outside the Flemish region, an NA value is given for each of the properties.
#' @param x_lam The numeric value of the X coordinate in CRS
#' 'Belge 1972 / Belgian Lambert 72' (EPSG:31370).
#' @param y_lam The numeric value of the Y coordinate in CRS
#' 'Belge 1972 / Belgian Lambert 72' (EPSG:31370).
#' @param properties_of_interest A vector or properties, as a subset of these
#' provided by the webservice. Default Bodemserie, Unibodemtype and Bodemtype.
#'
#' @return A data.frame with the properties as column headers.
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @export
#' @family download functions
#'
#' @examples
#' \dontrun{
#' library(inborutils)
#' extract_soil_map_data(173995.67, 212093.44)
#' extract_soil_map_data(173995.67, 212093.44,
#'                          properties_of_interest = c("Eenduidige_legende",
#'                                                     "Textuurklasse"))
#' }
extract_soil_map_data <- function(x_lam,
                                  y_lam,
                                  properties_of_interest) {
  if (missing(x_lam) | missing(y_lam)) stop("x_lam and y_lam needed")
  if (missing(properties_of_interest)) {
    properties_of_interest <- c("Bodemserie",
                                "Unibodemtype",
                                "Bodemtype")
    message("Defaulting to Bodemserie, Unibodemtype, Bodemtype. To avoid this message provide properties of interest in the function call.")
  }
  # dealing with point data inside a certain polygon of the soil map:
  wfs_bodemtypes <- "https://www.dov.vlaanderen.be/geoserver/bodemkaart/bodemtypes/wfs?"
  query = list(service = "WFS",
               request = "GetFeature",
               version = "1.1.0",
               typeName = "bodemkaart:bodemtypes",
               outputFormat = "json",
               propertyname = as.character(paste(properties_of_interest,
                                                 collapse = ",")),
               CRS = "EPSG:31370",
               CQL_FILTER = sprintf("INTERSECTS(geom,POINT(%s %s))",
                                    x_lam, y_lam)) # INTERSECT OPERATOR

  result <- GET(wfs_bodemtypes, query = query)
  parsed <- fromJSON(content(result, "text"))
  soil_info_df <- parsed$features
  # if else to catch cases where a point falls outside the map
  if (is.null(soil_info_df$properties)) {
    as.data.frame(
      matrix(rep(NA, length(properties_of_interest)),
             nrow = 1,
             dimnames = list(NULL, properties_of_interest)))
  } else {
    soil_info_df$properties[,-1]
  }

}
