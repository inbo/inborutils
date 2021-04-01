#' Example data.frame with species name column
#'
#' A dataset containing 3 taxa to be matched with GBIF Taxonomy Backbone. The
#' variable are as follows:
#'
#' @format A data frame with 3 rows and 3 variables
#' \itemize{
#'   \item {speciesName: name of the species}
#'   \item {kingdom: kingdom to which the species belongs}
#'   \item {euConcernStatus: level of concern according to EU directives}
#' }
#'
#' @docType data
#' @keywords datasets
#' @family datasets
#' @name species_example
#' @usage species_example
NULL


#' Example data.frame with coordinates
#'
#' A dataset containing 52 coordinates as latitude and longitude
#'
#' @format A data frame with 52 rows and 3 variables:
#' \itemize{
#'   \item{id: resource identifier}
#'   \item{latitude: Latitude of the coordinates}
#'   \item{longitude: Longitude of the coordinates}
#' }
#'
#' @docType data
#' @keywords datasets
#' @family datasets
#' @name coordinate_example
#' @usage coordinate_example
NULL


#' Example data.frame with KNMI downloaded data
#'
#' A dataset containing the rainfall from January 1st till February 1st for
#' Vlissingen and Westdorpe, as downloaded from KNMI
#'
#' @format A data frame with 1536 rows and 9 variables:
#' \itemize{
#'   \item{value: measured value}
#'   \item{datetime: datetime of the measurement}
#'   \item{unit: unit (mm)}
#'   \item{variable_name: precipitation}
#'   \item{longitude: coordinate}
#'   \item{latitude: coordinate}
#'   \item{location_name: station name}
#'   \item{source_filename: filename from which the data was read}
#'   \item{quality_code: empty string as KNMI does not provide this}
#' }
#'
#' @docType data
#' @keywords datasets
#' @family datasets
#' @name rain_knmi_2012
#' @usage rain_knmi_2012
NULL
