#' @title Query header information from INBOVEG
#'
#' @description  `r lifecycle::badge('defunct')`
#' This function queries the INBOVEG database for header
#' information (metadata for a vegetation-recording or releve) for one or more surveys and
#' the recorder type. See the examples for how to get information for all surveys.
#'
#' @param survey_name A character string or a character vector
#' giving the name or names of the survey(s) for which you want to extract header information.
#' If missing, all surveys are returned.
#' @param rec_type A character vector giving the name of record type for which
#' you want to extract header information e.g. 'Classic', 'Classic-emmer',
#' 'Classic-ketting', 'BioHab', 'ABS'. If missing, all recording types are returned.
#' @param connection dbconnection with the database 'Cydonia'
#' on the inbo-sql07-prd server
#' @param multiple If TRUE, survey_name can take a character vector with
#' multiple survey names that must match exactly. If FALSE (the default),
#' survey_name must be a single character string (one survey name) that can
#' include wildcards to allow partial matches
#' @param collect If FALSE (the default), a remote tbl object is returned.
#' This is like a reference to the result of the query but the full result of
#' the query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables RecordingGivid, Name, UserReference, Observer, LocationCode,
#' Latitude, Longitude, Area (in m2), Length (in cm), Width (in cm), VagueDateType,
#' VagueDateBegin, VagueDateEnd, SurveyId, RecTypeID.
#'
#' @family inboveg
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get header information from a specific survey and a specific recording type
#' # and collect the data
#' header_info <- inboveg_header(con, survey_name = "OudeLanden_1979",
#' rec_type = "Classic", collect = TRUE)
#'
#' # get header information from several specific surveys
#' header_severalsurveys <- inboveg_header(con, survey_name =
#' c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE)
#'
#' # get header information of all surveys,  don't collect the data
#' all_header_info <- inboveg_header(con)
#'
#' # close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }
#'
#' @name inboveg_header-deprecated
#' @usage inboveg_header(connection, survey_name, rec_type, multiple = FALSE,
#'   collect = FALSE)
#' @seealso \code{\link{inborutils-deprecated}}
#' @keywords internal
NULL

#' @rdname inborutils-deprecated
#' @section inboveg_header:
#' For \code{inboveg_header}, use [inbodb::inboveg_header()](https://inbo.github.io/inbodb/reference/get_inboveg_header.html)
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql
#'
#' @export


inboveg_header <- function(connection,
                           survey_name,
                           rec_type,
                           multiple = FALSE,
                           collect = FALSE) {

  .Defunct("inbodb::get_inboveg_classification()", package = "inborutils")
}
