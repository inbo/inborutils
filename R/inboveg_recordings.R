#' @title Query recording (releve) information from INBOVEG
#'
#' @description `r lifecycle::badge('deprecated')`
#' This function queries the INBOVEG database for
#' releve information (which species were recorded in which plots and in which
#' vegetation layers with which cover) for one or more surveys.
#'
#' @param survey_name A character string or a character vector, depending on
#' multiple parameter, giving the name or names of the
#' survey(s) for which you want to extract releve information. If missing, all
#' surveys are returned.
#' @param connection dbconnection with the database 'Cydonia'
#' on the inbo-sql07-prd server
#' @param collect If FALSE (the default), a remote tbl object is returned.
#' This is like a reference to the result of the query but the full result of
#' the query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#' @param multiple If TRUE, survey_name can take a character vector with
#' multiple survey names that must match exactly. If FALSE (the default),
#' survey_name must be a single character string (one survey name) that can
#' include wildcarts to allow partial matches
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables RecordingGivid (uniek Id), LayerCode, CoverCode,
#' OriginalName, ScientificName, PhenologyCode, CoverageCode, PctValue
#' (percentage coverage), RecordingScale (name of the scale of coverage)
#' @family inboveg
#'
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get the recordings from one survey and collect the data
#' recording_heischraal2012 <- inboveg_recordings(con, survey_name =
#' "MILKLIM_Heischraal2012", collect = TRUE)
#'
#' # get all recordings from MILKLIM surveys (partial matching), don't collect
#' recording_milkim <- inboveg_recordings(con, survey_name = "%MILKLIM%",
#' collect = TRUE)
#'
#' # get recordings from several specific surveys
#' recording_severalsurveys <- inboveg_recordings(con, survey_name =
#' c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE,
#' collect = TRUE)
#'
#' # get all recordings of all surveys,  don't collect the data
#' allrecordings <- inboveg_recordings(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }
#'
#' @name inboveg_recordings-deprecated
#' @usage inboveg_recordings(connection, survey_name, collect = FALSE,
#'   multiple = FALSE)
#' @seealso \code{\link{inborutils-deprecated}}
#' @keywords internal
NULL

#' @rdname inborutils-deprecated
#' @section inboveg_recordings:
#' For \code{inboveg_recordings}, use [inbodb::inboveg_recordings()](https://inbo.github.io/inbodb/reference/get_inboveg_recordings.html)
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql
#'
#' @export

inboveg_recordings <- function(connection,
                        survey_name,
                        collect = FALSE,
                        multiple = FALSE) {

  .Defunct("inbodb::get_inboveg_classification()", package = "inborutils")
}
