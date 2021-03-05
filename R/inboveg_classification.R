#' @title Query classification information from INBOVEG
#'
#' @description `r lifecycle::badge('defunct')`
#' This function queries the INBOVEG database for information
#' on the field classification (N2000 or BWK-code) of the releve (recording) for one
#' or more survey(s) by the name of the survey. See the examples
#' for how to get information for all surveys.
#'
#' @param survey_name A character vector giving the names of the surveys for
#' which you want to extract Classification information. If missing, all
#' surveys are returned.
#' @param classif A character vector giving the Classification code of the
#' vegetation type for which you want to extract information. If missing,
#' all classifications are returned.
#' @param connection dbconnection with the database 'Cydonia'
#' on the inbo-sql07-prd server
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables Id, SurveyName, Classification-code, BWK or
#' N2000-list, LocalClassification, Description of the Habitattype, Cover-code,
#' Cover in percentage.
#'
#' @family inboveg
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get a specific classification from a survey and collect the data
#' classif_info <- inboveg_classification(con,
#' survey_name = "MILKLIM_Heischraal2012", classif = "4010", collect = TRUE)
#'
#' # get all surveys, all classifications,  don't collect the data
#' allecodes <- inboveg_classification(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }
#'
#' @name inboveg_classification-deprecated
#' @usage inboveg_classification(connection, survey_name, classif, collect = FALSE)
#' @seealso \code{\link{inborutils-deprecated}}
#' @keywords internal
NULL

#' @rdname inborutils-deprecated
#' @section inboveg_classification:
#' For \code{inboveg_classification}, use [inbodb::inboveg_classification()](https://inbo.github.io/inbodb/reference/get_inboveg_classification.html)
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql
#'
#' @export

inboveg_classification <- function(connection,
                                   survey_name,
                                   classif,
                                   collect = FALSE) {

  .Defunct("inbodb::get_inboveg_classification()", package = "inborutils")

}

