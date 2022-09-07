#' @title Query survey information from INBOVEG
#'
#' @description `r lifecycle::badge('defunct')`
#' This function queries the INBOVEG database for survey information
#' (metadata about surveys) for one or more survey(s) by the name of the survey.
#' See the examples for how to get information for all surveys.
#'
#' @param survey_name A character vector giving the names of the surveys for
#' which you want to extract survey information.
#' @param connection dbconnection with the database 'Cydonia'
#' on the inbo-sql07-prd server
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables Id, Name, Description, Owner and Creator.
#'
#' @family inboveg
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get information of a specific survey and collect data
#' survey_info <- inboveg_survey(con,
#'   survey_name = "OudeLanden_1979",
#'   collect = TRUE
#' )
#'
#' # get information of all surveys and collect data
#' allsurveys <- inboveg_survey(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }
#'
#' @name inboveg_survey-defunct
#' @usage inboveg_survey(connection, survey_name, collect = FALSE)
#' @seealso \code{\link{inborutils-defunct}}
#' @keywords internal
NULL

#' @rdname inborutils-defunct
#' @section inboveg_survey:
#' For \code{inboveg_survey}, use
#' [inbodb::inboveg_survey()](
#' https://inbo.github.io/inbodb/reference/get_inboveg_survey.html)
#'
#'
#' @export

inboveg_survey <- function(connection,
                           survey_name,
                           collect = FALSE) {
  .Defunct("inbodb::get_inboveg_classification()", package = "inborutils")
}
