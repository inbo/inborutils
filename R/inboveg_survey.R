#' @title Query survey information from INBOVEG
#'
#' @description This function queries the INBOVEG database for survey information
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
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#' survey <- "OudeLanden_1979"
#' get information of a specific survey and collect data
#' survey_info <- inboveg_survey(con, survey_name = "OudeLanden_1979", 
#' collect = TRUE)
#' get information of all surveys and collect data
#' allsurveys <- inboveg_survey(con)
#' dbDisconnect(con)
#' rm(con)
#' }

inboveg_survey <- function(connection, 
                  survey_name,
                  collect = FALSE) {
  
  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  
  if (missing(survey_name)){
    survey_name <- "%"
  } else {
    assert_that(is.character(survey_name))
  }
  
  sql_statement <- glue_sql(
    "SELECT
    ivS.Id
    , ivS.Name
    , ivS.Description
    , ivS.Owner
    , ivS.creator
    FROM [dbo].[ivSurvey] ivS
    WHERE ivS.Name LIKE {survey_name}",
    survey_name = survey_name,
    .con = connection)

query_result <- tbl(connection, sql(sql_statement))

if (!isTRUE(collect)) {
  return(query_result)
} else {
  query_result <- collect(query_result)
  return(query_result)
}
}
