#' @title Query header information from INBOVEG
#'
#' @description This function queries the INBOVEG database for header
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

  .Deprecated("inbodb::get_inboveg_classification()", package = "inborutils")

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  if (missing(survey_name) & !multiple) {
    survey_name <- "%"
  }

  if (missing(survey_name) & multiple) {
    stop("Please provide one or more survey names to survey_name when multiple
         = TRUE")
  }

  if (!missing(survey_name)) {
    if (!multiple) {
      assert_that(is.character(survey_name))
    } else {
      assert_that(is.vector(survey_name, mode = "character"))
    }
  }


  if (missing(rec_type)) {
    rec_type <- "%"
  } else {
    assert_that(is.character(rec_type))
  }

common_part <- "SELECT
      ivR.RecordingGivid
      , ivS.Name
      , ivR.UserReference
      , ivR.Observer
      , ivR.LocationCode
      , ivR.Latitude
      , ivR.Longitude
      , COALESCE(ivR.Length * ivR.Width / 10000, try_convert(decimal, ivR.Area)) AS Area
      , ivR.Length
      , ivR.Width
      , ivR.VagueDateType
      , ivR.VagueDateBegin
      , ivR.VagueDateEnd
      , ivR.SurveyId
      , ivR.RecTypeID
      FROM [dbo].[ivRecording] ivR
      INNER JOIN [dbo].[ivSurvey] ivS on ivS.Id = ivR.SurveyId
      INNER JOIN [dbo].[ivRecTypeD] ivRec on ivRec.ID = ivR.RecTypeID
      where ivR.NeedsWork = 0"

if (!multiple) {
  sql_statement <- glue_sql(common_part,
                            "AND ivS.Name LIKE {survey_name}
                            AND ivREc.Name LIKE {rec_type}",
                            survey_name = survey_name,
                            rec_type = rec_type,
                            .con = connection)

} else {
  sql_statement <- glue_sql(common_part,
                            "AND ivS.Name IN ({survey_name*})
                            AND ivREc.Name LIKE {rec_type}",
                            survey_name = survey_name,
                            rec_type = rec_type,
                            .con = connection)
}

query_result <- tbl(connection, sql(sql_statement))

if (!isTRUE(collect)) {
  return(query_result)
} else {
  query_result <- collect(query_result)
  return(query_result)
}
}
