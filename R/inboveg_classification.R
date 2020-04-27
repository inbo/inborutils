#' @title Query classification information from INBOVEG
#'
#' @description This function queries the INBOVEG database for information
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

  .Deprecated("inbodb::get_inboveg_classification()", package = "inborutils")

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  if (missing(survey_name)) {
    survey_name <- "%"
  } else {
    assert_that(is.character(survey_name))
  }

  if (missing(classif)) {
    classif <- "%"
  } else {
    assert_that(is.character(classif))
  }

  sql_statement <- glue_sql(
    "Select ivR.RecordingGivid
    , ivS.Name
    , ivRLClas.Classif
    , ivRLRes_Class.ActionGroup
    , ivRLRes_Class.ListName
    , ftBWK.Description as LocalClassification
    , ftN2k.Description  as Habitattype
    , ivRLClas.Cover
    , ftC.PctValue
    FROM ivRecording ivR
    INNER JOIN ivSurvey ivS on ivS.Id = ivR.surveyId
    LEFT JOIN [dbo].[ivRLClassification] ivRLClas on ivRLClas.RecordingID = ivR.Id
    LEFT JOIN [dbo].[ivRLResources] ivRLRes_Class on ivRLRes_Class.ResourceGIVID = ivRLClas.ClassifResource
    LEFT JOIN [syno].[Futon_dbo_ftActionGroupList] ftAGL_Class on ftAGL_Class.ActionGroup = ivRLRes_Class.ActionGroup collate Latin1_General_CI_AI
    AND ftAGL_Class.ListName = ivRLRes_Class.ListName collate Latin1_General_CI_AI
    LEFT JOIN [syno].[Futon_dbo_ftBWKValues] ftBWK on ftBWK.Code = ivRLClas.Classif collate Latin1_General_CI_AI
    AND ftBWK.ListGIVID = ftAGL_Class.ListGIVID
    LEFT JOIN [syno].[Futon_dbo_ftN2kValues] ftN2K on ftN2K.Code = ivRLClas.Classif collate Latin1_General_CI_AI
    AND ftN2K.ListGIVID = ftAGL_Class.ListGIVID
    LEFT JOIN [dbo].[ivRLResources] ivRLR_C on ivRLR_C.ResourceGIVID = ivRLClas.CoverResource
    LEFT JOIN [syno].[Futon_dbo_ftActionGroupList] ftAGL_C on ftAGL_C.ActionGroup = ivRLR_C.ActionGroup collate Latin1_General_CI_AI
    AND ftAGL_C.ListName = ivRLR_C.ListName collate Latin1_General_CI_AI
    LEFT JOIN [syno].[Futon_dbo_ftCoverValues] ftC on ftC.Code = ivRLClas.Cover collate Latin1_General_CI_AI
    AND ftAGL_C.ListGIVID = ftC.ListGIVID
    WHERE ivRLClas.Classif is not NULL
    AND ivS.Name LIKE {survey_name}
    AND ivRLClas.Classif LIKE {classif}",
    survey_name = survey_name,
    classif = classif,
    .con = connection)

  query_result <- tbl(connection, sql(sql_statement))

  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- collect(query_result)
    return(query_result)
  }
}

