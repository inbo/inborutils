#' Provide vegetation classifications
#'
#' Get the available vegatation classifications for the vegetation surveys
#'
#' @param connection odb DBIConnection-class, using DBI-ODBC like connectors
#' @param survey_name vector of char or NULL (default NULL),
#'     i.e. all surveys are taken into account
#'
#' @return data.frame
#' @export
#' @importFrom DBI dbSendQuery dbBind dbFetch dbClearResult
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#' classifications <- inboveg_classification(con,
#'                            survey_name = c('Sigma_LSVI_2012'))
#' dbDisconnect(con)
#' }
inboveg_classification <- function(connection, survey_name) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  if (missing(survey_name)) {
    survey_name <- NULL
  } else {
    assert_that(is.character(survey_name))
  }


  query <- "
        SELECT ivRecording.RecordingGivid
            , ivRLClassification.*
            , ftaClassif.Description as ClassifDescription
	        , ftaCover.Description as CoverDescription
        FROM dbo.ivRecording
        INNER JOIN dbo.ivSurvey ON ivSurvey.Id = ivRecording.SurveyId
        INNER JOIN dbo.ivRLClassification ON
            ivRLClassification.RecordingID = ivRecording.Id
        LEFT JOIN dbo.ivRLResources ClassifRes ON
            ClassifRes.ResourceGIVID = ivRLClassification.ClassifResource
	    LEFT JOIN D0013_00_Futon.dbo.ftActionGroupList ftaClassif ON
            ftaClassif.ListName = ClassifRes.ListName COLLATE Latin1_General_CI_AI
        LEFT JOIN dbo.ivRLResources CoverRes ON
            CoverRes.ResourceGIVID = ivRLClassification.CoverResource
	    LEFT JOIN D0013_00_Futon.dbo.ftActionGroupList ftaCover ON
            ftaCover.ListName = CoverRes.ListName COLLATE Latin1_General_CI_AI
        WHERE ivSurvey.Name IN ?
        ORDER BY ivRecording.RecordingGivid;
        "
  query <- dbSendQuery(connection, query)
  dbBind(query, survey = survey_name)
  classification_list <- dbFetch(query)
  dbClearResult(query)
  classification_list
}

