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
#' library(DBI)
#' con <- dbConnect(odbc::odbc(), dsn="Cydonia-prd")
#' classifications <- inboveg_classification(con,
#'                            survey_name = c('Sigma_LSVI_2012'))
#' }
inboveg_classification <- function(connection, survey_name = NULL) {

    assert_that(is.vector(survey_name) | is.null(survey_name))

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

