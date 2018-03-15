
#' Provide metadata about the vegetation recordings of the INBOVEG database
#'
#' For each vegetation recording identified by RecordingGivid, the metadata is
#' provided in the resulting output table
#'
#' @param connection odb
#' @param recording_type vector of char, default c('Classic', 'Classic-emmer',
#'    'Classic-ketting')
#' @param survey_name vector of char or NULL (default NULL),
#'     i.e. all surveys are taken into account
#'
#' @return DBIConnection-class, using DBI-ODBC like connectors
#' @export
#' @importFrom DBI dbSendQuery dbBind dbFetch dbClearResult
#' @importFrom assertthat assert_that is.string is.null
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' con <- dbConnect(odbc::odbc(), dsn="Cydonia-prd")
#' metadata <- inboveg_metadata_kop(con, survey_name = NULL)
#' metadata <- inboveg_metadata_kop(con, survey_name = c("Sigma_LSVI_2012"))
#' }
inboveg_metadata_kop <- function(connection,
                                 recording_type = c('Classic', 'Classic-emmer',
                                                    'Classic-ketting'),
                                 survey_name = NULL) {

    assert_that(is.vector(recording_type))

    if (is.null(survey_name)) {
        query <- "SELECT ivRecording.RecordingGivid, ivRecording.UserReference,
                ivRecording.LocationCode, ivRecording.Latitude,
                ivRecording.Longitude, ivRecording.Area, ivRecording.SurveyId,
                ivRecTypeD.Name, ivRecording.NeedsWork
              FROM ivRecTypeD
              INNER JOIN ivRecording ON ivRecTypeD.ID = ivRecording.RecTypeID
              WHERE ivRecTypeD.Name IN ?;
             "
        query <- dbSendQuery(connection, query)
        dbBind(query, list(recording_type))
    } else {
        assert_that(is.vector(survey_name))

        query <- "SELECT ivRecording.RecordingGivid, ivRecording.UserReference,
                ivRecording.LocationCode, ivRecording.Latitude,
                ivRecording.Longitude, ivRecording.Area, ivRecording.SurveyId,
                ivRecTypeD.Name, ivRecording.NeedsWork
              FROM ivRecTypeD
              INNER JOIN ivRecording ON ivRecTypeD.ID = ivRecording.RecTypeID
              WHERE ivRecTypeD.Name IN ?
                AND ivSurvey.Name IN ?;
             "
        query <- dbSendQuery(connection, query)
        dbBind(query, list(recording_type, survey_name))
    }

    synonym_list <- dbFetch(query)
    dbClearResult(query)
    synonym_list
}
