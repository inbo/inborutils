#' Provide metadata about the vegetation recordings of the INBOVEG database
#'
#' For each vegetation recording identified by RecordingGivid, the metadata is
#' provided in the resulting output table
#'
#' @param connection odb DBIConnection-class, using DBI-ODBC like connectors
#' @param recording_type vector of char, default c('Classic', 'Classic-emmer',
#'    'Classic-ketting')
#' @param survey_name vector of char or NULL (default NULL),
#'     i.e. all surveys are taken into account
#'
#' @return data.frame
#' @export
#' @importFrom DBI dbSendQuery dbBind dbFetch dbClearResult
#' @importFrom assertthat assert_that is.string
#' @family inboveg
#' @examples
#' \dontrun{
#' library(DBI)
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#' metadata <- inboveg_metadata_kop(con)
#' metadata <- inboveg_metadata_kop(con, survey_name = c("Sigma_LSVI_2012"))
#' dbDisconnect(con)
#' }
inboveg_metadata_kop <- function(connection,
                                 recording_type = c('Classic', 'Classic-emmer',
                                                    'Classic-ketting'),
                                 survey_name) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  assert_that(is.vector(recording_type))

  if (missing(survey_name)) {
    survey_name <- NULL
  } else {
    assert_that(is.character(survey_name))
  }

    query <- "
            SELECT ivRecording.RecordingGivid, ivRecording.UserReference,
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

    metadata_kop <- dbFetch(query)
    dbClearResult(query)
    metadata_kop
}
