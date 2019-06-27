#' Taxa and coverage per vegetation layer
#'
#' For the requested surveys, the coverage for each taxon and each vegetation
#' layer is provided.
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
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#' coverage <- inboveg_coverage(con, survey_name = c('Sigma_LSVI_2012'))
#' coverage <- inboveg_coverage(con, recording_type = c('Classic',
#'     'Classic-emmer', 'Classic-ketting'), survey_name = c('Sigma_LSVI_2012'))
#'  dbDisconnect(con)
#' }
inboveg_coverage <- function(connection,
                            recording_type = c('Classic', 'Classic-emmer',
                                               'Classic-ketting'),
                            survey_name = NULL) {

    assert_that(is.vector(recording_type))
    assert_that(is.vector(survey_name) | is.null(survey_name))

    query <- "
        SELECT ivRecording.RecordingGivid, ivRecording.LocationCode,
            ivRLLayer.LayerCode, ivRLLayer.CoverCode,
            ivRLIdentification.TaxonFullText AS OriginalName,
            ivRLIdentification.PhenologyCode,
            ivRLTaxonOccurrence.CoverageCode, ftCoverValues.PctValue,
            ftActionGroupList.Description, ivRLIdentification.TaxonGroup,
            ivRecording.VagueDateType, ivRecording.VagueDateBegin,
            ivRecording.VagueDateEnd, ivRecTypeD.Name
        FROM dbo.ivRecTypeD
        INNER JOIN dbo.ivRecording ON ivRecording.RecTypeID = ivRecTypeD.ID
        INNER JOIN dbo.ivSurvey ON ivSurvey.Id = ivRecording.SurveyId
        LEFT JOIN dbo.ivRLLayer ON ivRLLayer.RecordingID = ivRecording.Id
        LEFT JOIN dbo.ivRLTaxonOccurrence ON
            ivRLTaxonOccurrence.LayerID = ivRLLayer.ID
        LEFT JOIN dbo.ivRLIdentification ON
            ivRLIdentification.OccurrenceID = ivRLTaxonOccurrence.ID
        LEFT JOIN dbo.ivRLResources ON
            ivRLResources.ResourceGIVID = ivRLTaxonOccurrence.CoverageResource
        LEFT JOIN D0013_00_Futon.dbo.ftActionGroupList ON
            ftActionGroupList.ListName = ivRLResources.ListName
            COLLATE Latin1_General_CI_AI
        LEFT JOIN D0013_00_Futon.dbo.ftCoverValues ON
            ftCoverValues.ListGIVID = ftActionGroupList.ListGIVID
            COLLATE Latin1_General_CI_AI
            AND ftCoverValues.Code = [ivRLTaxonOccurrence].[CoverageCode]
            COLLATE Latin1_General_CI_AI
        WHERE ivRLIdentification.Preferred = 1
            AND ivRecTypeD.Name IN ?
            AND ivSurvey.Name IN ?
        ORDER BY ivRLLayer.LayerCode;
        "
    query <- dbSendQuery(connection, query)
    dbBind(query, survey = survey_name)
    coverage <- dbFetch(query)
    dbClearResult(query)
    coverage
}

