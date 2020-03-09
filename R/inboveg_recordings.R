#' @title Query recording (releve) information from INBOVEG
#'
#' @description This function queries the INBOVEG database for
#' releve information (which species were recorded in which plots and in
#' which vegetation layers with which cover) for one or more surveys,
#' or in combination with the unique ID (recordingGIVID) or user reference
#'
#' @param user_reference A character string or a character vector giving the name
#' of a recording for which you want to extract releve information. If missing, all
#' user-references are returned.
#' @param recording_givid A character string or a character vector giving
#' the unique id of a recording for which you want to extract releve information.
#' If missing, all recording_givids are returned.
#' @param survey_name A character string or a character vector, depending on
#' multiple parameter, giving the name or names of the survey(s) for which you
#' want to extract releve information. If missing, all surveys are returned.
#' @param connection dbconnection with the database 'Cydonia'
#' on the inbo-sql07-prd server
#' @param collect If FALSE (the default), a remote tbl object is returned.
#' This is like a reference to the result of the query but the full result of
#' the query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#' @param multiple If TRUE, survey_name, user_reference or recording_givid can
#' take a character vector with multiple survey names that must match exactly.
#' If FALSE (the default), survey_name , user_reference or recording_givid must
#' be a single character string (one survey name, or one user_reference or one
#' recording_givid). Only survey_name can include wildcarts to allow partial matches.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables RecordingGivid (unique ID), User reference, LayerCode, CoverCode,
#' OriginalName, ScientificName, PhenologyCode, CoverageCode, PctValue
#' (percentage coverage), RecordingScale (name of the scale of coverage)
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql
#'
#' @export
#' @family inboveg
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get the recordings from one survey and collect the data
#' recording_heischraal2012 <- inboveg_recordings(con, survey_name =
#' "MILKLIM_Heischraal2012", collect = TRUE)
#'
#' # get all recordings from MILKLIM surveys (partial matching), don't collect
#' recording_milkim <- inboveg_recordings(con, survey_name = "%MILKLIM%",
#' collect = TRUE)
#'
#' # get recordings from several specific surveys
#' recording_severalsurveys <- inboveg_recordings(con, survey_name =
#' c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE,
#' collect = TRUE)
#'
#' # get recordings from several specific recordinggivid
#' recording_severalgivids <- inboveg_recordings(con,
#' recording_givid = c("IV2012081609450300","IV2012081610204607"),
#' multiple = TRUE, collect = TRUE)
#'
#' # get all recordings of all surveys,  don't collect the data
#' allrecordings <- inboveg_recordings(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }

inboveg_recordings <- function(
  connection,
  survey_name,
  user_reference,
  recording_givid,
  collect = FALSE,
  multiple = FALSE) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")

  if (!multiple) {
    if (missing(survey_name)) {
      survey_name <- "%"
    } else {
      assert_that(is.character(survey_name))
    }
    if (missing(user_reference)) {
      user_reference <- "%"
    } else {
      assert_that(is.character(user_reference))
    }
    if (missing(recording_givid)) {
      recording_givid <- "%"
    } else {
      assert_that(is.character(recording_givid))
    }
  } else {
    if (missing(survey_name) & missing(recording_givid) &
        missing(user_reference)) {
      stop("Please provide one or more values to either survey_name or to user_reference or recording_givid when multiple = TRUE") #nolint
    }
    if (missing(survey_name) & !missing(user_reference)) {
      warning("The same user_reference might have been used in different surveys. Consider also providing one or more values to survey_name when multiple = TRUE") #nolint
    }
    if (!missing(survey_name)) {
      assert_that(is.vector(survey_name, mode = "character"))
    }
    if (!missing(user_reference)) {
      assert_that(is.vector(user_reference, mode = "character"))
    }
    if (!missing(recording_givid)) {
      assert_that(is.vector(recording_givid, mode = "character"))
    }
  }


  common_part <- "SELECT ivS.Name
  , ivR.[RecordingGivid]
  , ivR.UserReference
  , ivRL_Layer.LayerCode
  , ivRL_Layer.CoverCode
  , ivRL_Iden.TaxonFullText as OrignalName
  , Synoniem.ScientificName
  , ivRL_Iden.PhenologyCode
  , ivRL_Taxon.CoverageCode
  , ftCover.PctValue
  , ftAGL.Description as RecordingScale
  FROM  dbo.ivSurvey ivS
  INNER JOIN [dbo].[ivRecording] ivR  ON ivR.SurveyId = ivS.Id
  -- Deel met soortenlijst en synoniem
  INNER JOIN [dbo].[ivRLLayer] ivRL_Layer on ivRL_Layer.RecordingID = ivR.Id
  INNER JOIN [dbo].[ivRLTaxonOccurrence] ivRL_Taxon on
  ivRL_Taxon.LayerID = ivRL_Layer.ID
  INNER JOIN [dbo].[ivRLIdentification] ivRL_Iden on
  ivRL_Iden.OccurrenceID = ivRL_Taxon.ID
  LEFT JOIN (SELECT ftTaxon.TaxonName AS TaxonFullText
  , COALESCE([GetSyn].TaxonName, ftTaxon.TaxonName) AS ScientificName
  , COALESCE([GetSyn].TaxonGIVID, ftTaxon.TaxonGIVID) AS TAXON_LIST_ITEM_KEY
  , COALESCE([GetSyn].TaxonQuickCode, ftTaxon.TaxonQuickCode) AS QuickCode
  FROM [syno].[Futon_dbo_ftTaxon] ftTaxon
  INNER JOIN [syno].[Futon_dbo_ftTaxonListItem] ftTLI ON
  ftTLI.TaxonGIVID = ftTaxon.TaxonGIVID
  LEFT JOIN (SELECT ftTaxonLI.TaxonListItemGIVID
  , ftTaxon.TaxonGIVID
  , ftTaxon.TaxonName
  , ftTaxon.TaxonQuickCode
  , ftAGL.ListName
  , ftTaxonLI.PreferedListItemGIVID
  FROM [syno].[Futon_dbo_ftActionGroupList] ftAGL
  INNER JOIN [syno].[Futon_dbo_ftTaxonListItem] ftTaxonLI ON
  ftTaxonLI.TaxonListGIVID = ftAGL.ListGIVID
  LEFT JOIN [syno].[Futon_dbo_ftTaxon] ftTaxon ON
  ftTaxon.TaxonGIVID = ftTaxonLI.TaxonGIVID
  WHERE 1=1
  AND ftAGL.ListName = 'INBO-2011 Sci'
  ) GetSyn
  ON GetSyn.TaxonListItemGIVID = ftTLI.PreferedListItemGIVID
  WHERE ftTLI.TaxonListGIVID = 'TL2011092815101010'
  ) Synoniem on
  ivRL_Iden.TaxonFullText = Synoniem.TaxonFullText collate Latin1_General_CI_AI
  -- Hier begint deel met bedekking
  LEFT JOIN [dbo].[ivRLResources] ivRL_Res on
  ivRL_Res.ResourceGIVID = ivRL_Taxon.CoverageResource
  LEFT JOIN [syno].[Futon_dbo_ftActionGroupList] ftAGL on
  ftAGL.ActionGroup = ivRL_Res.ActionGroup collate Latin1_General_CI_AI
  AND ftAGL.ListName = ivRL_Res.ListName collate Latin1_General_CI_AI
  LEFT JOIN [syno].[Futon_dbo_ftCoverValues] ftCover on
  ftCover.ListGIVID = ftAGL.ListGIVID
  AND ivRL_Taxon.CoverageCode = ftCover.Code collate Latin1_General_CI_AI
  WHERE 1=1
  AND ivRL_Iden.Preferred = 1"

  if (!multiple) {
    sql_statement <- glue_sql(common_part,
                              "AND ivS.Name LIKE {survey_name}
                              AND ivR.[RecordingGivid] LIKE {recording_givid}
                              AND ivR.UserReference LIKE {user_reference}",
                              survey_name = survey_name,
                              user_reference = user_reference,
                              recording_givid = recording_givid,
                              .con = connection)

  } else {
    if (!missing(survey_name) & !missing(user_reference) &
        !missing(recording_givid)) {
      sql_statement <- glue_sql(common_part,
                                "AND ivS.Name IN ({survey_name*})
                                AND (ivR.[RecordingGivid] IN ({recording_givid*})
                                OR ivR.UserReference IN ({user_reference*}))",
                                survey_name = survey_name,
                                user_reference = user_reference,
                                recording_givid = recording_givid,
                                .con = connection)
    }
    if (missing(survey_name) & !missing(user_reference) &
        !missing(recording_givid)) {
      sql_statement <- glue_sql(common_part,
                                "AND (ivR.[RecordingGivid] IN ({recording_givid*})
                                OR ivR.UserReference IN ({user_reference*}))",
                                survey_name = survey_name,
                                user_reference = user_reference,
                                recording_givid = recording_givid,
                                .con = connection)
    }
    if (missing(survey_name) & missing(user_reference) &
        !missing(recording_givid)) {
      sql_statement <- glue_sql(common_part,
                                "AND ivR.[RecordingGivid] IN ({recording_givid*})",
                                survey_name = survey_name,
                                user_reference = user_reference,
                                recording_givid = recording_givid,
                                .con = connection)
    }
    if (missing(survey_name) & !missing(user_reference) &
        missing(recording_givid)) {
      sql_statement <- glue_sql(common_part,
                                "AND ivR.UserReference IN ({user_reference*})",
                                survey_name = survey_name,
                                user_reference = user_reference,
                                recording_givid = recording_givid,
                                .con = connection)
    }
    if (!missing(survey_name) & missing(user_reference) &
        missing(recording_givid)) {
      sql_statement <- glue_sql(common_part,
                                "AND ivS.Name IN ({survey_name*})",
                                survey_name = survey_name,
                                user_reference = user_reference,
                                recording_givid = recording_givid,
                                .con = connection)
    }
    if (!missing(survey_name) & missing(user_reference) &
        !missing(recording_givid)) {
      sql_statement <- glue_sql(common_part,
                                "AND ivS.Name IN ({survey_name*})
                                AND ivR.[RecordingGivid] IN ({recording_givid*})",
                                survey_name = survey_name,
                                user_reference = user_reference,
                                recording_givid = recording_givid,
                                .con = connection)
    }
    if (!missing(survey_name) & !missing(user_reference) &
        missing(recording_givid)) {
      sql_statement <- glue_sql(common_part,
                                "AND ivS.Name IN ({survey_name*})
                                AND ivR.UserReference IN ({user_reference*})",
                                survey_name = survey_name,
                                user_reference = user_reference,
                                recording_givid = recording_givid,
                                .con = connection)
    }

  }

  query_result <- tbl(connection, sql(sql_statement))

  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- collect(query_result)
    return(query_result)
  }
}


