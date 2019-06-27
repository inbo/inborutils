#' @title Query relevé information from INBOVEG
#'
#' @description This function queries the INBOVEG database for 
#' relevé information for a survey. 
#'
#' @param survey_name A character vector giving the name of the survey for which
#' you want to extract relevé information.
#' @param connection dbconnection with the database 'Cydonia' 
#' on the inbo-sql07-prd server
#' @param collect If FALSE (the default), a remote tbl object is returned. 
#' This is like a reference to the result of the query but the full result of 
#' the query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) with variables RecordingGivid (uniek Id), LayerCode, CoverCode, 
#' OrignalName, ScientificName, PhenologyCode, CoverageCode, PctValue 
#' (percentage coverage), RecordingScale (name of the scale of coverage)
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr collect tbl sql
#'
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#' # get the relevés from one survey and collect the data
#' relevé_info <- inboveg_relevé(con, survey_name = "MILKLIM_Heischraal2012", 
#' collect = TRUE)
#' # get all relevés of all surveys,  don't collect the data
#' allrelevés <- inboveg_relevé(con)
#' dbDisconnect(con)
#' rm(con)
#' }


inboveg_relevé <- function(connection,
                        survey_name,
                        collect = FALSE) {
  
  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  
  if (missing(survey_name)) {
    survey_name <- "%"
  } else {
    assert_that(is.character(survey_name))
  }
  
  
  sql_statement <- glue_sql(
    "SELECT ivS.Name
            , ivR.[RecordingGivid]
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
    AND ivRL_Iden.Preferred = 1
    AND ivS.Name LIKE {survey_name}",
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


## werkt nog niet, de pctvalue wordt niet gegeven in dit script, in MsqlSMS wel
## verder uitzoeken, als je op recordingGIVID werkt, lukt het ook 
