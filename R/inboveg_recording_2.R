## TEST VOORBEELDEN

# get the recordings from one survey and collect the data
recording_heischraal2012 <- inboveg_recordings6(con, survey_name ="MILKLIM_Heischraal2012", collect = TRUE)
# --> dit werkt

# get all recordings from MILKLIM surveys (partial matching), don't collect
recording_milkim <- inboveg_recordings6(con, survey_name = "%MILKLIM%", collect = TRUE)
# --> dit werkt

# get recordings from several specific surveys
recording_severalsurveys <- inboveg_recordings6(con, survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE,collect = TRUE)
# --> dit werkt
## bij functie5 werkt dit niet meer, dus multiple is fout
# Error in is.character(survey_name, mode = "vector") :
#2 arguments passed to 'is.character' which requires 1

# get all recordings of all surveys,  don't collect the data
allrecordings <- inboveg_recordings6(con)
# --> dit werkt

# dit werkt  ( --user_reference = "HS_036",)
recording_heischraal2012_test3 <- inboveg_recordings6(con,
                               #survey_name = "MILKLIM_Heischraal2012",
                               user_reference = "1",
                               #recording_givid = "IV2012081609450300",
                               multiple = TRUE,
                                collect = FALSE)
# juiste warning krijg je hier: warning("The same user_reference might have been used in different surveys.
# Consider also providing one or more values to survey_name when multiple = TRUE")
# werkt niet meer bij functie n5

#" Functie 5 dit werkt niet meer
# Error in eval(assertion, env) :
#   argument "survey_name" is missing, with no default
# In addition: Warning message:
#   In inboveg_recordings5(con, user_reference = "1", multiple = TRUE,  :
#
#                            Show Traceback
#
#                          Rerun with Debug
#                          Error in eval(assertion, env) :
#                            argument "survey_name" is missing, with no default

recording_heischraal2012_test4 <- inboveg_recordings6(con,
                                                      #survey_name = "MILKLIM_Heischraal2012",
                                                      #user_reference = "1",
                                                      #recording_givid = "IV2012081609450300",
                                                      multiple = TRUE,
                                                      collect = FALSE)
# juiste error: Error in inboveg_recordings2(con, multiple = TRUE, collect = FALSE) :
# Please provide one or more values to either survey_name or to user_reference or recording_givid when multiple = #  # # TRUE
## functie 5 werkt!

# multiple user references: werkt!
recording_heischraal2012_test5 <- inboveg_recordings6(con,
                                                      #survey_name = "MILKLIM_Heischraal2012",
                                                      user_reference = c("HS_036", "HS_037"),
                                                      #recording_givid = "IV2012081609450300",
                                                      multiple = TRUE,
                                                      collect = FALSE)
# Warning message:
#   In inboveg_recordings3(con, user_reference = c("HS_036", "HS_037"),  :
#  The same user_reference might have been used in different surveys. Consider also providing one or more values to survey_name when multiple = TRUE

# multiple user references: met multiple = FALSE ?
recording_heischraal2012_test5B <- inboveg_recordings6(con,
                                                      #survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
                                                      user_reference = c("HS_036", "HS_037"),
                                                      #recording_givid = "IV2012081609450300",
                                                      multiple = FALSE,
                                                      collect = FALSE)
## ERROr want multiple is false: hier een melding geven ook?
#" functie5 ook error
# Error: Result 2 must be a single string, not a vector of class `sql/character` and of length 2
# Call `rlang::last_error()` to see a backtrace

# multiple survey_name : met multiple = FALSE ?
recording_heischraal2012_test5C <- inboveg_recordings6(con,
                                                       survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
                                                       #user_reference = c("HS_036", "HS_037"),
                                                       #recording_givid = "IV2012081609450300",
                                                       multiple = FALSE,
                                                       collect = FALSE)
## ERROr want multiple is false: hier een melding geven ook?
## functie 5 ook error

# multiple recordinggivid: werkt
recording_heischraal2012_test6 <- inboveg_recordings6(con,
                                                      #survey_name = "MILKLIM_Heischraal2012",
                                                      #user_reference = c("HS_036", "HS_037"),
                                                      recording_givid = c("IV2012081609450300","IV2012081610204607"),
                                                      multiple = TRUE,
                                                      collect = TRUE)
# error met functie 5
# Error in eval(assertion, env) :
#   argument "survey_name" is missing, with no default


# multiple recordinggivid with multiple = FALSE
recording_heischraal2012_test7 <- inboveg_recordings6(con,
                                                      #survey_name = "MILKLIM_Heischraal2012",
                                                      #user_reference = c("HS_036", "HS_037"),
                                                      recording_givid = c("IV2012081609450300","IV2012081610204607"),
                                                      # multiple = TRUE,
                                                      collect = TRUE)
# ERROR
## functie 5 ook error

# wildcards in userreference or recordinggivid?
recording_heischraal2012_test8 <- inboveg_recordings6(con,
                                                      #survey_name = "MILKLIM_Heischraal2012",
                                                      #user_reference = "HS%",
                                                      recording_givid = "IV2012%",
                                                      multiple = TRUE,
                                                      collect = TRUE)
# functie 5 error
recording_heischraal2012_test9 <- inboveg_recordings6(con,
                                                      #survey_name = "MILKLIM_Heischraal2012",
                                                      user_reference = "HS%",
                                                      #recording_givid = "IV2012%",
                                                      multiple = TRUE,
                                                      collect = TRUE)
## functie 5 error


## FUNCTIE


inboveg_recordings6 <- function(
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
  }

  else {
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
