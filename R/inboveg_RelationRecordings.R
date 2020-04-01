#' @title Query relation (Parent - Child) information of recordings (releve) from INBOVEG
#'
#' @description This function queries the INBOVEG database for
#' relation information on recordings for one or more surveys based on
#' Parent (classic-chain/bucket) and Child (classic) relationchip.
#'
#' @param survey_name A character string or a character vector, depending on
#' multiple parameter, giving the name or names of the
#' survey(s) for which you want to extract recordings information. If missing, all
#' surveys are returned.
#' @param connection dbconnection with the database 'Cydonia'
#' on the inbo-sql07-prd server
#' @param multiple If TRUE, survey_name can take a character vector with
#' multiple survey names that must match exactly. If FALSE (the default),
#' survey_name must be a single character string (one survey name) that can
#' include wildcards to allow partial matches
#'
#' @return A dataframe with variables RecordingId, Child_GIVID (unique RecordingGIVID), Child_UserRef (UserReference),
#' ParentId (RecordingId), Parent_GIVID (uniek RecordingGIVID) and Parent_UserRef (UserReference)
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#'
#' @export
#' @family inboveg
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get the Parent-Child-relations from one survey
#' relations_N2000meetnet_Grasland <- inboveg_relation(con,
#' survey_name = "N2000meetnet_Grasland")
#'
#' # get all Parent-Child-relations from N2000meetnet surveys (partial matching)
#' relations_N2000meetnet <- inboveg_relation(con, survey_name = "%N2000meetnet%")
#'
#' # get Parent-Child-relations from several specific surveys
#' relations_severalsurveys <- inboveg_relation(con, survey_name =
#' c("DeBlankaart-1985-Beheer", "N2000meetnet_Grasland"), multiple = TRUE)
#'
#' # get all Parent-Child-relations of all relevant surveys
#' allrelations <- inboveg_relation(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }
#'

inboveg_relation3 <- function(connection,
                           survey_name,
                           multiple = FALSE,
                           collect = FALSE) {

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

common_part <-
# " SELECT
#       ivSurvey.Name
#       , ivRecordingRelation.RecordingId
#       , ivRecording.RecordingGivid AS Child_GIVID
#       , ivRecording.UserReference AS Child_UserRef
#       , ivRecordingRelation.ParentId
#       , ivRecording_1.RecordingGivid AS Parent_GIVID
#       , ivRecording_1.UserReference AS Parent_UserRef
#   FROM (
#         (ivRecordingRelation
#         RIGHT JOIN ivRecording ON ivRecordingRelation.RecordingId = ivRecording.Id)
#         LEFT JOIN ivRecording AS ivRecording_1 ON ivRecordingRelation.ParentId = ivRecording_1.Id
#         )
#   INNER JOIN ivSurvey ON ivRecording.SurveyId = ivSurvey.Id
#   WHERE (((ivRecordingRelation.ParentId) Is Not Null));"


## 2de poging
# " SELECT
#       ivSurvey.Name
#       , ivRecordingRelation.RecordingId
#       , ivRecording.RecordingGivid AS Child_GIVID
#       , ivRecording.UserReference AS Child_UserRef
#       , ivRecordingRelation.ParentId
#       , ivRecording_1.RecordingGivid AS Parent_GIVID
#       , ivRecording_1.UserReference AS Parent_UserRef
#   FROM ivSurvey
#       INNER JOIN ivRecording on ivSurvey.Id = ivRecording.SurveyId
#       RIGHT JOIN ivRecording ON ivRecordingRelation.RecordingId = ivRecording.Id
#       LEFT JOIN ivRecording AS ivRecording_1 ON ivRecordingRelation.ParentId = ivRecording_1.Id
#       WHERE (((ivRecordingRelation.ParentId) Is Not Null));"
#" 3de poging

"SELECT
        ivSurvey.Name
        , ivRecordingRelation.RecordingId
        , ivRecording.RecordingGivid AS Child_GIVID
        , ivRecording.UserReference AS Child_UserRef
        , ivRecordingRelation.ParentId
        , ivRecording_1.RecordingGivid AS Parent_GIVID
        , ivRecording_1.UserReference AS Parent_UserRef
  FROM (
          (ivRecordingRelation
            RIGHT JOIN ivRecording ON ivRecordingRelation.RecordingId = ivRecording.Id)
          LEFT JOIN ivRecording AS ivRecording_1 ON ivRecordingRelation.ParentId = ivRecording_1.Id
        )
  INNER JOIN ivSurvey ON ivRecording.SurveyId = ivSurvey.Id
  WHERE (((ivRecordingRelation.ParentId) Is Not Null));"

if (!multiple) {
  sql_statement <- glue_sql(common_part,
                            "AND ivS.Name LIKE {survey_name}",
                            survey_name = survey_name,
                            .con = connection)

} else {
  sql_statement <- glue_sql(common_part,
                            "AND ivS.Name IN ({survey_name*})",
                            survey_name = survey_name,
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

# TEST
relations_N2000meetnet_Grasland <- inboveg_relation3(con, survey_name = "N2000meetnet_Grasland")
relations_N2000meetnet <- inboveg_relation3(con, survey_name = "%N2000meetnet%")
relations_severalsurveys <- inboveg_relation3(con, survey_name = c("DeBlankaart-1985-Beheer", "N2000meetnet_Grasland"), multiple = TRUE)
allrelations <- inboveg_relation3(con)
