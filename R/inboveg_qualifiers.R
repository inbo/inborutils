#' @title Query qualifier information of recordings (releve) from INBOVEG
#'
#' @description `r lifecycle::badge('defunct')`
#' This function queries the INBOVEG database for
#' qualifier information on recordings  for one or more surveys.
#'
#' @param survey_name A character string or a character vector, depending on
#' multiple parameter, giving the name or names of the
#' survey(s) for which you want to extract recordings information. If missing,
#' all surveys are returned.
#' @param qualifier_type A character vector giving the name of qualifier type
#' for which you want to extract  information e.g. 'SQ' (site qualifier), 'MQ'
#' (management qualifier).
#' If missing, all qualifier types are returned.
#' @param connection dbconnection with the database 'Cydonia'
#' on the inbo-sql07-prd server
#' @param multiple If TRUE, survey_name can take a character vector with
#' multiple survey names that must match exactly. If FALSE (the default),
#' survey_name must be a single character string (one survey name) that can
#' include wildcards to allow partial matches
#'
#' @return A dataframe with variables RecordingGivid (unique Id), UserReference,
#' Observer, QualifierType, QualifierCode, Description, 2nd QualifierCode,
#' 2nd Description, 3rd QualifierCode, 3rd Description, Elucidation, in case
#' qualifier is 'NotSure', ParentID, QualifierResource
#'
#' @family inboveg
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#'
#' # get the qualifiers from one survey
#' qualifiers_heischraal2012 <- inboveg_qualifiers(con,
#'   survey_name =
#'     "MILKLIM_Heischraal2012"
#' )
#'
#' # get all site qualifiers (SQ) from MILKLIM surveys (partial matching)
#' qualifiers_milkim <- inboveg_qualifiers2(con,
#'   survey_name = "%MILKLIM%",
#'   qualifier_type = "SQ"
#' )
#'
#' # get qualifiers from several specific surveys
#' qualifiers_severalsurveys <- inboveg_qualifiers(con,
#'   survey_name =
#'     c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE
#' )
#'
#' # get all qualifiers of all surveys
#' allqualifiers <- inboveg_qualifiers(con)
#'
#' # Close the connection when done
#' dbDisconnect(con)
#' rm(con)
#' }
#'
#' @name inboveg_qualifiers-defunct
#' @usage inboveg_qualifiers(connection, survey_name, qualifier_type,
#'   multiple = FALSE)
#' @seealso \code{\link{inborutils-defunct}}
#' @keywords internal
NULL

#' @rdname inborutils-defunct
#' @section inboveg_qualifiers:
#' For \code{inboveg_qualifiers}, use [inbodb::inboveg_qualifier()](
#' https://inbo.github.io/inbodb/reference/get_inboveg_qualifier.html)
#'
#'
#' @export


inboveg_qualifiers <- function(connection,
                               survey_name,
                               qualifier_type,
                               multiple = FALSE) {
  .Defunct("inbodb::get_inboveg_classification()", package = "inborutils")
}
