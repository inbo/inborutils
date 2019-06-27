#' Survey overview
#'
#'
#' @param connection odb DBIConnection-class, using DBI-ODBC like connectors
#'
#' @return data.frame
#' @export
#' @importFrom DBI dbReadTable
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' con <- connect_inbo_dbase("D0010_00_Cydonia")
#' surveys <- inboveg_coverage(con)
#' dbDisconnect(con)
#' }
inboveg_survey <- function(connection) {

    iv_survey <- dbReadTable(con, "ivSurvey")
    iv_survey

}

