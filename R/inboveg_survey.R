

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
#' con <- dbConnect(odbc::odbc(), dsn="Cydonia-prd")
#' surveys <- inboveg_coverage(con)
#' }
inboveg_survey <- function(connection) {

    iv_survey <- dbReadTable(con, "ivSurvey")
    iv_survey

}

