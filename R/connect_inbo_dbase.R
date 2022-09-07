
#' Connect to an INBO database
#'
#' @description `r lifecycle::badge('defunct')`
#' Connects to an INBO database by simply providing the database's name as an
#' argument.
#' The function can only be used from within the INBO network.
#'
#' For more information, refer to
#' \href{https://inbo.github.io/tutorials/tutorials/r_database_access/}{this tutorial}. # nolint
#'
#' @param database_name char Name of the INBO database you want to connect
#'
#' @return odbc connection
#'
#' @examples
#' \dontrun{
#' connection <- connect_inbo_dbase("D0021_00_userFlora")
#' connection <- connect_inbo_dbase("W0003_00_Lims")
#' }
#'
#' @name connect_inbo_dbase-defunct
#' @usage connect_inbo_dbase(database_name)
#' @seealso \code{\link{inborutils-defunct}}
#' @keywords internal
NULL

#' @rdname inborutils-defunct
#' @section connect_inbo_dbase:
#' For \code{connect_inbo_dbase}, use
#' [inbodb::connect_inbo_dbase()](
#' https://inbo.github.io/inbodb/reference/connect_inbo_dbase.html)
#' @export
connect_inbo_dbase <- function(database_name) {
  .Defunct("inbodb::connect_inbo_dbase()", package = "inborutils")
}
