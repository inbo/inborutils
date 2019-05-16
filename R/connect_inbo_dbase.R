
#' Connect to an INBO database
#'
#' By default, INBO/windows users will have the `SQL Server` driver available
#' and do not have to adapt the second argument. However, for other operating
#' systems or users with other drivers installed, the `sql_driver` option
#' can be used.
#'
#' @param database_name char Name of the INBO database you want to connect
#' @param sql_driver char Driver (`SQL Server` by default) to use for connection
#'
#' @return odbc connection
#'
#' @export
#'
#' @importFrom DBI dbGetQuery dbConnect dbListTables
#' @importFrom odbc odbc
#'
#' @examples
#' \dontrun{
#' connection <- connect_inbo_dbase("D0021_00_userFlora")
#' }
connect_inbo_dbase <- function(database_name, sql_driver = "SQL Server") {

    # datawarehouse databases (sql08) start with an M, S or W; most
    # transactional (sql07) with a D (by agreement with dba's)
    if (any(startsWith(database_name, c("M", "S", "W")))) {
        server = "inbo-sql08-prd.inbo.be"  # DWH server
        type <- "INBO DWH Server"
    } else {
        server = "inbo-sql07-prd.inbo.be"  # SQL transactional server
        type <- "INBO PRD Server"
    }

    # connect to database
    conn <- dbConnect(odbc(),
                      driver = sql_driver,
                      server = server,
                      port = 1433,
                      database = database_name,
                      trusted_connection = "YES")

    # derived from the odbc package Viewer setup to activate the Rstudio Viewer
    code_call <- c(match.call())
    code_call <- paste(c("library(inborutils)",
                         paste("con <-", gsub(", ", ",\n\t", code_call))),
                       collapse = "\n")
    on_connection_opened(conn, code_call, type)

    return(conn)
}

on_connection_closed <- function(connection) {
    # make sure we have an observer
    observer <- getOption("connectionObserver")
    if (is.null(observer))
        return(invisible(NULL))

    # provide information no DWH or database
    if (grepl("08", connection@info$servername)) {
        type <- "INBO DWH Server"
    } else if (grepl("07", connection@info$servername)) {
        type <- "INBO PRD Server"
    }

    observer$connectionClosed(type, connection@info$dbname)
}

#' Overwrite the odbc function
#'
#' @inheritParams DBI::dbDisconnect
#'
#' @importFrom odbc dbIsValid
#' @importFrom utils getFromNamespace
#' @export
setMethod(
    "dbDisconnect", "OdbcConnection",
    function(conn, ...) {
        if (!dbIsValid(conn)) {
            warning("Connection already closed.", call. = FALSE)
        }

        on_connection_closed(conn)
        conn_release = getFromNamespace("connection_release", "odbc")
        conn_release(conn@ptr)
        invisible(TRUE)
    })

#' Rstudio Viewer integration
#'
#' See https://stackoverflow.com/questions/48936851/calling-odbc-connection-within-function-does-not-display-in-rstudio-connection and https://rstudio.github.io/rstudio-extensions/connections-contract.html#persistence
#' @param connection odbc connection
#' @param code dbase connection code
#' @param type INBO database server name
#'
#' @importFrom odbc odbcListObjectTypes odbcListObjects odbcListColumns
#' odbcPreviewObject odbcConnectionActions
#' @importFrom DBI dbDisconnect
on_connection_opened <- function(connection, code, type) {
    # make sure we have an observer
    observer <- getOption("connectionObserver")
    if (is.null(observer))
        return(invisible(NULL))

    # use the database name as the display name
    display_name <- paste("INBO Databases -", connection@info$dbname)
    server_name <- connection@info$servername

    # let observer know that connection has opened
    observer$connectionOpened(
        # connection type
        type = type,

        # name displayed in connection pane
        displayName = display_name,

        # host key
        host = connection@info$dbname,

        # icon for connection
        icon = system.file(file.path("static", "logo.png"),
                           package = "inborutils"),

        # connection code
        connectCode = code,

        # disconnection code
        disconnect = function() {
            dbDisconnect(connection)
        },

        listObjectTypes = function() {
            odbcListObjectTypes(connection)
        },

        # table enumeration code
        listObjects = function(...) {
            odbcListObjects(connection, ...)
        },

        # column enumeration code
        listColumns = function(...) {
            odbcListColumns(connection, ...)
        },

        # table preview code
        previewObject = function(rowLimit, ...) {
            odbcPreviewObject(connection, rowLimit, ...)
        },

        # other actions that can be executed on this connection
        actions = odbcConnectionActions(connection),

        # raw connection object
        connectionObject = connection
    )
}


