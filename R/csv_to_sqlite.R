#' Save a single CSV-table into a single table sqlite database
#'
#' @param csv_file name of the text file to convert
#' @param sqlite_file name of the newly created sqlite file
#' @param table_name name of the table to store the data table in the sqlite
#'      dbase
#' @param delim text file delimiter (default ",")
#' @param pre_process_size the number of lines to check the data types of the
#'      individual columns (default 1000)
#' @param chunk_size the number of lines to read for each chunk (default 50000)
#' @param quote same as in \code{readr::read_delim} (default "\"")
#' @param escape_backslash same as in \code{readr::read_delim} (default FALSE)
#' @param col_names same as in \code{readr::read_delim} (default TRUE)
#' @param col_types same as in \code{readr::read_delim} (default NULL)
#' @param locale same as in \code{readr::read_delim} (default default_locale())
#' @param na same as in \code{readr::read_delim} (default c("", "NA"))
#' @param quoted_na same as in \code{readr::read_delim} (default TRUE)
#' @param comment same as in \code{readr::read_delim} (default "")
#' @param trim_ws same as in \code{readr::read_delim} (default FALSE)
#' @param skip same as in \code{readr::read_delim} (default 0)
#' @param escape_double same as in \code{readr::read_delim} (default FALSE)
#'
#' @return a SQLite database
#'
#' @export
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite dbWriteTable
#' @importFrom readr read_delim
#' @importFrom dplyr %>% select_if mutate_at
#' @importFrom lubridate is.Date is.POSIXt
csv_to_sqlite <- function(csv_file, sqlite_file, table_name, delim = ",",
                          pre_process_size = 1000, chunk_size = 50000,
                          quote = "\"", escape_backslash = FALSE,
                          col_names = TRUE, col_types = NULL,
                          locale = default_locale(), na = c("", "NA"),
                          quoted_na = TRUE, comment = "",
                          trim_ws = FALSE, skip = 0,
                          escape_double = TRUE,
                          callback = append_to_sqlite) {
    con <- dbConnect(SQLite(), dbname = sqlite_file)

    # read an extract of the data to extract the colnames and types
    # to figure out the date and the datetime columns
    df <- read_delim(csv_file, delim, quote,
                     escape_backslash, col_names, col_types,
                     locale, na, quoted_na, comment,
                     trim_ws, skip, escape_double,
                     n_max = pre_process_size)
    date_cols <- df %>%
        select_if(is.Date) %>%
        colnames()
    datetime_cols <- df %>%
        select_if(is.POSIXt) %>%
        colnames()
    # write this first batch of lines to SQLITE table, converting dates to string representation
    df <- df %>%
      mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
      mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
    dbWriteTable(con, table_name, df, overwrite = TRUE)

    # readr chunk functionality
    read_delim_chunked(csv_file,
                       callback = append_to_sqlite, delim = delim,
                       skip = pre_process_size, col_names = colnames(df),
                       col_types = spec(df), chunk_size = chunk_size,
                       progress = FALSE)
    dbDisconnect(con)
}

#' Callback function that appends new sections to the SQLite table
#' @param x data frame
#' @param data_cols name of columns containing Date objects
#' @param datetime_cols name of columns containint POSIXt objects
append_to_sqlite <- function(x, data_cols, datetime_cols) {

  x <- as.data.frame(x)
  x <- x %>%
    mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
    mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
  # append data frame to table
  dbWriteTable(con, table_name, x, append = TRUE)
}
