#' Save a delimited text table into a single table `sqlite` database
#'
#' The table can be a comma separated (`csv`) or a tab separated (`tsv`) or any
#' other delimited text file. The file is read in chunks. Each chunk is copied
#' in the same `sqlite` table database before the next chunk is loaded into
#' memory.
#' See the INBO tutorial [Handling large files in R](
#' https://inbo.github.io/tutorials/tutorials/r_large_data_files_handling/)
#' to learn more.
#'
#' @section Remark:
#' The \code{callback} argument in the \code{read_delim_chunked} function call
#' refers to the custom written callback function `append_to_sqlite` applied
#' to each chunk.
#'
#' @param csv_file Name of the text file to convert.
#' @param sqlite_file Name of the newly created `sqlite` file.
#' @param table_name Name of the table to store the data table in the `sqlite`
#'   database.
#' @param delim Text file delimiter (default ",").
#' @param pre_process_size Number of lines to check the data types of the
#'   individual columns (default 1000).
#' @param chunk_size Number of lines to read for each chunk (default 50000).
#' @param show_progress_bar Show progress bar (default TRUE).
#' @param ... Further arguments to be passed to \code{read_delim}.
#'
#' @return a `SQLite` database
#' @family Data_handling_utilities
#' @examples
#' \dontrun{
#' library(R.utils)
#' library(dplyr)
#' csv.name <- "2016-04-20-processed-logs-big-file-example.csv"
#' db.name <- "2016-04-20-processed-logs-big-file-example.db"
#' # download the CSV file example
#' csv.url <- paste("https://s3-eu-west-1.amazonaws.com/lw-birdtracking-data/",
#'   csv.name, ".gz",
#'   sep = ""
#' )
#' download.file(csv.url, destfile = paste0(csv.name, ".gz"))
#' gunzip(paste0(csv.name, ".gz"))
#' # Make a SQLite database
#' sqlite_file <- "example2.sqlite"
#' table_name <- "birdtracks"
#' csv_to_sqlite(
#'   csv_file = csv.name,
#'   sqlite_file = sqlite_file,
#'   table_name = table_name
#' )
#' # Get access to SQLite database
#' my_db <- src_sqlite(sqlite_file, create = FALSE)
#' bird_tracking <- tbl(my_db, "birdtracks")
#' # Example query via dplyr
#' results <- bird_tracking %>%
#'   filter(device_info_serial == 860) %>%
#'   select(date_time, latitude, longitude, altitude) %>%
#'   filter(date_time < "2014-07-01") %>%
#'   filter(date_time > "2014-03-01") %>%
#'   as_tibble()
#' head(results)
#' }
#' @export
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite dbWriteTable
#' @importFrom readr read_delim read_delim_chunked
#' @importFrom dplyr %>% select_if mutate_at
#' @importFrom lubridate is.Date is.POSIXt
csv_to_sqlite <- function(csv_file, sqlite_file, table_name,
                          delim = ",",
                          pre_process_size = 1000, chunk_size = 50000,
                          show_progress_bar = TRUE, ...) {
  con <- dbConnect(SQLite(), dbname = sqlite_file)

  # read a first chunk of data to extract the colnames and types
  # to figure out the date and the datetime columns
  args <- list(file = csv_file, delim = delim, n_max = pre_process_size, ...)
  args <- args[!duplicated(names(args))]
  skip_plus_one <- !(isFALSE(args$col_names) | is.character(args$col_names))
  df <- do.call(read_delim, args)
  date_cols <- df %>%
    select_if(is.Date) %>%
    colnames()
  datetime_cols <- df %>%
    select_if(is.POSIXt) %>%
    colnames()

  # write the first batch of lines to SQLITE table, converting dates to string
  # representation
  df2 <- df %>%
    mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
    mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
  dbWriteTable(con, table_name, df2, overwrite = TRUE)

  # readr chunk functionality

  args <- list(
    file = csv_file,
    callback = append_to_sqlite(
      con = con, table_name = table_name,
      date_cols = date_cols,
      datetime_cols = datetime_cols
    ),
    delim = delim,
    skip = pre_process_size + skip_plus_one,
    chunk_size = chunk_size,
    progress = show_progress_bar,
    col_names = names(attr(df, "spec")$cols),
    ...
  )
  args <- args[!duplicated(names(args))]

  do.call(read_delim_chunked, args)

  dbDisconnect(con)
}

#' Callback function that appends new sections to the `SQLite` table.
#' @param con A valid connection to `SQLite` database.
#' @param table_name Name of the table to store the data table in the `sqlite`
#'   database.
#' @param date_cols Name of columns containing Date objects
#' @param datetime_cols Name of columns containing `POSIXt` objects.
#'
#' @keywords internal
append_to_sqlite <- function(con, table_name,
                             date_cols, datetime_cols) {
  #' @param x Data.frame we are reading from.
  function(x, pos) {
    x <- as.data.frame(x)
    x <- x %>%
      mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
      mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
    # append data frame to table
    dbWriteTable(con, table_name, x, append = TRUE)
  }
}
