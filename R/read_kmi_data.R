
#' Load KMI data as data.frame
#'
#' @param filename path/filename of the KMI datafile
#' @param n_max int shortcut for testing, loading just a section of the data
#'
#' @return data.frame with the headers location_name, datetime (UTC), value,
#' unit,variable_name, latitude, longitude and source_filename
#'
#' @export
#' @importFrom readr read_delim cols col_character col_double col_integer
#' col_datetime
#' @importFrom dplyr select mutate %>%
#' @importFrom rlang .data
#' @importFrom lubridate tz
#'
#' @family download functions
#'
#' @examples
#' \dontrun{
#' # see vignettes for more examples
#' file_path <- "kmi_file.txt"
#' precipation_kmi <- read_kmi_data(file_path)
#' }
read_kmi_data <- function(filename, n_max = Inf) {
    col_types <- cols(date = col_datetime("%Y-%m-%d_%H"),
                      JAAR = col_integer(),
                      MAAND = col_integer(),
                      DAG = col_integer(),
                      UUR = col_integer(),
                      STATION = col_character(),
                      `NEERSLAG(mm)` = col_double()
    )
    raw_data <- read_delim(filename, delim = ";",
                           col_names = TRUE,
                           col_types = col_types,
                           n_max = n_max)

    # currently assumes that data is already UTC time zone (default of readr)
    # currently assumes KMI data is ALWAYS precipitation data...
    sprintf("Time zone of data is %s", tz(raw_data$date))
    kmi_data <- raw_data %>%
        select(datetime = .data$date,
               location_name = .data$STATION,
               value = .data$`NEERSLAG(mm)`) %>%
        mutate(unit = "mm") %>%
        mutate(variable_name = "precipitation") %>%
        mutate(source_filename = basename(filename)) %>%
        mutate(quality_code = "")

    kmi_data
}

