
#' Load RWS data
#'
#' Currently, the function supports conductivity and water temperature data
#' files
#'
#' @param filename path/filename of the RWS datafile
#' @param n_max int shortcut for testing, loading just a section of the data
#'
#' @return data.frame with the headers location_name, datetime (UTC), value,
#' unit,variable_name, latitude, longitude and source_filename
#'
#' @export
#' @importFrom readr read_delim cols col_character col_number read_lines
#' @importFrom dplyr select mutate %>%
#' @importFrom assertthat are_equal
#' @importFrom lubridate force_tz dmy_hm
#' @importFrom rlang .data
#'
read_rws_data <- function(filename, n_max = Inf) {

    # Extract variable info
    rws_con <- file(filename, "rb")
    header <- read_lines(rws_con, n_max = 1)
    close(rws_con)

    # Extract location_name from file
    file_only <- basename(filename)
    location_name <- tolower(strsplit(file_only, "_")[[1]][[1]])
    var_name <- tolower(strsplit(strsplit(file_only, "_")[[1]][[2]],
                                 "\\.")[[1]][[1]])

    # prepare reading in function of variable type in header
    if (grepl("temp", header)) {
        # temperature is given in 0.1 °C
        variable_longname <- "water_temperature"
        variable_shortname <- "temp"
        variable_unit <- "degrees celsius"
        variable_factor <- 0.1 # recalculation factor
        col_types <- cols(
            Date = col_character(),
            Time = col_character(),
            temp = col_number()
        )
    } else if (grepl("cond", header)) {
        # conductivity is given in 0.01 S/m
        variable_longname <- "conductivity"
        variable_shortname <- "cond"
        variable_unit <- "" # TODO, still unclear
        variable_factor <- 10. # recalculation factor
        col_types <- cols(
            Date = col_character(),
            Time = col_character(),
            cond = col_number()
        )
    } else {
        stop("Header contains unknown variable for RWS parser")
    }

    # Note: RWS uses a * to define missing values
    rws_data <- read_delim(filename, delim = ", ", trim_ws = TRUE,
                           n_max = n_max, col_types = col_types,
                           na = "*")

    rws_data <- rws_data %>%
        # for RWS, the MET is default time zone: convert to UTC with force_tz
        mutate(datetime = force_tz(dmy_hm(paste(.data$Date,
                                                .data$Time)),
                                   tzone = "UTC")) %>%
        select(.data$datetime, value = variable_shortname) %>%
        mutate(value = .data$value * variable_factor) %>%
        mutate(unit = variable_unit) %>%
        mutate(variable_name = variable_longname) %>%
        mutate(location_name = location_name) %>%
        mutate(source_filename = basename(filename)) %>%
        mutate(quality_code = "")

    rws_data

}
