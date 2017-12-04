
#' Read KNMI rainfall datafile as data.frame
#'
#' Remark that this function is specifically to extract the rainfall (RH) data,
#' coordinates are extracted from the header of the KNMI data file
#'
#' @param filename path/filename of the KMI datafile
#' @param n_max int shortcut for testing, loading just a section of the data
#'
#' @return data.frame with the headers location_name, datetime (UTC), value,
#' unit,variable_name, latitude, longitude and source_filename
#'
#' @export
#' @importFrom iterators ireadLines nextElem
#' @importFrom readr read_delim cols col_character col_number
#' @importFrom dplyr select_ mutate %>% mutate_
#' @importFrom lubridate ymd_h
#'
read_knmi_rainfall <- function(filename, n_max = Inf) {
    # Extract data from header by reading the header lines
    knmi_con <- file(filename, "r")
    header_it <- ireadLines(knmi_con)
    coord_lines <- FALSE
    line <- nextElem(header_it)
    while (startsWith(line, "#")) {
        # extract station coordinate info
        if (coord_lines) {
            if (line == "# ") {
                coord_lines <- FALSE
            } else {
                station <- strsplit(gsub("# ", "", line), "[ ]+")[[1]][5]
                coordinate_info[[station]] <- strsplit(gsub("# |:", "", line),
                                                       "[ ]+")[[1]]
            }
        }
        if (grepl("LON.*LAT", line)) {
            coord_lines <- TRUE
            coordinate_header <- strsplit(gsub("# ", "", line), "[ ]+")[[1]]
            coordinate_info <- list()
        }

        # extract header info
        if (grepl("STN,YYYYMMDD", line)) {
            header <- strsplit(gsub("#? ", "", line), ",")[[1]]
        }
        line <- nextElem(header_it)
    }

    close(knmi_con)

    # convert coordinates information to mergeble data.frame
    coordinates <- as.data.frame(coordinate_info, row.names = coordinate_header)
    coordinates <- coordinates %>%
        t() %>% as.data.frame() %>%
        select_(station = quote(STN), longitude = "LON(east)",
                latitude = "LAT(north)", location_name = quote(NAME))

    # Read the KNMI data format
    col_types <- cols(
        STN = col_character(),
        YYYYMMDD = col_character(),
        HH = col_character(),
        DR = col_number(),
        RH = col_number()
    )
    knmi_data <- read_delim(filename, delim = ", ", comment = "#",
                            trim_ws = TRUE, col_names = header,
                            n_max = n_max, col_types = col_types)

    knmi_data %>%
        mutate(datetime = ymd_h(paste(~YYYYMMDD, ~HH))) %>%
        select_(station = quote(STN), value = quote(RH), quote(datetime)) %>%
        mutate(unit = "mm") %>%
        mutate_(value = ifelse(~value == -1, -1, ~value/10.)) %>%
        mutate(variable_name = "precipitation") %>%
        merge(coordinates, by = "station") %>%
        mutate_(source_filename = basename(~filename)) %>%
        select_(-~station) %>%
        mutate(quality_code = "")

}
