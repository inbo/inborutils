
#' Read MOW data
#'
#' Coordinate data is contained in the header of the file
#'
#' @param filename path/filename of the RWS datafile
#' @param n_max int shortcut for testing, loading just a section of the data
#'
#' @return data.frame with the headers location_name, datetime (UTC), value,
#' unit,variable_name, latitude, longitude and source_filename
#'
#' @export
#' @importFrom readr read_delim cols col_character col_double
#' @importFrom dplyr select mutate %>% mutate_ select_
#' @importFrom iterators ireadLines nextElem
#' @importFrom sp CRS
#' @importFrom lubridate dmy_hms
#'
read_mow_data <- function(filename, n_max = Inf) {
    # Extract data from header by reading the header lines
    # Apparently, the encoding for MOW data is not UTF8, the ISO is derived from
    # the way mu was rendered in the conductivity files
    mow_con <- file(filename, "r", encoding = "ISO-8859-15")
    header_it <- ireadLines(mow_con)
    line <- nextElem(header_it)
    cnt <- 1
    while (!startsWith(line, "Date")) {
        # extract station coordinate info
        if (startsWith(line, "Station Name")) station_name <- mow_header_split(line)
        if (startsWith(line, "Easting")) longitude <- as.numeric(mow_header_split(line))
        if (startsWith(line, "Northing")) latitude <- as.numeric(mow_header_split(line))
        if (startsWith(line, "Parameter Name")) variable_name <- mow_header_split(line)
        if (startsWith(line, "Time series Unit")) variable_unit <- mow_header_split(line)
        line <- nextElem(header_it)
        cnt <- cnt + 1
    }
    header <- strsplit(line, "\t")[[1]]
    close(mow_con)

    # Handle coordinate information
    crs_lambert <- CRS("+init=epsg:31370")
    crs_wgs84 <- CRS("+init=epsg:4326")
    coordinates <- as.data.frame(list(latitude = latitude,
                                      longitude = longitude))
    coordinates_wgs <- reproject_coordinates(coordinates, "longitude",
                                             "latitude", crs_lambert, crs_wgs84)

    # Define mapping for mow supported values
    variable_mapping <- list("DO" = "dissolved_oxygen",
                             "Turb_YSI" = "turbidity_ySI",
                             "Turb_SG5" = "turbidity_SG5",
                             "Turb_SG25" = "turbidity_SG25",
                             "Cond" = "conductivity",
                             "T\u00B0" = "water_temperature")
    variable_name_mapped <- variable_mapping[[variable_name]]
    if (is.null(variable_name_mapped)) {
        variable_name_mapped <- variable_name
        print(sprintf("Variable name is not mapped, using %s as variable name",
                      variable_name_mapped))
    }

    # Read data.frame from the file itself
    col_types <- cols(
        X1 = col_character(),
        X2 = col_character(),
        X3 = col_double(),
        X4 = col_character(),
        X5 = col_character()
    )
    mow_data <- read_delim(filename, delim = "\t", skip = cnt,
                           n_max = n_max, col_names = FALSE,
                           col_types = col_types)
    colnames(mow_data) <- header
    mow_data %>%
        mutate_(datetime = dmy_hms(paste(~Date, ~Time))) %>% # UTC by default
        select_(~datetime, value = header[[3]],
               quality_code = "Quality flag") %>%
        mutate(location_name = station_name) %>%
        mutate(variable_name = variable_name_mapped) %>%
        mutate(unit = variable_unit) %>%
        mutate(longitude = coordinates_wgs$longitude) %>%
        mutate(latitude = coordinates_wgs$latitude) %>%
        mutate(source_filename = basename(filename))

}


#' Get data entry from MOW header data file
#'
#' @param inputline character string coming from MOW data file
#'
#' @return char string with the entry
mow_header_split <- function(inputline) {
    trimws(strsplit(inputline, ":")[[1]][[2]])
}
