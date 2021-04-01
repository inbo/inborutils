#' Extract date and coordinate information from Google Maps kml file
#'
#' @param filename (char) filename/path of the kml file
#'
#' @return df with columns datetime, x (numeric), y (numeric)
#' @export
#'
#' @importFrom tidyr separate extract
#' @importFrom rlang .data
#' @importFrom lubridate dmy_hm
#'
#' @family download_functions
#'
#' @examples
#' \dontrun{
#' df <- read_kml_file("BE1002.kml")
#' }
read_kml_file <- function(filename) {
    kml_text <- readLines(filename)
    coords <- grep("<coordinates>", kml_text)
    coord <- as.data.frame(kml_text[coords + 1])
    colnames(coord) <- c("ruw")
    coords <- coord %>%
        separate(.data$ruw, c("x", "y", "z"), ",")

    date_ids <- grep("<name>", kml_text)
    dates_raw <- kml_text[date_ids][-1]
    dates_matched <- regmatches(dates_raw,
                        regexpr("[0-9]{2}/[0-9]{2}/[0-9]{4}.*[0-9]{2}:[0-9]{2}",
                                dates_raw))
    dates <- dmy_hm(dates_matched)
    datetime <- as.data.frame(dates)
    colnames(datetime) <- c("datetime")

    data <- cbind(datetime, coords[,1:2])
    data$x <- as.numeric(data$x)
    data$y <- as.numeric(data$y)
    return(data)
}



