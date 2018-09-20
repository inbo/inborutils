#' Extract date and coordinate information from Google Maps kml file
#'
#' @param filename (char) filename/path of the kml file
#'
#' @return df with columns date (as chars), hour (as char), x (numeric),
#'     y (numeric)
#' @export
#'
#' @importFrom tidyr separate extract
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' df <- read_kml_file("BE1002.kml")
#' }
read_kml_file <- function(filename) {
    kml.text <- readLines(filename)
    coords <- grep("<coordinates>", kml.text)
    coord <- as.data.frame(kml.text[coords + 1])
    colnames(coord) <- c("ruw")
    coords <- coord %>%
        separate(.data$ruw, c("x", "y", "z"), ",")

    dates <- grep("<name>", kml.text)
    dates <- as.data.frame(kml.text[dates])
    dates <- dates[-1,]
    dates <- as.data.frame(dates)
    colnames(dates) <- c("ruw")
    date <- as.data.frame(substr(dates$ruw, 13, 22))
    colnames(date) <- c("date")
    time <- extract(dates, c("ruw"), into = c("hours", "minutes"),
                    "([[:digit:]]+):([[:digit:]]+)")
    time$time <- paste(time$hours, time$minutes, sep = ":")
    hour <- time[,3]

    data <- cbind(date, hour, coords[,1:2])
    data$x <- as.numeric(data$x)
    data$y <- as.numeric(data$y)
    return(data)
}




