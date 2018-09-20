
#' Extract date and coordinate information from Google Maps kml file
#'
#' @param filename (char) filename/path of the kml file
#'
#' @return df with columns datum
#' @export
#'
#' @importFrom tidyr separate
#'
#' @examples
#' df <- read_kml_file("data/kml_example_data/BE1002.kml")
read_kml_file <- function(filename) {
    kml.text <- readLines(filename)
    coords <- grep("<coordinates>", kml.text)
    coord <- as.data.frame(kml.text[coords + 1])
    colnames(coord) <- c("ruw")
    coords <- coord %>%
        separate(ruw, c("x", "y", "z"), ",")

    dates <- grep("<name>", kml.text)
    date <- as.data.frame(kml.text[dates])
    date <- date[-1,]
    date <- as.data.frame(date)
    colnames(date) <- c("ruw")
    datum <- as.data.frame(substr(date$ruw, 13,22))
    colnames(datum) <- c("datum")
    time <- extract(date, c("ruw"), into = c("hours", "minutes"),
                    "([[:digit:]]+):([[:digit:]]+)")
    time$time <- paste(time$hours, time$minutes, sep = ":")
    hour <- time[,3]

    data <- cbind(datum, hour, coords[,1:2])
    data$x <- as.numeric(data$x)
    data$y <- as.numeric(data$y)
    return(data)
}




