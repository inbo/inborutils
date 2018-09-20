library(tidyr)


#' Load
#'
#' @param filename (char)
#'
#' @return
#' @export
#'
#' @examples
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
    time$time <- paste(time$hours,time$minutes, sep = ":")
    uur <- time[,3]

    gegevens <- cbind(datum,uur,coords[,1:2])
    gegevens$x <- as.numeric(gegevens$x)
    gegevens$y <- as.numeric(gegevens$y)
    return(gegevens)
}


