library(tidyr)

setwd("../data/kml_example_data")
fileNames <- Sys.glob("*.kml")

for (fileName in fileNames) {
  kml.text <- readLines(fileName)
  coords <- grep("<coordinates>",kml.text)
  coord <- as.data.frame(kml.text[coords+1])
  colnames(coord) <- c("ruw")
  coords <- coord %>%
    separate(ruw, c("x", "y", "z"), ",")
  
  dates <- grep("<name>",kml.text)
  date <- as.data.frame(kml.text[dates])
  date <- date[-1,]
  date <- as.data.frame(date)
  colnames(date) <- c("ruw")
  datum <- as.data.frame(substr(date$ruw, 13,22))
  colnames(datum) <- c("datum")
  time <- extract(date, c("ruw"), into=c("hours","minutes"), "([[:digit:]]+):([[:digit:]]+)")
  time$time <- paste(time$hours,time$minutes, sep=":")
  uur<- time[,3]
  
  Gegevens <- cbind(datum,uur,coords[,1:2])
  Gegevens$x <- as.numeric(Gegevens$x)
  Gegevens$y <- as.numeric(Gegevens$y)
  fileName <- substr(fileName,1,6)
  Gegevens$bever <- fileName
  write.csv2(Gegevens, file=sprintf('../csv_example_data/%s.csv', fileName), row.names = F)
}


