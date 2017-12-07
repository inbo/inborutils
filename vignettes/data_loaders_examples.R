## ------------------------------------------------------------------------
library(inborutils)

## ----eval=FALSE, include=TRUE--------------------------------------------
#  response <- download_knmi_data_hour(c(310, 319), "PRCP",
#                                      "2012-01-01", "2012-02-01",
#                                      output_file = "knmi_download.csv")

## ----eval=FALSE, include=TRUE--------------------------------------------
#  rain_knmi_2012 <- read_knmi_rainfall("./knmi_download.csv")

## ----include=FALSE-------------------------------------------------------
rain_knmi_2012 <- inborutils::rain_knmi_2012

## ------------------------------------------------------------------------
head(rain_knmi_2012)

## ----fig.width = 7-------------------------------------------------------
library(ggplot2)
ggplot(rain_knmi_2012, aes(x = datetime, y = value)) + 
    geom_line() + 
    xlab("") + ylab("mm")

## ----include=FALSE-------------------------------------------------------
fpath <- system.file("extdata", "mow_example.txt", package = "inborutils")

## ----eval=FALSE, include=TRUE--------------------------------------------
#  fpath <- "mow_example.txt"

## ------------------------------------------------------------------------
conductivity_mow <- read_mow_data(fpath)
head(conductivity_mow)

## ----fig.width = 7-------------------------------------------------------
library(ggplot2)
ggplot(conductivity_mow, aes(x = datetime, y = value)) + 
    geom_line() + 
    xlab("") + ylab("µS/cm") + 
    scale_x_datetime(date_labels = "%H:%M\n%Y-%m-%d", date_breaks = "4 hours")

## ----include=FALSE-------------------------------------------------------
rpath <- system.file("extdata", "kmi_example.txt", package = "inborutils")

## ----eval=FALSE, include=TRUE--------------------------------------------
#  rpath <- "kmi_example.txt"

## ------------------------------------------------------------------------
rainfall_kmi <- read_kmi_data(rpath)
head(rainfall_kmi)

