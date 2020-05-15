# Coding club 23-01-2018
#
# S. Van Hoey
#

# ------------------------
# DATASET:
# RAINFALL DATA KLEMSKERKE
# ------------------------

library(readr)
library(dplyr)

# First try:
klemskerke <- read_delim("../data/20180123/20180123_rainfall_klemskerke.csv",
                         delim = ";", skip = 7)
# * Value interpret as string, since `,` usage as decimal sign..
# * timestamp proper interpreted

# OPTION 1: I want to use the . decmal sign and keep it as such, adapt Value
# --------------------------------------------------------------------------
klemskerke <- read_delim("../data/20180123/20180123_rainfall_klemskerke.csv",
                         delim = ";", skip = 7)
klemskerke <- klemskerke %>%
    select(datetime = `#Timestamp`, value = Value,
           quality_code = `Quality Code`) %>%   # select/rename the column names
    mutate(value = gsub(",", ".", value)) %>%   # make , -> .
    mutate(value = as.numeric(value))           # convert to numeric
head(klemskerke)

# OPTION 2: using functions of the data import cheat sheet
# ---------------------------------------------------------
klemskerke <- read_delim("../data/20180123/20180123_rainfall_klemskerke.csv",
                         delim = ";", skip = 7)
klemskerke <- klemskerke %>%
    select(datetime = `#Timestamp`, value = Value,
           quality_code = `Quality Code`) %>%
    mutate(value = parse_double(value, locale = locale(decimal_mark = ",")))
head(klemskerke)

# DROP  NA values
# ---------------
library(tidyr)
klemskerke <- drop_na(klemskerke)

# basic line plot
# ---------------
library(ggplot2)

ggplot(klemskerke, mapping = aes(datetime, value)) +
    geom_line() +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    xlab("") +
    ylab("rainfall (mm)")

# bar plot of the yearly rainfall sum
# -----------------------------------

year_sum <- klemskerke %>%
    filter(datetime >= as.Date("2012-01-01") &
               datetime < as.Date("2017-01-01")) %>%
    group_by(year = lubridate::floor_date(datetime, "year")) %>%
    summarize(value = sum(value))

ggplot(year_sum, mapping = aes(year, value)) +
    geom_bar(stat = "identity") +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    xlab("") +
    ylab("rainfall (mm)")

# Using the custom load function
# ------------------------------
source("./20180123_vmm_function_loader.R")
klemskerke <- load_waterinfo_data("../data/20180123/20180123_rainfall_klemskerke.csv")


# --------------------------
# DATASET:
# GHENT REGIONAL GROWTH DATA
# --------------------------

# read the data
bevolking <- readr::read_delim("../data/20180123/20180123_gent_groeiperwijk.csv",
                               delim = ";")

# convert to a tidy data set (see cheatsheet for functions)
tidy_bevolking <- bevolking %>%
    gather(key = "year", value = "growth", -wijk) %>%
    separate(year, into = c("dummy", "year")) %>%
    mutate(year = parse_integer(year)) %>%
    select(-dummy)

# heatmap of the data
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
ggplot(tidy_bevolking, aes(factor(year), wijk, fill = growth)) +
    geom_tile(colour = "white") +
    theme_minimal(base_size = 14) +
    scale_fill_distiller(palette = "RdBu", type = "div", direction = 1) +
    labs(x = "", y = "")

# individual plot of the evolution for each region separate
ggplot(tidy_bevolking, aes(x = factor(year), y = growth)) +
    geom_bar(stat = "identity") +
    facet_wrap("wijk") +
    scale_x_discrete(breaks = c("1999", "2004", "2009")) +
    xlab("")

