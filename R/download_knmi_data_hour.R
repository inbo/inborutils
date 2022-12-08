#' Download KNMI hourly data to file
#'
#' More info, zie
#' https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script # nolint
#'
#' @param stations list of integers. For an overview,
#' see https://www.daggegevens.knmi.nl/klimatologie/uurgegevens
#' @param variables list of variables, options are:
#' \itemize{
#'  \item{"WIND = DD:FH:FF:FX"}{Wind}
#'  \item{"TEMP = T:T10N:TD"}{Temperature}
#'  \item{"SUNR = SQ:Q"}{Sunshine duration and global radiation}
#'  \item{"PRCP = DR:RH"}{Precipitation and potential evaporation}
#'  \item{"VICL = VV:N:U"}{Visibility, cloud cover and relative humidity}
#'  \item{"WEER = M:R:S:O:Y:WW"}{Weather phenomena, weather types}
#'  \item{"ALL"}{all variables}
#' }
#'
#' @param start_date date interpretable string to define start of the period
#' @param end_date date interpretable string to define end of the period
#' @param output_file path/filename of the output_file_name
#'
#' @return response containing, status_code, content,...
#'
#' @export
#'
#' @importFrom lubridate parse_date_time hour
#' @importFrom httr POST content
#'
#' @family download_functions
#'
#' @examples
#' \dontrun{
#' download_knmi_data_hour(c(310, 319), "PRCP", "2011-01-01", "2012-02-17",
#'   output_file = "knmi_output_download.txt"
#' )
#' }
download_knmi_data_hour <- function(stations, variables, start_date, end_date,
                                    output_file = "knmi_download.txt") {
  start_date <- parse_date_time(start_date,
    orders = c("ymd_HMS", "ymd", "ym", "y")
  )
  end_date <- parse_date_time(end_date,
    orders = c("ymd_HMS", "ymd", "ym", "y")
  )
  lubridate::hour(end_date) <- 23 # make sure to have all hours of the day

  res <- POST(
    url = "https://www.daggegevens.knmi.nl/klimatologie/uurgegevens",
    body = list(
      stns = paste(stations, collapse = ":"),
      vars = paste(variables, collapse = ":"),
      start = format(start_date, "%Y%m%d%H"),
      end = format(end_date, "%Y%m%d%H")
    )
  )

  write(content(res, "text", encoding = "UTF-8"), output_file)

  res
}
