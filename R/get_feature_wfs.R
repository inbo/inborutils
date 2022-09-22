#' Get a layer from a web feature service
#'
#' This function constructs a `URL` request from its arguments and either reads
#' in the resulting vector layer as a `sf` object or returns the number of
#' features that are requested.
#' The request is made up of key-value pairs and additional key-value pairs can
#' be passed to the function.
#' The full documentation for the `WFS` standard can be consulted from
#' https://www.ogc.org/standards/wfs
#'
#' @param wfs Web address for the service which you want to query features from
#' @param version Version number for the service.
#' For instance `"2.0.0"`.
#' @param layername the name of a layer hosted by the web feature service
#' @param crs coordinate reference system to represent the features.
#' For instance `"EPSG:31370"`.
#' @param bbox Optional bounding box.
#' Pass this as a named vector with names "xmin", "xmax", "ymin", "ymax".
#' @param filter Optional [standard OGC filter]
#' (https://www.ogc.org/standards/filter) specification
#' @param cql_filter Optional [Contextual Query Language]
#' (https://portal.ogc.org/files/96288) filter.
#' This currently only works if the `WFS` is hosted on a `GeoServer`.
#' @param output_format Optional output format supported by the `WFS`.
#' @param property_name Optional character string.
#' Which fields or columns to return?
#' If you want to specify multiple columns, separate them by a comma.
#' The column containing the feature geometry is usually called `geom`,
#' `geometry` or `SHAPE`.
#' @param result_type For version `"2.x.x"`, this can be either `"results"`
#' (default) or `"hits"`.
#' The former returns the requested features, the latter returns the number of
#' requested features.
#' @param ... Additional key-value pairs passed on to the WFS query.
#'
#' @importFrom httr parse_url build_url GET content
#' @importFrom sf read_sf
#' @importFrom xml2 as_list
#' @importFrom assertthat assert_that is.string
#'
#' @details See https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/
#'  for more information.
#' @family GIS_utilities
#'
#' @export
#' @examples
#' \dontrun{
#' vlaanderen <- get_feature_wfs(
#' wfs = paste0("https://eservices.minfin.fgov.be/",
#'              "arcgis/services/R2C/Regions/MapServer/WFSServer"),
#' layername = "regions",
#' crs = "EPSG:31370",
#' filter = paste0("<Filter><PropertyIsEqualTo><PropertyName>",
#'                 "regions:NameDUT</PropertyName><Literal>'Vlaams Gewest'",
#'                 "</Literal></PropertyIsEqualTo></Filter>"))
#' }
get_feature_wfs <- function(
  wfs,
  version = "2.0.0",
  layername,
  crs,
  bbox = NULL,
  filter = NULL,
  cql_filter = NULL,
  output_format = NULL,
  property_name = NULL,
  result_type = c("results", "hits"),
  ...
  ) {
  result_type <- match.arg(result_type)
  url <- parse_url(wfs)
  assert_that(grepl("\\d\\.\\d\\.\\d", version))
  assert_that(grepl("EPSG:\\d+", crs))
  if (!is.null(bbox)) {
    assert_that(length(bbox) == 4)
    assert_that(all(names(bbox) %in% c("xmin", "xmax", "ymin", "ymax")))
    bbox <- paste(
      bbox[["xmin"]],
      bbox[["ymin"]],
      bbox[["xmax"]],
      bbox[["ymax"]],
      sep = ",")
  }
  if (grepl(pattern = "^2", x = version)) {
    url$query <- list(service = "wfs",
                      request = "GetFeature",
                      version = version,
                      typeNames = layername,
                      srsName = crs,
                      bbox = bbox,
                      filter = filter,
                      cql_filter = cql_filter,
                      outputFormat = output_format,
                      propertyName = property_name,
                      resultType = result_type,
                      ...
    )
  }
  if (grepl(pattern = "^1", x = version)) {
    url$query <- list(service = "wfs",
                      request = "GetFeature",
                      version = version,
                      typeName = layername,
                      srsName = crs,
                      bbox = bbox,
                      filter = filter,
                      cql_filter = cql_filter,
                      outputFormat = output_format,
                      propertyName = property_name,
                      resultType = NULL,
                      ...
    )
  }

  request <- build_url(url)

  result <- GET(request)
  parsed <- as_list(content(result, "parsed", encoding = "UTF-8"))

  if (names(parsed) == "ExceptionReport") {
    message <- unlist(parsed$ExceptionReport$Exception$ExceptionText)
    old_op <- options(warning.length = max(nchar(message), 1000))
    on.exit(options(old_op))
    stop(sprintf(paste0(message, "\nThe requested url was: %s"),
                 request))
  }

  if (result_type == "hits") {
    n_features <- attr(parsed$FeatureCollection, "numberMatched")
    return(n_features)
  } else {
    result <- read_sf(request)
    return(result)
  }
}
