#' Get data from a Zenodo archive
#'
#' This function will download an entire archive from Zenodo
#' (\url{https://zenodo.org}).
#' It only works for Zenodo created DOI (not when the DOI is for
#' example derived from Zookeys.)
#'
#' @author Hans Van Calster, \email{hans.vancalster@@inbo.be}
#' @author Floris Vanderhaeghe, \email{floris.vanderhaeghe@@inbo.be}
#'
#' @param path Path where the data must be downloaded.
#' Defaults to the working directory.
#' @param doi a doi pointer to the Zenodo archive starting with
#' '10.5281/zenodo.'.
#' See examples.
#' @param parallel Logical.
#' If \code{TRUE} (the default), files will be
#' downloaded concurrently for multi-file records.
#' Of course, the operation is limited by bandwidth and traffic limitations.
#' @param quiet Logical (\code{FALSE} by default).
#' Do you want to suppress informative messages (not warnings)?
#'
#' @importFrom stringr
#' fixed
#' str_remove
#' str_split
#' str_match
#' @importFrom curl curl_fetch_memory curl_download
#' @importFrom jsonlite fromJSON
#' @importFrom tools md5sum
#' @importFrom utils tail
#' @importFrom assertthat
#' assert_that
#' is.string
#' is.flag
#' noNA
#'
#' @export
#' @family download_functions
#'
#' @examples
#' \dontrun{
#' # Example download of an archive containing a single zip
#' download_zenodo(doi = "10.5281/zenodo.1283345")
#' download_zenodo(doi = "10.5281/zenodo.1283345", quiet = TRUE)
#' # Example download of an archive containing multiple files
#' # using parallel download
#' # (multiple files will be simultaneously downloaded)
#' download_zenodo(doi = "10.5281/zenodo.1172801", parallel = TRUE)
#' # Example download of an archive containing a single pdf file
#' download_zenodo(doi = "10.5281/zenodo.168478")
#' }
download_zenodo <- function(doi,
                            path = ".",
                            parallel = TRUE,
                            quiet = FALSE) {
  assert_that(is.string(doi), is.string(path))
  assert_that(is.flag(parallel), noNA(parallel), is.flag(quiet), noNA(quiet))

  # check for existence of the folder
  stopifnot(dir.exists(path))

  record <- str_remove(doi, fixed("10.5281/zenodo."))

  # Retrieve file name by records call
  base_url <- "https://zenodo.org/api/records/"
  req <- curl_fetch_memory(paste0(base_url, record))
  content <- fromJSON(rawToChar(req$content))

  # Calculate total file size
  totalsize <- sum(content$files$size) %>%
    human_filesize()

  # extract individual file names and urls
  file_urls <- content$files$links$self
  filenames <- basename(content$files$key)
  destfiles <- file.path(path, filenames)

  # extract check-sum(s)
  file_md5 <- content$files$checksum

  # download files
  if (!quiet) {
    message(
      "Will download ",
      (nrfiles <- length(filenames)),
      " file",
      ifelse(nrfiles > 1, "s", ""),
      " (total size: ",
      totalsize,
      ") from https://doi.org/",
      doi,
      " (",
      content$metadata$title,
      "; version: ",
      ifelse(!is.null(content$metadata$version),
        content$metadata$version,
        content$metadata$relations$version[1, 1]
      ),
      ")\n"
    )
  }

  if (length(file_urls) > 1 && parallel) {
    curl::multi_download(
      urls = file_urls,
      destfiles = destfiles,
      progress = !quiet
    )
  } else {
    mapply(curl_download,
      file_urls,
      destfiles,
      MoreArgs = list(quiet = quiet)
    )
  }

  # check each of the files

  if (!quiet) message("\nVerifying file integrity...\n")

  for (i in seq_along(file_urls)) {
    filename <- filenames[i]
    destfile <- destfiles[i]
    md5 <- unname(md5sum(destfile))
    zenodo_md5 <- str_split(file_md5[i], ":")[[1]][2]
    if (identical(md5, zenodo_md5)) {
      if (!quiet) {
        message(
          filename,
          " was downloaded and its integrity verified (md5sum: ",
          md5,
          ")"
        )
      }
    } else {
      warning(
        "Incorrect download! md5sum ",
        md5,
        " for file",
        filename,
        " does not match the Zenodo archived md5sum ",
        zenodo_md5
      )
    }
  }
}



#' Human-readable binary file size
#'
#' Takes an integer (referring to number of bytes) and returns an optimally
#' human-readable
#' \href{https://en.wikipedia.org/wiki/Binary_prefix}{binary-prefixed}
#' byte size (KiB, MiB, GiB, TiB, PiB, EiB).
#' The function is vectorised.
#'
#' @author Floris Vanderhaeghe, \email{floris.vanderhaeghe@@inbo.be}
#'
#' @param x A positive integer, i.e. the number of bytes (B).
#' Can be a vector of file sizes.
#'
#' @return
#' A character vector.
#'
#' @examples
#' human_filesize(7845691)
#' v <- c(12345, 456987745621258)
#' human_filesize(v)
#'
#' @family Helpers
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' @importFrom dplyr
#' %>%
human_filesize <- function(x) {
  assert_that(is.numeric(x))
  assert_that(all(x %% 1 == 0 & x >= 0))
  magnitude <-
    log(x, base = 1024) %>%
    floor() %>%
    pmin(8)
  unit <- factor(magnitude,
    levels = 0:8,
    labels = c(
      "B",
      "KiB",
      "MiB",
      "GiB",
      "TiB",
      "PiB",
      "EiB",
      "ZiB",
      "YiB"
    )
  )
  size <- (x / 1024^magnitude) %>% round(1)
  return(paste(size, unit))
}
