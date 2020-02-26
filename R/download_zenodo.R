#' Get data from a Zenodo archive
#'
#' This function will download an entire archive from Zenodo (\url{https://zenodo.org}).
#' It only works for Zenodo created DOI (not when the DOI is for
#' example derived from Zookeys.)
#'
#' @param path Path where the data must be downloaded.
#' Defaults to the working directory.
#' @param doi a doi pointer to the Zenodo archive starting with '10.5281/zenodo.'. See examples.
#' @param parallel Logical (\code{FALSE} by default).
#' If \code{TRUE}, will run a number of parallel processes, each downloading
#' another file.
#' This is useful when multiple large files are present in the Zenodo
#' record, which otherwise would be downloaded sequentially.
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
#' @importFrom parallel
#' makeCluster
#' clusterMap
#' stopCluster
#'
#' @export
#' @family download functions
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
                            parallel = FALSE,
                            quiet = FALSE) {

  assert_that(is.string(doi), is.string(path))
  assert_that(is.flag(parallel), noNA(parallel), is.flag(quiet), noNA(quiet))

  # check for existence of the folder
  stopifnot(dir.exists(path))

  record <- str_remove(doi, fixed("10.5281/zenodo."))

  # Retrieve file name by records call
  base_url <- 'https://zenodo.org/api/records/'
  req <- curl_fetch_memory(paste0(base_url, record))
  content <- fromJSON(rawToChar(req$content))

  # Calculate total file size
  totalsize <- sum(content$files$size) %>%
                human_filesize()

  # extract individual file names and urls
  file_urls <- content$files$links$self
  filenames <- str_match(file_urls, ".+/([^/]+)")[,2]
  destfiles <- file.path(path, filenames)

  # extract check-sum(s)
  file_md5 <- content$files$checksum

  # download files
  if (!quiet) {
  message("Will download ",
          length(filenames),
          " files (total size: ",
          totalsize,
          ") from https://doi.org/",
          doi,
          " (",
          content$metadata$title,
          "; version: ",
          content$metadata$version,
          ")\n"
  )
  }

  if (parallel) {

    nr_nodes <- min(10, length(file_urls))

    if (!quiet) message("Initializing parallel download on ",
                         nr_nodes,
                         " R session nodes...\n")

    clus <- makeCluster(nr_nodes)

    if (!quiet) {
    message("Starting parallel downloads. ",
            "This may take a while (and I can't show you the overall progress).\n",
            "Be patient...\n")
    }

    clusterMap(clus,
               function(src, dest) {
                 curl_download(url = src,
                               destfile = dest,
                               quiet = quiet)
                 },
               file_urls,
               destfiles)

    stopCluster(clus)

    if (!quiet) message("Ended parallel downloads.")

  } else {

    mapply(curl_download,
             file_urls,
             destfiles,
             MoreArgs = list(quiet = quiet))

  }

  # check each of the files

  if (!quiet) message("\nVerifying file integrity...\n")

  for (i in seq_along(file_urls)) {
    filename <- filenames[i]
    destfile <- destfiles[i]
    md5 <- unname(md5sum(destfile))
    zenodo_md5 <- str_split(file_md5[i], ":")[[1]][2]
    if (all.equal(md5, zenodo_md5)) {
      if (!quiet) message(filename,
                          " was downloaded and its integrity verified (md5sum: ",
                          md5,
                          ")")
    } else {
      warning("Incorrect download! md5sum ",
              md5,
              " for file",
              filename,
              " does not match the Zenodo archived md5sum ",
              zenodo_md5)
    }
  }
}



#' Human-readable binary file size
#'
#' Takes an integer (referring to number of bytes) and returns an optimally
#' human-readable
#' \href{https://en.wikipedia.org/wiki/Binary_prefix}{binary-prefixed}
#' byte size (KiB, MiB, GiB, TiB, PiB, EiB)
#'
#' @param x An integer, i.e. the number of bytes (B).
#'
#' @return A string.
#' @importFrom assertthat
#' assert_that
#' is.number
#' @importFrom dplyr
#' %>%
#' @keywords internal
human_filesize <- function(x) {
  assert_that(is.number(x))
  magnitude <-
    log(x, base = 1024) %>%
    floor %>%
    min(8)
  unit <- switch(magnitude + 1,
                 "B",
                 "KiB",
                 "MiB",
                 "GiB",
                 "TiB",
                 "PiB",
                 "EiB",
                 "ZiB",
                 "YiB")
  size <- (x / 1024^magnitude) %>% round(1)
  return(paste(size, unit))
}
