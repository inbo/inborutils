#' Get data from a Zenodo archive
#'
#' This function will download an entire archive from Zenodo (\url{https://zenodo.org}).
#' It only works for Zenodo created DOI (not when the DOI is for
#' example derived from Zookeys.)
#'
#' @param path Path where the data must be downloaded.
#' Defaults to the working directory.
#' @param doi a doi pointer to the Zenodo archive starting with '10.5281/zenodo.'. See examples.
#'
#' @return Downloaded file(s) in the specified folder.
#'
#' @importFrom stringr fixed str_remove str_split
#' @importFrom curl curl_fetch_memory curl_download
#' @importFrom jsonlite fromJSON
#' @importFrom tools md5sum
#' @importFrom utils tail
#' @importFrom assertthat
#' assert_that
#' is.string
#'
#' @export
#' @family download
#'
#' @examples
#' \dontrun{
#' # Example download of an archive containing a single zip
#' download_zenodo(doi = "10.5281/zenodo.1283345")
#' # Example download of an archive containing multiple files
#' # (all files will be downloaded)
#' download_zenodo(doi = "10.5281/zenodo.1172801")
#' # Example download of an archive containing a single pdf file
#' download_zenodo(doi = "10.5281/zenodo.168478")
#' }
download_zenodo <- function(doi,
                            path = ".") {

  assert_that(is.string(doi), is.string(path))

  # check for existence of the folder
  if (!dir.exists(path)) {
    stop("The path does not exist.")
  }

  record <- str_remove(doi, fixed("10.5281/zenodo."))

  # Retrieve file name by records call
  base_url <- 'https://zenodo.org/api/records/'
  req <- curl_fetch_memory(paste0(base_url, record))
  content <- fromJSON(rawToChar(req$content))

  # extract individual file names and urls
  file_urls <- content$files$links$self

  # extract check-sum(s)
  file_md5 <- content$files$checksum

  # donwload each of the files
  for (i in seq_along(file_urls)) {
    file_name <- tail(str_split(file_urls[i], "/")[[1]], 1)
    destfile <- file.path(path, file_name)
    curl_download(url = file_urls[i],
                  destfile = destfile,
                  quiet = FALSE)
    md5 <- unname(md5sum(destfile))
    zenodo_md5 <- str_split(file_md5[i], ":")[[1]][2]
    if (all.equal(md5, zenodo_md5)) {
      print(paste0("md5sum ", md5, " for ", file_name," is correct."))
    } else {
      warning(paste0("md5 sum ",
                     md5,
                     " for file",
                     file_name,
                     " does not match the Zenodo archived md5 sum ",
                     zenodo_md5))
    }
  }
}

