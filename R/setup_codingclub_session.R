#' Download files (code and data) for a specific coding club session
#'
#' @description
#' This function will populate the content of /src and /data directories
#' for a specific coding club session whose date is passed as a parameter.
#'
#' @details
#' blablabla
#'
#' @param session_date The date of the coding-club session, in the "YYYYMMDD" format.
#'
#' @importFrom assertthat assert_that
#' @importFrom RCurl CFILE curlPerform
#' @importFrom gh gh
#' @importFrom here here
#'
#' @examples
#' setup_codingclub_session("20200326")
#'
#' @export
setup_codingclub_session <- function(session_date) {
  msg_session_date <- "session_date should be a string representing a date (YYYYMMDD)"

  assert_that(is.string(session_date), msg = msg_session_date)
  assert_that(grepl("\\d{4}(0[1-9]|1[012])(0[1-9]|[12][0-9]|3[01])", session_date), msg = msg_session_date)

  download_content_in_subdir <- function(session_date, subdirectory) {

    download_file <- function(url, file){
      f = CFILE(file, mode="wb")
      a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
      RCurl::close(f)
      return(a)
    }

    content_found = FALSE
    content <- tryCatch({
      resp <- gh(sprintf("/repos/inbo/coding-club/contents/%s/%s", subdirectory, session_date))
      content_found = TRUE
      resp # Because last evaluted expression is returned... TODO: is there a better way?
    }, error = function(error_condition) {
      warning(sprintf("No content for this session (%s) found in %s", session_date, subdirectory))
    })

    if (content_found) {
      target_dir <- here(subdirectory, session_date)
      dir.create(target_dir, recursive = TRUE) # Suppress warnings?

      for (f in content) {
        download_file(f$download_url, file.path(target_dir, f$name))
      }
    }
  }

  download_content_in_subdir(session_date, 'src')
  download_content_in_subdir(session_date, 'data')

  # TODO: Show error asking to double-check the date if nothing is found?
}

# TODO: default value (date of the day) for session_date?
# TODO: should we clean the 'date' subdirectories, if they already exists? Or make that configurable?
# TODO: think about what to support/suggest for people already having an environment
# TODO: improve documentation, write example
# TODO: run linter?
# TODO: it seems other package functions use the "curl" package, we use RCurl... possible to avoid the latter?

