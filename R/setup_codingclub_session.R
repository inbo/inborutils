#' Download files (code and data) for a specific coding club session
#'
#' @description This function will populate the content of /src and /data
#'   directories for a specific coding club session whose date is passed as a
#'   parameter. Directories are created if they do not exist otherwise a warning
#'   is returned. Files with the same name are overwritten.
#'
#' @param session_date The date of the coding-club session, in the "YYYYMMDD"
#'   format. Default: actual date
#' @param root_dir Root directory where source and data subdirectories are
#'   located. Default: "./"
#' @param src_rel_path Relative path for R script(s). Default: `src`
#' @param data_rel_path Relative path for data files. Default: `data`
#'
#' @usage setup_codingclub_session(`20200225`)
#' @importFrom assertthat assert_that
#' @importFrom gh gh
#' @importFrom curl curl_download
#'
#' @examples
#' \dontrun{
#' library(inborutils)
#'
#' # Download coding club files for session 2020-02-25
#' # R script(s) in `./src/20200225` and data files in `./data/20200225`
#' setup_codingclub_session("20200225")
#'
#' # Specify the folders where you want to save R files and data files
#' # R files will be downloaded in `./source`
#' # Data files will be downloaded in `./dataframes`
#'   src_rel_path = "source",
#'   data_rel_path = "dataframes"
#' )
#'
#' # You can modify the root directory even if it should be normally not needed.
#' # For example you want to move the root to the parent directory, `../`
#' setup_codingclub_session("20200225",
#'   root_dir = "../",
#'   src_rel_path = "source",
#'   data_rel_path = "dataframes"
#' )
#' }
#'
#' @export

setup_codingclub_session <- function(
                                     session_date = format(Sys.Date(), "%Y%m%d"),
                                     root_dir = ".",
                                     src_rel_path = "src",
                                     data_rel_path = "data") {
  msg_session_date <- "session_date must be a string representing a date (YYYYMMDD)"
  assert_that(is.string(session_date), msg = msg_session_date)
  assert_that(grepl(
    "\\d{4}(0[1-9]|1[012])(0[1-9]|[12][0-9]|3[01])",
    session_date
  ),
  msg = msg_session_date
  )

  github_src_link <- sprintf(
    "https://github.com/inbo/coding-club/tree/master/data/%s",
    session_date
  )
  github_data_link <- sprintf(
    "https://github.com/inbo/coding-club/tree/master/src/%s",
    session_date
  )
  src_target_dir <- paste(root_dir, src_rel_path, session_date, sep = "/")
  data_target_dir <- paste(root_dir, data_rel_path, session_date, sep = "/")
  print(sprintf(
    "R scripts in %s will be downloaded in folder: %s",
    github_src_link, src_target_dir
  ))
  print(sprintf(
    "Data in %s will be downloaded in folder: %s",
    github_data_link, data_target_dir
  ))
  continue <- tolower(readline("Do you want to continue? (Y/N) "))
  if (continue == "y") {
    cat("\nDownlaod source file(s)...\n")
    download_content_in_subdir(session_date,
      target_directory = src_target_dir,
      github_subdirectory = "src"
    )
    cat("\nSource file(s) downloaded.\n")

    cat("\nDownlaod data file(s)...\n")
    download_content_in_subdir(session_date,
      target_directory = data_target_dir,
      github_subdirectory = "data"
    )
    cat("\nData file(s) downloaded.\n")
    # TODO: Show error asking to double-check the date if nothing is found?
  } else {
    print("Download aborted.")
  }
}

download_content_in_subdir <- function(session_date,
                                       target_directory,
                                       github_subdirectory) {
  assert_that(github_subdirectory %in% c("src", "data"),
    msg = "R scripts are in ./src/, data are in ./data."
  )
  content_found <- FALSE
  content <- tryCatch(
    {
      resp <- gh(sprintf(
        "/repos/inbo/coding-club/contents/%s/%s",
        github_subdirectory, session_date
      ))
      content_found <- TRUE
      resp
    },
    error = function(error_condition) {
      warning(sprintf(
        "No content for this session (%s) found. Is the date correct?",
        session_date))
    }
  )

  if (content_found) {
    dir.create(target_directory, recursive = TRUE, showWarnings = TRUE)
    for (f in content) {
      print(sprintf("Downloading %s", f$html_url))
      curl_download(url = f$download_url,
                    destfile = file.path(target_directory, f$name),
                    mode = "wb")
    }
  }
}

# TODO: should we clean the 'date' subdirectories, if they already exists? Or make that configurable?
# TODO: run linter?
