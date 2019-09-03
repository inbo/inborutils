#' Write essential session info to a file or connection
#'
#' Writes essentials of
#' \code{\link[sessioninfo:session_info]{sessioninfo::session_info()}}
#' to a file or connection in order to
#' maintain versionable documentation of a workflow and aid
#' reproducibility.
#'
#' The following information is written:
#' \itemize{
#' \item{essential platform info: version, os, system, ctype}
#' \item{essential info on loaded packages: package, loadedversion, date,
#' source.}
#' }
#'
#' @param file A file path or a \code{\link[base]{connection}}.
#' @inheritParams sessioninfo::session_info
#'
#' @importFrom sessioninfo session_info
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_sessioninfo(file = "./sessioninfo.yml")
#' }
write_sessioninfo <- function(file, pkgs = NULL, include_base = FALSE) {
  si <- session_info()
  si[["platform"]] <- si[["platform"]][c("version", "os", "system", "ctype")]
  relevant <- c("loadedversion", "date", "source")
  si[["packages"]] <- apply(
    si[["packages"]],
    1,
    function(x) {
      as.list(x[relevant])
    }
  )
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("please install the 'yaml' package first")
  }
  yaml::write_yaml(si, file)
}
