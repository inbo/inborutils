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
#' source if different from CRAN.}
#' }
#'
#' @param file A file path or a \code{\link[base]{connection}}.
#'
#' @importFrom sessioninfo
#' platform_info
#' package_info
#' @importFrom stringr
#' str_detect
#' @importFrom utils
#' write.table
#' capture.output
#' @importFrom rlang
#' .data
#' @importFrom dplyr
#' mutate
#' select
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_sessioninfo(file = "sessioninfo.txt")
#' }
write_sessioninfo <- function(file) {

  platform_info()[c("version",
                    "os",
                    "system",
                    "ctype")] %>%
    unlist %>%
    as.matrix() %>%
    write.table(file = file,
                quote = FALSE,
                col.names = FALSE,
                sep = "\t")

  capture.output(cat("\n"),
                 file = file,
                 append = TRUE)

  package_info() %>%
    as.data.frame %>%
    mutate(non_cran = ifelse(str_detect(source, "CRAN"),
                             "",
                             source)) %>%
    select(.data$package,
           .data$loadedversion,
           .data$date,
           .data$non_cran) %>%
    write.table(file = file,
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE,
                sep = " ",
                append = TRUE)

}
