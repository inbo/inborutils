#' Split a long (plot) label into a two-liner
#'
#' When a label exceeds the maximum length as defined by the user, the function
#' splits the text/label on the first space after the centre of the string.
#' This is very useful for long labels in a plot
#'
#' The function can be easily applied to a `data.frame` by using e.g. `sapply`
#'
#' @param label A character array (the label)
#' @param maxlength When the label exceeds this length, it will be split into
#' a two-line label
#'
#' @export
#'
#' @family Plot_utilities
#'
#' @examples
#' plot_label_splitter("Exotic long label", 10)
#'
plot_label_splitter <- function(label, maxlength) {
  label <- as.character(label)
  if (nchar(label) > maxlength) {
    # find space closest to centre of string
    split_space <- nchar(label) %/% 2
    regex_statement <- paste("(^.{", split_space, "})(\\S*)([ ])", sep = "")
    label <- gsub(regex_statement, "\\1\\2\n", label)
  }
  return(label)
}
