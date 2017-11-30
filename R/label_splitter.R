#
# These functions are useful within the contect of any plotting/visualisation
# routine and not bound to a specific package
#


#' Split a long label into a two-liner
#'
#' When a label exceeds the maximum length as defined by the user, the function
#' splits the text/label on the first space after the center of the string.
#'
#' The function can be easily applied to a data.frame by using e.g. sapply
#'
#' @param label A character array (the label)
#' @param maxlength When the label exceeds this length, it will be splitted into
#' a two-line label
#'
#' @examples
#' label.splitter("Exotic long label", 10)
#'
label.splitter <- function(label, maxlength){
    label <- as.character(label)
    if (nchar(label) > maxlength) {
        # find space closest to center of string
        split.space <- nchar(label) %/% 2
        regex.statement <- paste("(^.{", split.space, "})(\\S*)([ ])", sep = "")
        label <- gsub(regex.statement, "\\1\\2\n", label)
    }
    return(label)
}