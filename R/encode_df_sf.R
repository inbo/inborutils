#' Convert encoding of character and factor variables in a dataframe
#'
#' @param x A dataframe or an object (such as `sf`) with the `data.frame`
#' class
#' @param to Encoding string to convert to.
#' All \code{R} platforms support \code{""} (for the encoding of the current
#' locale), \code{"latin1"} and \code{"UTF-8"}.
#' See \code{\link[base]{iconv}} for more information.
#'
#' @md
#'
#' @return
#' The original object, with character variables (and levels of
#' (character) factor variables) converted to the specified encoding.
#'
#' @export
#' @importFrom dplyr
#' %>%
#' mutate_if
#' @importFrom assertthat
#' assert_that
#' is.string
convertdf_enc <- function(x,
                          to = "UTF-8") {

    assert_that(inherits(x, "data.frame"))
    assert_that(is.string(to))

    is_chfact <- function(vec) {
        if (is.factor(vec)) {
            is.character(levels(vec))
        } else FALSE
    }

    conv_levels <- function(fact, to) {
        levels(fact) <- iconv(levels(fact),
                              to = to)
        return(fact)
    }

    x %>%
        mutate_if(is.character,
                  iconv,
                  to = to) %>%
        mutate_if(is_chfact,
                  conv_levels,
                  to = to)

}
