#' Convert encoding of character and factor variables in a dataframe
#'
#' @details
#' Encoding strings: all \code{R} platforms support \code{""} (for the
#' encoding of the current
#' locale), \code{"latin1"} and \code{"UTF-8"}.
#' See \code{\link[base]{iconv}} for more information.
#'
#' @param x A dataframe or an object (such as `sf`) with the `data.frame`
#' class
#'
#' @inheritParams base::iconv
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
                          from = "",
                          to = "UTF-8",
                          sub = NA) {

    assert_that(inherits(x, "data.frame"))
    assert_that(is.string(to))

    is_chfact <- function(vec) {
        if (is.factor(vec)) {
            is.character(levels(vec))
        } else FALSE
    }

    conv_levels <- function(fact, from, to, sub) {
        levels(fact) <- iconv(levels(fact),
                              from = from,
                              to = to,
                              sub = sub)
        return(fact)
    }

    x %>%
        mutate_if(is.character,
                  iconv,
                  from = from,
                  to = to,
                  sub = sub) %>%
        mutate_if(is_chfact,
                  conv_levels,
                  from = from,
                  to = to,
                  sub = sub)

}
