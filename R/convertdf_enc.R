#' Convert encoding of character and factor variables in a dataframe
#'
#' @details
#' Encoding strings: all \code{R} platforms support \code{""} (for the
#' encoding of the current
#' locale), \code{"latin1"} and \code{"UTF-8"}.
#' See \code{\link[base]{iconv}} for more information.
#'
#' @param x An object with the `data.frame`
#' class (such as `data.frame` or `sf`)
#' @param colnames Should column names be converted as well?
#'
#' @inheritParams base::iconv
#'
#' @md
#'
#' @family Data_handling_utilities
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
#' is.flag
#' noNA
convertdf_enc <- function(x,
                          from = "",
                          to = "UTF-8",
                          sub = NA,
                          colnames = FALSE) {
  assert_that(inherits(x, "data.frame"))
  assert_that(
    is.string(to),
    is.string(from),
    is.string(sub) | is.na(sub)
  )
  assert_that(is.flag(colnames), noNA(colnames))


  is_chfact <- function(vec) {
    if (is.factor(vec)) {
      is.character(levels(vec))
    } else {
      FALSE
    }
  }

  conv_levels <- function(fact, from, to, sub) {
    levels(fact) <- iconv(levels(fact),
      from = from,
      to = to,
      sub = sub
    )
    return(fact)
  }

  x %>%
    mutate_if(is.character,
      iconv,
      from = from,
      to = to,
      sub = sub
    ) %>%
    mutate_if(is_chfact,
      conv_levels,
      from = from,
      to = to,
      sub = sub
    ) %>%
    {
      if (colnames) {
        `colnames<-`(., iconv(colnames(.),
          from = from,
          to = to,
          sub = sub
        ))
      } else {
        .
      }
    }
}
