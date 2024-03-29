
#' Convert all factors of a dataframe to characters
#'
#' The function checks the which columns are factors and converts these to
#' character vectors
#'
#' all credits to Thierry Onkelinx
#'
#' @param rp A dataframe
#' @family Data_handling_utilities
#' @examples
#' df_factors_to_char(PlantGrowth) # column group will be chars as well
#'
#' @export
#' @importFrom dplyr %>% mutate_each_ funs
df_factors_to_char <- function(rp) {
  factors <- sapply(rp, is.factor)
  if (any(factors)) {
    rp <- rp %>%
      mutate_each_(funs(as.character), vars = names(factors)[factors])
  }
  return(rp)
}
