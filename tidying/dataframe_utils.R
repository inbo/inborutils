#
## Functions useful within the context of data-frame handling
## and cleaning/adapting data sets
#


#' Convert all factors of a dataframe to characters
#'
#' The function checks the which columns are factors and converts these to
#' character vectors
#'
#' all credits to Thierry Onkelinx
#'
#' @param rp A dataframe
#'
#' @examples
#' df_factors_to_char(PlantGrowth) # column group will be chars as well
#'
df_factors_to_char <- function(rp){
    factors <- sapply(rp, is.factor)
    if (any(factors)) {
        rp <- rp %>%
            mutate_each_(funs(as.character), vars = names(factors)[factors])
    }
    return(rp)
}