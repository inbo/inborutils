
#' Title
#'
#' @param mod A model object (or data.frame). For the moment only lm and a plain dataset is implemented
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
vif <- function(mod, ...){
  UseMethod("vif")
}

#' Title
#'
#' @param mod linear model object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
vif.lm <- function(mod, ...){
  vif.default(mod, ...)
}



#' Variance inflation factors of a data frame
#'
#'Contains a wrapper function around car::vif to calculate the variance inflation of data.frames
#'
#' @param mod A data.frame with numerical and factor variables
#'
#' @return A matrix containing the VIF and standardised VIF factors
#' @export
#' @importFrom car vif
#' @method vif data.frame
#'
#' @examples
vif.data.frame <- function(mod, ...){
  mod <- cbind(dummyResponse = 1, mod)
  my_formula <- as.formula(mod)
  mod <- lm(my_formula, data = mod)
  vif.default(mod, ...)
}


#' Title
#'
#' @param mod An object like lm which supports the functions coef, terms and model.matrix
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
vif.default <- function (mod, ...)
{
  if (any(is.na(coef(mod))))
    stop("there are aliased coefficients in the model")
  v <- vcov(mod)
  assign <- attr(model.matrix(mod), "assign")
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2)
    stop("model contains fewer than 2 terms")
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs,
                                                                       -subs]))/detR
    result[term, 2] <- length(subs)
  }
  #if (all(result[, 2] == 1))
  #  result <- result[, 1]
  #else
  result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  result
}





