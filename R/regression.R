#'Extract formula
#' @param outcome Terminal outcome of type character.
#' @param dataframe Dataframe of type data.frame. Must contain outcome and all observables.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
extract_formula <- function(outcome, covariates) {
  reg_formula <- paste(outcome, "~", sep=" ")

  for (covariate in covariates) {
    if (covariate == covariates[-1]) {
      reg_formula <- paste(reg_formula, covariate, sep=" ")
    } else {
      reg_formula <- paste(reg_formula, covariate, "+", sep=" ")
    }
  }
  return(reg_formula)
}

#' Stage Regression Function
#' @param outcome Terminal outcome of type character.
#' @param dataframe Dataframe of type data.frame. Must contain outcome and all observables.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
stage_regression <- function(outcome, covariates, dataframe){
  if (!is.character(outcome)) stop("outcome must a character")
  if (!is.data.frame(dataframe)) stop("data must a data.frame")

  # Extract formula
  reg_formula <- extract_formula(outcome, covariates)

  # Regression
  # TODO: Change to GAM!
  stage_reg <- lm(reg_formula, data = dataframe)

  return(stage_reg)
}
