#'Extract formula
#' @param outcome Terminal outcome of type character.
#' @param dataframe Dataframe of type data.frame. Must contain outcome and all observables.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
extract_formula <- function(dataframe) {
  for (col in colnames(dataframe)) {
    if (col == colnames(dataframe)[1]) {
      reg_formula <- paste(col, "~", sep=" ")
    } else if (col == colnames(dataframe)[length(colnames(dataframe))]) {
      reg_formula <- paste(reg_formula, col, sep=" ")
    } else {
      reg_formula <- paste(reg_formula, col, "+", sep=" ")
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
stage_regression <- function(dataframe) {
  # Extract formula
  reg_formula <- extract_formula(dataframe)

  # Regression
  # TODO: Change to GAM!
  stage_reg <- lm(reg_formula, data = dataframe)

  return(stage_reg)
}
