#'Extract formula
#' @param outcome Terminal outcome of type character.
#' @param dataframe Dataframe of type data.frame. Must contain outcome and all observables.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
get_formula <- function(dataframe) {
  for (col in colnames(dataframe)) {
    if (col == colnames(dataframe)[1]) {
      reg_formula <- paste(col, "~", sep=" ")
    } else if (col == colnames(dataframe)[length(colnames(dataframe))]) {
      reg_formula <- paste(reg_formula, " ", "s(", col, ")", sep="")
      #lm
      #paste(reg_formula, col, sep=" ")
    } else {
      reg_formula <- paste(reg_formula, " ", "s(", col, ")", " ", "+", sep="")
      #lm
      #reg_formula <- paste(reg_formula, col, "+", sep=" ")
    }
  }
  return(formula(reg_formula))
}

#' Stage Regression Function
#' @param outcome Terminal outcome of type character.
#' @param dataframe Dataframe of type data.frame. Must contain outcome and all observables.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
stage_regression <- function(reg_formula, dataframe) {
  stage_reg <- gam(reg_formula, data = dataframe)
  return(stage_reg)
}
