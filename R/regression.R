#'Extract formula
#' @param outcome Terminal outcome of type character.
#' @param dataframe Dataframe of type data.frame. Must contain outcome and all observables.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
extract_formula <- function(outcome, dataframe) {
  reg_formula <- paste(outcome, "~", sep=" ")

  reg_cols <- colnames(dataframe)
  reg_cols <- reg_cols[reg_cols != outcome]
  
  for (col in reg_cols) {
    if (col == reg_cols[length(reg_cols)]) {
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
stage_regression <- function(outcome, dataframe){
  if (!is.character(outcome)) stop("outcome must a character") 

  if (!is.data.frame(dataframe)) stop("data must a data.frame") 
  
  # Extract formula
  reg_formula <- extract_formula(outcome, dataframe)

  # Regression
  stage_reg <- lm(reg_formula, data = dataframe)
  
  return(stage_reg)
}
