#'Optimal Treatment Regime
#' @param exposure Exposure variable. Type character.
#' @param covariates List of covariates of type characters.
#' @param stage_column Column to indicate stage of treatment. Type character.
#' @param start_stage Value to indicate first stage of treatment. Type integer
#' @param end_stage Value to indicate last stage of treatment. Type integer
#' @param dataframe Dataframe of type data.frame.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
optimalDTR <- function(exposure, exposure_lag, reg_formula, stage_column, start_stage, end_stage, dataframe){
  if (!is.character(exposure)) stop("exposure must a character")
  #if (!is.formula(reg_formula)) stop("reg_formula must a formula")
  if (!is.character(stage_column)) stop("stage_column must a character")
  if (!(start_stage%%1==0)) stop("start_stage must a integer")
  if (!(end_stage%%1==0)) stop("end_stage must a integer")
  if (!is.data.frame(dataframe)) stop("dataframe must a data.frame")

  optDTR <- list()

  for (stage in start_stage:end_stage) {
    stage_name <- paste("stage", stage, sep = "")
    print(paste("Running stage", stage, sep=""))

    # Stage dataframe
    stage_df <- dataframe %>% filter(get(stage_column) == stage)

    if (stage == start_stage) {
      stage_model <- stage_regression(reg_formula, stage_df)
    } else {
      # Prepare dataframe
      prev_stage_name <- paste("stage", stage-1, sep = "")
      stage_df$response <- optDTR$opt_resp[[prev_stage_name]]

      # Prepare formula
      reg_formula <- update(reg_formula, response ~ . )

      # Stage regression with previous optimal response as regresand
      stage_model <- stage_regression(reg_formula, stage_df)
    }

    # Persist stage regression
    optDTR$model[[stage_name]] <- stage_model

    # Optimal policy
    opt_policy <- optimal_policy(exposure, exposure_lag, stage_model, stage_df)

    optDTR$opt_tx[[stage_name]] <- opt_policy$opt_tx
    optDTR$opt_resp[[stage_name]] <- opt_policy$opt_resp
  }
  return(optDTR)
}
