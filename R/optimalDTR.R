#'Optimal Treatment Regime
#' @param exposure Exposure variable. Type character.
#' @param covariates List of covariates of type characters.
#' @param outcome Terminal outcome. Type character.
#' @param stage_column Column to indicate stage of treatment. Type character.
#' @param start_stage Value to indicate first stage of treatment. Type integer
#' @param end_stage Value to indicate last stage of treatment. Type integer
#' @param dataframe Dataframe of type data.frame.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
optimalDTR <- function(exposure, covariates, outcome, stage_column, start_stage, end_stage, dataframe){
  if (!is.character(exposure)) stop("exposure must a character")
  if (!is.vector(covariates)) stop("covariates must a vector")
  if (!is.character(outcome)) stop("outcome must a character")
  if (!is.character(stage_column)) stop("stage_column must a character")
  if (!(start_stage%%1==0)) stop("start_stage must a integer")
  if (!(end_stage%%1==0)) stop("end_stage must a integer")
  if (!is.data.frame(dataframe)) stop("dataframe must a data.frame")

  optDTR <- list()

  for (stage in start_stage:end_stage) {
    stage_name <- paste("stage", stage, sep = "")

    # Stage dataframe
    stage_df <- dataframe %>% filter(get(stage_column) == stage)

    if (stage == start_stage) {
      # Prepare dataframe
      response_var <- stage_df %>% select(outcome)
      covariate_vars <- stage_df %>% select(append(exposure, covariates))
      stage_df <- cbind(response_var, covariate_vars)

      # Stage regression with outcome as regressand
      stage_model <- stage_regression(stage_df)
    } else {
      # Prepare dataframe
      prev_stage_name <- paste("stage", stage-1, sep = "")
      response_var <- optDTR$opt_resp[[prev_stage_name]]
      covariate_vars <- stage_df %>% select(append(exposure, covariates))
      stage_df <- cbind(response_var, covariate_vars)

      # Stage regression with previous optimal response as regresand
      stage_model <- stage_regression(stage_df)
    }

    # Persist stage regression
    optDTR$model[[stage_name]] <- stage_model

    # Optimal policy
    opt_policy <- optimal_policy(exposure, optDTR$model[[stage_name]], stage_df)

    optDTR$opt_tx[[stage_name]] <- opt_policy$opt_tx
    optDTR$opt_resp[[stage_name]] <- opt_policy$opt_resp
  }
  return(optDTR)
}
