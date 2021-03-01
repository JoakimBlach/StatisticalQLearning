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
  if (!is.integer(start_stage)) stop("start_stage must a integer")
  if (!is.integer(end_stage)) stop("end_stage must a integer")
  if (!is.data.frame(dataframe)) stop("dataframe must a data.frame")

  optDTR <- list("model", "opt_tx", "opt_resp")

  for (stage in start_stage:end_stage) {
    stage_name <- paste("stage", "1", sep = "")

    # Stage dataframe
    stage_df <- dataframe[stage_column = stage, ]

    # Stage regression
    optDTR$model[stage_name] <- stage_regression(outcome, covariates, stage_df)

    # Optimal policy
    opt_policy <- optimal_policy(exposure, optDTR$model[stage_name], stage_df)

    optDTR$opt_tx[stage_name] <- opt_policy$opt_tx
    optDTR$opt_resp[stage_name] <- opt_policy$opt_resp
  }

  return(optDTR)
}
