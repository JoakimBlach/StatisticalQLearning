#' @param exposure Exposure of type character.
#' @param model Model object.
#' @param dataframe Dataframe of type data.frame.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
optimal_policy <- function(exposure, model, dataframe) {
  opt_policy <- list("opt_tx" = c(), "opt_resp" =c())

  for (row in 1:nrow(dataframe)) {
    # Counterfactual observation
    counterfactual_obs <- dataframe[row, ]

    # Predict counuterfactual
    predict_outcome <- function(value) {
      counterfactual_obs[1, exposure] <- value
      response <- predict(reg, counterfactual_obs)
      return(response)
    }

    # Find optimal value
    opt_res <- optim(1000, predict_outcome, method="BFGS")

    opt_policy$opt_tx <- append(opt_policy$opt_tx, opt_res$value)
    opt_policy$opt_resp <- append(opt_policy$opt_resp, opt_res$par)
  }

  return(opt_tx)
}
