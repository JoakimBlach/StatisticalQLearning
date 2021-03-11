#'Optimal Policy
#' @param exposure Exposure of type character.
#' @param model Model object.
#' @param dataframe Dataframe of type data.frame.
#' @keywords Dynamic treatment regime.
#' @export
#' @examples
optimal_policy <- function(exposure, exposure_lag, model, dataframe, init_val_optim=1000) {
  opt_policy <- list("opt_tx" = c(), "opt_resp" =c())

  for (row in 1:nrow(dataframe)) {
    # Counterfactual observation
    counterfactual_obs <- dataframe[row, ]

    # Predict counuterfactual
    predict_outcome <- function(value) {
      counterfactual_obs[1, exposure] <- value
      response <- predict(model, counterfactual_obs)
      return(-response)
    }

    # Find optimal value
    opt_res <- optim(init_val_optim, predict_outcome, lower=dataframe[row, exposure_lag], upper=Inf, method="L-BFGS-B")

    opt_policy$opt_tx <- append(opt_policy$opt_tx, opt_res$par)
    opt_policy$opt_resp <- append(opt_policy$opt_resp, -opt_res$value)
  }

  return(opt_policy)
}
