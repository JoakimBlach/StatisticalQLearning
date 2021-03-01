#'
#'
#'
#'
optimal_policy <- function(treatment, model, dataframe) {
  opt_tx <- list("opt_treatments" = c(), "opt_response" =c())
  
  for (row in 1:nrow(dataframe)) {
    predict_outcome <- function(value) {
      # Counterfactual observation
      counterfactual_obs <- dataframe[row, ]
      counterfactual_obs[1, treatment] <- value
      
      # Model response  
      response <- predict(reg, counterfactual_obs)  
      
      return(response)
    }
    
    # Optimise    
    opt_res <- optim(1000, predict_outcome, method="BFGS")
    
    opt_tx$opt_treatments <- append(opt_tx$opt_treatments, opt_res$value)
    opt_tx$opt_response <- append(opt_tx$opt_response, opt_res$par)
  }
  
  return(opt_tx)
}