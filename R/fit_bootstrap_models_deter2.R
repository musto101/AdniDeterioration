fit_bootstrap_models_deter2 <- function(training, test, m, B,
                                       model, grid, group){
  
  
  n1 <- nrow(test)
  predictions <- matrix(0, B, n1)
  hard_pred <- matrix('0', n1, B)
  
  for (b in 1:B) {
    
    print(b)
    boot_train <- bootstrapping(training, m, group)
    preds <- model_deter2(boot_train, test, grid, model)
    predictions[b,] <- unlist(preds[[1]])
    hard_pred[, b] <- as.vector(preds[[2]])
    
  }
  
  der <- as.data.frame(hard_pred)
  
  # hard_fact <- apply(X = hard_pred, MARGIN = 2, as.factor)
  #
  # class(hard_fact[,1])
  
  list_preds <- list(predictions, der)
  
  return(list_preds)
  
}


# def fit_bootstrap_models(X_train, Y_train, X_predict, fit_muh_fun, n, m, B):
#   '''
#       Train B bootstrap estimators and calculate predictions on X_predict
#       Return: list of matrices [M,P]
#         samples_idx = B-by-m matrix, row b = indices of b-th bootstrap sample
#         predictions = B-by-n1 matrix, row b = predictions from b-th bootstrap sample
#           (n1=len(X_predict))
#     '''
# samples_idx = generate_bootstrap_samples(n, m, B)
# n1 = len(X_predict)
# # P holds the predictions from individual bootstrap estimators
# predictions = np.zeros((B, n1), dtype=float)
# for b in range(B):
#   predictions[b] = fit_muh_fun(X_train[samples_idx[b], :],\
#                                Y_train[samples_idx[b], ], X_predict)
# return([samples_idx, predictions])
