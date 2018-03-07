#==============================================================================#
#                          Best Model Predictions                              #
#==============================================================================#
#' bestPredictions
#'
#' \code{bestPredictions} Performs predictions using select models on new data.
#' 
#' @param best Data frame containing the top performing models and estimators
#' @param models List of models 
#' @param newdata Data frame containing new data to predict.
#'
#' @return predictions Data frame containing predictions for newdata
#'
#' @author John James, \email{jjames@@yXsciencesalon.org}
#' @family BMA functions
#' @export
bestPredictions <- function(best, models, newdata) {
  
  # Extract best models
  bestModels <- list()
  for (i in 1:nrow(best)) {
    bestModels <- c(bestModels, list(models[[best$Prior[i]]]))
  }
  
  p <- rbindlist(lapply(seq_along(bestModels), function(m) {
    p <- predict(object = bestModels[[m]], estimator = best$Estimator[m], newdata = newdata)
    e <- cv.summary.bas(pred = p$fit, ytrue = as.numeric(newdata$audience_score))
    p$fit <- c(p$fit,e)
    as.list(round(as.numeric(p$fit), 1))
  }))
  predictions <- t(as.data.frame(p, rownames = FALSE))
  colnames(predictions) <- paste0(best$Prior, " (", best$Estimator, ")")
  predictions <- cbind("Film" = c(cases$title, "MSE"), predictions, `Audience Score` = c(newdata$audience_score, ""))
  rownames(predictions) <- NULL
  return(predictions)
}
