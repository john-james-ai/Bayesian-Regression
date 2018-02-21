#==============================================================================#
#                   Bayes Model Averaging Predictions                          #
#==============================================================================#
#' bmaPredict
#'
#' \code{bmaPredict} Performs prediction using a single model and estimator
#' 
#' Performs predictions on a single model and estimator and returns MSE and
#' optionally, residual vs fitted, parameter estimates and prediction
#' distributions on existing training data or new test data.
#'
#' @param model BAS.LM object
#' @param estimator String indicating which estimator to use. Options are 'BMA', 'BPM', 'HPM', & 'MPM'
#' @param yX Optional data frame containing test data
#' @param trial Numeric indicator of the trial number when repeated trials are being conducted.
#' @param prediction Logical value to indicate whether the observed design matrix 
#' used in fitting or the newdata will be used for estimating the mean or 
#' for making predictions. The default is FALSE for estimation of the mean.
#' @param rvp Logical indicating whether to return residuals vs predicted (fitted) data frame
#' @param pe Logical indicating whether to return paramater estimates
#' @param pi Logical indicating whether to return prediction intervals
#'
#' @return mse frame containing MSE measures for each model
#'
#' @author John James, \email{jjames@@yXsciencesalon.org}
#' @family BMA functions
#' @export
bmaPredict <- function(model, estimator, yX = NULL, trial = NULL, prediction = FALSE, rvp = FALSE, 
                       pe = FALSE, pi = FALSE) {
  
  p <- list()
  estimators <- c("BMA", "BPM", "HPM", "MPM")
  
  if (!(estimator %in% estimators)) stop("Invalid estimator. Must be 'BMA', BPM', 'HPM', or 'MPM'.")
  
  # Perform prediction
  if (is.null(yX)) {
    y <- model$Y
    pred <- predict(object = model, se.fit = pi, 
                            estimator = estimator, prediction = prediction)
  } else {
    y <- yX$audience_score
    pred <- predict(object = model, newdata = yX, se.fit = pi, 
                            estimator = estimator, prediction = prediction)
  }
  
  if (prediction == TRUE) {
    Yhat <- pred$Ypred
    se <- pred$se.pred
  } else {
    Yhat <- pred$fit
    se <- pred$se.fit
  }
    
  
  # Prepare MSE
  p$mse <- data.frame(Prior = model$prior,
                      PriorDesc = model$priorDesc,
                      id = estimator,
                      mse = mean((y - Yhat)^2))
  if (!(is.null(trial))) {
    p$mse <- cbind(Trial = trial, p$mse)
  }
  
  # Prepare residual vs prediction data frame 
  if (rvp == TRUE) {
    p$rvp <- data.frame(Residual = (y - Yhat), Predicted = Yhat)
  }
  
  # Prepare parameter estimates
  if (pe == TRUE) {
    p$pe <- bmaPDC(m = model, estimator = estimator)
  }
  
  # Prepare prediction intervals
  if (pi == TRUE) {
    p$pi <- data.frame(Yhat = Yhat, SE = se)
    if (prediction == TRUE) {
      p$ci <- confint(pred, parm = 'pred')
    } else {
      p$ci <- confint(pred, parm = 'mean')
    }
    p$pi <- cbind(p$pi, p$ci[,c(1:2)])
    names(pi) <- c("Yhat", "SE", "2.5%", "97.5%")
  }
    
  
  return(p)
}
