#==============================================================================#
#                   Bayes Model Averaging Predictions                          #
#==============================================================================#
#' bmaPredictModels
#'
#' \code{bmaPredictModels} Predictions cross validation
#'
#' @param models List of BMA models
#' @param yX Data frame containing test data
#' @param trial Numeric indicator of the number of trials of repeated predictions
#'
#' @return mse frame containing MSE measures for each model
#'
#' @author John James, \email{jjames@@yXsciencesalon.org}
#' @family BMA functions
#' @export
bmaPredictModels <- function(models, yX, trial = NULL) {
  
  predictions <- list()
  predictions$mse <- data.frame()
  
  estimators <- c("BMA", "BPM", "HPM", "MPM")

  y <- yX$audience_score
  
  predictions$mse <- data.frame()

  predictions$modelResults <- lapply(models, function(m) {
    estimatorResults <- lapply(estimators, function(e) {
      p <- bmaPredict(model = m, estimator = e, yX = yX, prediction = TRUE,
                      trial = trial,  rvp = FALSE, pe = FALSE, pi = FALSE)
      predictions$mse <<- rbind(predictions$mse, p$mse)
      p
    })
    names(estimatorResults) <- estimators
    estimatorResults
  })
  names(predictions$modelResults) <- unlist(lapply(models, function(m) { m$prior}))
  
  return(predictions)
}
