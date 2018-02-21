#==============================================================================#
#                                BMA Performance                               #
#==============================================================================#
#' bmaPerformance
#'
#' \code{bmaPerformance} Compares predictive accuracy of best predictive models
#'
#' @param yX Data frame containing the y vector and X matrix of parameters.
#' @param trials Numeric indicating the number of trials to perform
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaPerformance <- function(yX, trials = 3) {

  predictions <- list()
  set.seed(529)
  
  predictions$mse <- data.frame()
  estimators <- c("BMA", "BPM", "HPM", "MPM")

  predictions <- lapply(seq_len(trials), function(i) {

    # Sample Data
    sample <- sample.int(n = nrow(yX), size = floor(.8*nrow(yX)), replace = F)
    train <- as.data.frame(yX[sample, ])
    test  <- as.data.frame(yX[-sample, ])

    # Train models
    models <- bma(train)
    

    # Perform predictions on new data.
    p <- bmaPredictModels(models = models, yX = test, trial = i)
    predictions$mse <<- rbind(predictions$mse, p$mse)
    p
  })
  
  return(predictions)
}
