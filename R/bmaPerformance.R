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
  predictions$MSE <- data.frame()
  estimators <- c("BMA", "BPM", "HPM", "MPM")
  set.seed(529)
  
  predictions$trials <- lapply(seq_len(trials), function(i) {
    
    resample <- TRUE
    
    # Sample Data
    while(resample == TRUE) {
      sample <- sample.int(n = nrow(yX), size = floor(.8*nrow(yX)), replace = F)
      train <- as.data.frame(yX[sample, ])
      test  <- as.data.frame(yX[-sample, ])
      resample <- checkSample(test)
    }
    
    # Train models
    models <- bma(train)

    # Perform predictions on new data.
    p <- bmaPredictModels(models = models, yX = test, trial = i)
    predictions$MSE <<- rbind(predictions$MSE, p$MSE)
    p$modelResults
  })
  
  names(predictions$trials) <- paste0("Trial-",seq(1:trials)) 
  
  return(predictions)
}
checkSample <- function(test) {
  resample <- FALSE
  constants <- sapply(test, function(x) {
    if (length(unique(x)) == 1) resample <<- TRUE
  })
  return(resample)
}