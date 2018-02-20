#==============================================================================#
#                                BMA Compare                                   #
#==============================================================================#
#' bmaCompare
#'
#' \code{bmaCompare} Compares predictive accuracy of best predictive models
#'
#' @param yX Data frame containing the y vector and X matrix of parameters.
#' @param trials Numeric indicating the number of trials to perform
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaCompare <- function(yX, trials = 2) {

  set.seed(529)
  predictions <- lapply(seq_len(trials), function(i) {

    # Sample Data
    sample <- sample.int(n = nrow(yX), size = floor(.8*nrow(yX)), replace = F)
    train <- as.data.frame(yX[sample, ])
    test  <- as.data.frame(yX[-sample, ])

    # Train models
    models <- bma(train)

    # Perform predictions on new data.
    p <- bmaPredict(models, test)

    p$performance <- cbind(Trial = as.factor(i), p$performance)
    p$performance
  })
  
  return(predictions)
}
