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

  top3 <- list()

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

    # Retain top 3
    top3 <<- c(top3, p$top3)
    top3 <<- list.sort(top3, mse, (mse))
    top3 <<- lapply(seq(1:3), function(x) {top3[[x]]})

    p$performance <- cbind(Trial = as.factor(i), p$performance)
    p$performance
  })

  # Sort models, by MSE
  top <- list.sort(top3, mse, (mse))
  
  # Compute confidence intervals for top models
  for (i in 1:length(top)) {
    p <- predict(top[[i]]$model, newdata = top[[i]]$yX, 
                 estimator = top[[i]]$id, se.fit = TRUE)
    top[[i]]$ci <- confint(p, parm = "pred")
  }

  compare <- list(
    top = top,
    predictions = predictions
  )

  return(compare)
}
