#==============================================================================#
#                   Bayes Model Averaging Predictions                          #
#==============================================================================#
#' bmaPredict
#'
#' \code{bmaPredict} Predictions cross validation
#'
#' @param models List of BMA models
#' @param yX Data frame containing test data
#'
#' @return mse frame containing MSE measures for each model
#'
#' @author John James, \email{jjames@@yXsciencesalon.org}
#' @family BMA functions
#' @export
bmaPredict <- function(models, yX) {

  top3 <- list()

  y <- yX$audience_score
  performance <- rbindlist(lapply(models, function(m) {

    # Render Predictions
    yhat <- list()
    yhat$BMA <- predict(m, newdata = yX, estimator = "BMA")
    yhat$BPM <- predict(m, newdata = yX, estimator = "BPM")
    yhat$HPM <- predict(m, newdata = yX, estimator = "HPM")
    yhat$MPM <- predict(m, newdata = yX, estimator = "MPM")

    rvp <- list()
    rvp$BMA <- data.frame(Residual = (y - yhat$BMA$fit), Predicted = yhat$BMA$fit)
    rvp$BPM <- data.frame(Residual = (y - yhat$BPM$fit), Predicted = yhat$BPM$fit)
    rvp$HPM <- data.frame(Residual = (y - yhat$HPM$fit), Predicted = yhat$HPM$fit)
    rvp$MPM <- data.frame(Residual = (y - yhat$MPM$fit), Predicted = yhat$MPM$fit)

    # Obtain Coefficients Distributions
    pdc <- postParamDist(m)

    # Get model Size
    size <- list()
    size$BMA <- length(yhat$BMA$bestmodel[[yhat$BMA$best[1]]])
    size$BPM <- length(yhat$BPM$bestmodel)
    size$HPM <- length(yhat$HPM$bestmodel)
    size$MPM <- length(yhat$MPM$bestmodel)

    # Get Coefficients Variables
    coefs <- list()
    coefs$BMA <- pdc$BMA$df[yhat$BMA$bestmodel[[yhat$BMA$best[1]]]+1,]
    coefs$BPM <- pdc$BPM$df[yhat$BPM$bestmodel + 1,]
    coefs$HPM <- pdc$HPM$df[yhat$HPM$bestmodel + 1,]
    coefs$MPM <- pdc$MPM$df[yhat$MPM$bestmodel + 1,]
    
    # Compute MSE
    mse <-list()
    mse$BMA <- (cv.summary.bas(yhat$BMA$fit, y, score = "squared-error"))^2
    mse$BPM <- (cv.summary.bas(yhat$BPM$fit, y, score = "squared-error"))^2
    mse$HPM <- (cv.summary.bas(yhat$HPM$fit, y, score = "squared-error"))^2
    mse$MPM <- (cv.summary.bas(yhat$MPM$fit, y, score = "squared-error"))^2

    # Compute model id and name
    mnames <- list()
    mnames$id <- c("BMA", "BPM", "HPM", "MPM")
    mnames$names <- c("Bayesian Model Averaging", "Best Predictive Model",
                      "Highest Probability Model", "Median Probability Model")

    # Format list objects
    ml <- lapply(seq_along(mse), function (x) {
      model <- list()
      model$prior <- m$prior
      model$priorDesc <- m$priorDesc
      model$id <- mnames$id[x]
      model$name <- mnames$names[x]
      model$rvp <- rvp[[x]]
      model$mse <- mse[[x]]
      model$size <- size[[x]]
      model$pdc <- pdc[[x]]
      model$coefs <- coefs[[x]]
      model$model <- m
      model$yhat <- yhat[[x]]
      model$yX <- yX
      model
    })

    top3 <<- c(top3, ml)
    top3 <<- list.sort(top3, mse, (mse))
    top3 <<- lapply(seq(1:3), function(x) {top3[[x]]})

    mse <- c(Prior = m$prior, mse)
    mse
  }))


  analysis <- list(
    top3 = top3,
    performance = performance
    )

  return(analysis)
}
