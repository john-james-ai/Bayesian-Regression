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

    p$performance
  })

  # Sort models, by MSE
  top <- list.sort(top3, mse, (mse))

  # Summarize Results
  compare <- do.call("rbind", predictions)
  compare <- as.data.frame(compare %>% group_by(Prior) %>%
                             summarize(BMA = mean(BMA),
                                       BPM = mean(BPM),
                                       HPM = mean(HPM),
                                       MPM = mean(MPM)))

  #

  # Format results
  compare <- as.data.frame(compare)
  compare[,2:ncol(compare)] <- round(compare[,2:ncol(compare)],3)
  best <- min(compare[,2:ncol(compare)])
  meanMSE <- mean(compare[,2:ncol(compare)])
  bestBMA <- min(compare$BMA)
  bestBPM <- min(compare$BPM)
  bestHPM <- min(compare$HPM)
  bestMPM <- min(compare$MPM)
  meanBMA <- round(mean(compare$BMA), 3)
  meanBPM <- round(mean(compare$BPM), 3)
  meanHPM <- round(mean(compare$HPM), 3)
  meanMPM <- round(mean(compare$MPM), 3)
  meanRow <- data.frame(Prior = "Mean", BMA = meanBMA, BPM = meanBPM, HPM = meanHPM, MPM = meanMPM)
  bestRow <- data.frame(Prior = "Best", BMA = bestBMA, BPM = bestBPM, HPM = bestHPM, MPM = bestMPM)

  compare$BMA <- kableExtra::cell_spec(x = compare$BMA[1:(nrow(compare)-2)], format = "html",
                                       color =  ifelse(compare$BMA == top3[[1]]$mse, "red", "black"),
                                       bold =  ifelse(compare$BMA %in%
                                                        c(top3[[1]]$mse,
                                                          top3[[2]]$mse,
                                                          top3[[3]]$mse), TRUE, FALSE))
  compare$BPM <- kableExtra::cell_spec(x = compare$BPM[1:(nrow(compare)-2)], format = "html",
                                       color =  ifelse(compare$BPM == top3[[1]]$mse, "red", "black"),
                                       bold =  ifelse(compare$BPM %in%
                                                        c(top3[[1]]$mse,
                                                          top3[[2]]$mse,
                                                          top3[[3]]$mse), TRUE, FALSE))
  compare$HPM <- kableExtra::cell_spec(x = compare$HPM[1:(nrow(compare)-2)], format = "html",
                                       color =  ifelse(compare$HPM == top3[[1]]$mse, "red", "black"),
                                       bold =  ifelse(compare$HPM %in%
                                                        c(top3[[1]]$mse,
                                                          top3[[2]]$mse,
                                                          top3[[3]]$mse), TRUE, FALSE))
  compare$MPM <- kableExtra::cell_spec(x = compare$MPM[1:(nrow(compare)-2)], format = "html",
                                       color =  ifelse(compare$MPM == top3[[1]]$mse, "red", "black"),
                                       bold =  ifelse(compare$MPM %in%
                                                        c(top3[[1]]$mse,
                                                          top3[[2]]$mse,
                                                          top3[[3]]$mse), TRUE, FALSE))
  compare <- rbind(compare, meanRow)
  compare <- rbind(compare, bestRow)

  comparison <- list(
    top = top,
    summary = compare
  )

  return(comparison)
}
