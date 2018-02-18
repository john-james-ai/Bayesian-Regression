#==============================================================================#
#                           BMA Compare Report                                 #
#==============================================================================#
#' bmaCompareReport
#'
#' \code{bmaCompareReport} Renders tables and plots for BMA Compare function
#'
#' @param compare List containing performance data from BMACompare 
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaCompareReport <- function(compare) {
  
  top <- compare$top
  predictions <- compare$predictions
  
  # Get results
  performance <- do.call("rbind", predictions)
  
  # Reshape results
  mseDist <- list()
  mseDist$all <- melt(performance, id = c("Prior", "Trial"))
  names(mseDist$all) <- c("Prior", "Trial", "Model", "MSE")
  mseMIN <- min(mseDist$all$MSE)
  mseMAX <- max(mseDist$all$MSE)
  mseDist$BMA <- mseDist$all %>% filter(Model == "BMA") %>% select(Trial, MSE, Prior )
  mseDist$BPM <- mseDist$all %>% filter(Model == "BPM") %>% select(Trial, MSE, Prior )
  mseDist$HPM <- mseDist$all %>% filter(Model == "HPM") %>% select(Trial, MSE, Prior )
  mseDist$MPM <- mseDist$all %>% filter(Model == "MPM") %>% select(Trial, MSE, Prior )
  
  # Plot results
  msePlots <- list()
  msePlots$BMA <- plotLine(data = mseDist$BMA, xticks = FALSE, xLab = "Trials", yLab = "Mean Squared Error", 
                           yLow = mseMIN, yHigh = mseMAX,
                           plotTitle = "Prior Predictive Performance (BMA)")
  msePlots$BPM <- plotLine(data = mseDist$BPM, xticks = FALSE, xLab = "Trials", yLab = "Mean Squared Error", 
                           yLow = mseMIN, yHigh = mseMAX,
                           plotTitle = "Prior Predictive Performance (BPM)")
  msePlots$HPM <- plotLine(data = mseDist$HPM, xticks = FALSE, xLab = "Trials", yLab = "Mean Squared Error", 
                           yLow = mseMIN, yHigh = mseMAX,
                           plotTitle = "Prior Predictive Performance (HPM)")
  msePlots$MPM <- plotLine(data = mseDist$MPM, xticks = FALSE, xLab = "Trials", yLab = "Mean Squared Error", 
                           yLow = mseMIN, yHigh = mseMAX,
                           plotTitle = "Prior Predictive Performance (MPM)")
  
  
  
  # Summarize Results
  performance <- as.data.frame(performance %>% group_by(Prior) %>%
                             summarize(BMA = mean(BMA),
                                       BPM = mean(BPM),
                                       HPM = mean(HPM),
                                       MPM = mean(MPM)))

  # Format results
  performance[,2:ncol(performance)] <- round(performance[,2:ncol(performance)],3)
  best <- min(performance[,2:ncol(performance)])
  bestBMA <- min(performance$BMA)
  bestBPM <- min(performance$BPM)
  bestHPM <- min(performance$HPM)
  bestMPM <- min(performance$MPM)
  meanBMA <- round(mean(performance$BMA), 3)
  meanBPM <- round(mean(performance$BPM), 3)
  meanHPM <- round(mean(performance$HPM), 3)
  meanMPM <- round(mean(performance$MPM), 3)
  meanRow <- data.frame(Prior = "Mean", BMA = meanBMA, BPM = meanBPM, HPM = meanHPM, MPM = meanMPM)
  bestRow <- data.frame(Prior = "Best", BMA = bestBMA, BPM = bestBPM, HPM = bestHPM, MPM = bestMPM)

  performance$BMA <- kableExtra::cell_spec(x = performance$BMA[1:(nrow(performance)-2)], format = "html",
                                       color =  ifelse(performance$BMA == top[[1]]$mse, "red", "black"),
                                       bold =  ifelse(performance$BMA %in%
                                                        c(top[[1]]$mse,
                                                          top[[2]]$mse,
                                                          top[[3]]$mse), TRUE, FALSE))
  performance$BPM <- kableExtra::cell_spec(x = performance$BPM[1:(nrow(performance)-2)], format = "html",
                                       color =  ifelse(performance$BPM == top[[1]]$mse, "red", "black"),
                                       bold =  ifelse(performance$BPM %in%
                                                        c(top[[1]]$mse,
                                                          top[[2]]$mse,
                                                          top[[3]]$mse), TRUE, FALSE))
  performance$HPM <- kableExtra::cell_spec(x = performance$HPM[1:(nrow(performance)-2)], format = "html",
                                       color =  ifelse(performance$HPM == top[[1]]$mse, "red", "black"),
                                       bold =  ifelse(performance$HPM %in%
                                                        c(top[[1]]$mse,
                                                          top[[2]]$mse,
                                                          top[[3]]$mse), TRUE, FALSE))
  performance$MPM <- kableExtra::cell_spec(x = performance$MPM[1:(nrow(performance)-2)], format = "html",
                                       color =  ifelse(performance$MPM == top[[1]]$mse, "red", "black"),
                                       bold =  ifelse(performance$MPM %in%
                                                        c(top[[1]]$mse,
                                                          top[[2]]$mse,
                                                          top[[3]]$mse), TRUE, FALSE))
  performance <- rbind(performance, meanRow)
  performance <- rbind(performance, bestRow)

  report <- list(
    top = top,
    performance = performance,
    msePlots = msePlots
  )

  return(report)
}
