#==============================================================================#
#                       Bayes Model Averaging Reports                          #
#==============================================================================#
#' bmaAnalysis
#'
#' \code{bmaAnalysis} Reports results for a series of models.
#'
#' @param models List of BMA models
#' @return list of reports
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaAnalysis <- function(models, top = FALSE) {

  #---------------------------------------------------------------------------#
  #                             Format Data                                   #
  #---------------------------------------------------------------------------#
  pip <- do.call(cbind, lapply(models, function(x) x$probne0))
  rownames(pip) <- c(models[["BIC"]]$namesx)
  priors <- names(models)
  priorDesc <- lapply(models, function(m) { m$priorDesc })


  #---------------------------------------------------------------------------#
  #                    Posterior Model Probability Plot                       #
  #---------------------------------------------------------------------------#
  postProbs <- rbindlist(lapply(models, function(m) {
    pp <- list()
    pp$Prior <- m$prior
    pp$Post <- summary(m)[20,2]
    pp
  }))
  postProbsPlot <- plotBar2(data = postProbs, yLab = "Posterior Probability",
                            xLab = "Prior",
                            plotTitle = "Best Model Posterior Probabilities by Parameter Prior",
                            values = TRUE)


  #---------------------------------------------------------------------------#
  #                 Posterior Distribution of Coefficients                    #
  #---------------------------------------------------------------------------#
  pdc <- lapply(models, function(m) {
    postDist(m)
  })

  #---------------------------------------------------------------------------#
  #                 Parameter Inclusion Probabilities Plots                   #
  #---------------------------------------------------------------------------#
  # Get models for variables with minimum 50% inclusion across all models
  plotData <- as.data.frame(pip[2:nrow(pip),])
  for (i in 1:nrow(plotData)) {
    plots[[i]] <- plotBar2(data = data.frame(x = priors,
                                             y = as.numeric(plotData[i,])),
                           yLab = "Inclusion Probability",
                           xLab = "Prior", plotTitle = rownames(plotData)[i],
                           values = TRUE)
  }

  #---------------------------------------------------------------------------#
  #                 Parameter Inclusion Probabilities Summary                 #
  #---------------------------------------------------------------------------#
  pipTable <- as.data.frame(round(pip, 2))
  pipTable$BIC = kableExtra::cell_spec(x = pipTable$BIC, "html",
                                       bold =  ifelse(pipTable$BIC > .5, TRUE, FALSE))
  pipTable$AIC = kableExtra::cell_spec(x = pipTable$AIC, "html",
                                       bold =  ifelse(pipTable$AIC > .5, TRUE, FALSE))
  pipTable$`EB-G` = kableExtra::cell_spec(x = pipTable$`EB-G`, "html",
                                          bold =  ifelse(pipTable$`EB-G` > .5, TRUE, FALSE))
  pipTable$`EB-L` = kableExtra::cell_spec(x = pipTable$`EB-L`, "html",
                                          bold =  ifelse(pipTable$`EB-L` > .5, TRUE, FALSE))
  pipTable$g = kableExtra::cell_spec(x = pipTable$g, "html",
                                     bold =  ifelse(pipTable$g > .5, TRUE, FALSE))
  pipTable$HG = kableExtra::cell_spec(x = pipTable$HG, "html",
                                      bold =  ifelse(pipTable$HG > .5, TRUE, FALSE))
  pipTable$`HG-L` = kableExtra::cell_spec(x = pipTable$`HG-L`,
                                          "html", bold =  ifelse(pipTable$`HG-L` > .5, TRUE, FALSE))
  pipTable$`HG-N` = kableExtra::cell_spec(x = pipTable$`HG-N`,
                                          "html", bold =  ifelse(pipTable$`HG-N` > .5, TRUE, FALSE))
  #  pipTable$JZS = kableExtra::cell_spec(x = pipTable$JZS, "html", bold =  ifelse(pipTable$JZS > .5, TRUE, FALSE))
  pipTable$ZS = kableExtra::cell_spec(x = pipTable$ZS,
                                      "html", bold =  ifelse(pipTable$ZS > .5, TRUE, FALSE))

  #---------------------------------------------------------------------------#
  #                    Model Posterior Probability Summary                    #
  #---------------------------------------------------------------------------#
  bicBMAPred <- predict(models[["BIC"]], estimator = "BMA")
  bicBMAPostProb <- bicBMAPred$postprobs[1]
  bmaAnalysis <- rbindlist(lapply(seq_along(models), function(m) {
    model <- list()

    # Obtain BMA Prediction for Highest Probability Model
    bmaPred <- predict(models[[m]], estimator = "BMA")
    model[["prior"]] <- priorDesc[[m]]
    model[["size"]] <- length(bmaPred$bestmodel[[bmaPred$best[[1]]]])
    model[["post"]] <- round(bmaPred$postprobs[1], 3)
    model[["pctBic"]] <- round((bmaPred$postprobs[1] - bicBMAPostProb) /
                                  bicBMAPostProb * 100, 3)
    model
  }))
  names(bmaAnalysis) <- c("Prior","Size", "Posterior Probability", "% BIC")

  analysis = list(
    postProbsPlot = postProbsPlot,
    pipDf = pip,
    pdc = pdc,
    pipTable = pipTable,
    pipPlots = plots,
    bmaAnalysis = bmaAnalysis
  )
  return(analysis)
}
