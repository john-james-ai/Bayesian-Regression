#==============================================================================#
#         Bayes Model Averaging Parameter Inclusion Probability Plots          #
#==============================================================================#
#' bmaPIPPlots
#'
#' \code{bmaPIPPlots} Renders tables and plots of parameter inclusing probabilities 
#' 
#' Renders tables and plots of parameter inclusion probabilities by model variable. 
#' The table summarizes parameter inclusion probabilities across priors, highlighting
#' the probabilities including 0.5. Each plot reports inclusion probabilities 
#' of a single predictor across the  series of models.
#'
#' @param pip Data frame containing parameter inclusion probabilities from \code{bmaPIP.R}.
#' @return list of plots of parameter inclusion probabilities across models.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaPIPPlots <- function(pip) {
  
  p <- list()
  
  plotData <- as.data.frame(pip[2:nrow(pip),])
  
  p$plots <- list()
  for (i in 1:nrow(plotData)) {
    predictor <- gsub("yes", "", rownames(plotData)[i], fixed = TRUE)
    p$plots[[i]] <- plotBar2(data = data.frame(x = colnames(pip),
                                             y = as.numeric(plotData[i,])),
                           yLab = "Inclusion Probability", xLab = "Prior", 
                           plotTitle = paste("Inclusion Probabilities:",predictor),
                           values = TRUE)
  }
  
  p$table  <- as.data.frame(round(pip, 2))
  p$table$BIC = kableExtra::cell_spec(x = p$table$BIC, "html",
                                       bold =  ifelse(p$table$BIC > .5, TRUE, FALSE))
  p$table$AIC = kableExtra::cell_spec(x = p$table$AIC, "html",
                                       bold =  ifelse(p$table$AIC > .5, TRUE, FALSE))
  p$table$`EB-global` = kableExtra::cell_spec(x = p$table$`EB-global`, "html",
                                          bold =  ifelse(p$table$`EB-global` > .5, TRUE, FALSE))
  p$table$`EB-local` = kableExtra::cell_spec(x = p$table$`EB-local`, "html",
                                          bold =  ifelse(p$table$`EB-local` > .5, TRUE, FALSE))
  p$table$`g-prior` = kableExtra::cell_spec(x = p$table$`g-prior`, "html",
                                     bold =  ifelse(p$table$`g-prior` > .5, TRUE, FALSE))
  p$table$`hyper-g` = kableExtra::cell_spec(x = p$table$`hyper-g`, "html",
                                      bold =  ifelse(p$table$`hyper-g` > .5, TRUE, FALSE))
  p$table$`hyper-g-laplace` = kableExtra::cell_spec(x = p$table$`hyper-g-laplace`,
                                          "html", bold =  ifelse(p$table$`hyper-g-laplace` > .5, TRUE, FALSE))
  p$table$`hyper-g-n` = kableExtra::cell_spec(x = p$table$`hyper-g-n`,
                                          "html", bold =  ifelse(p$table$`hyper-g-n` > .5, TRUE, FALSE))
  #  p$table$JZS = kableExtra::cell_spec(x = p$table$JZS, "html", bold =  ifelse(p$table$JZS > .5, TRUE, FALSE))
  p$table$`ZS-null` = kableExtra::cell_spec(x = p$table$`ZS-null`,
                                      "html", bold =  ifelse(p$table$`ZS-null` > .5, TRUE, FALSE))
  
  return(p)
}
