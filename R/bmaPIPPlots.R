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
  p$table$`EB-G` = kableExtra::cell_spec(x = p$table$`EB-G`, "html",
                                          bold =  ifelse(p$table$`EB-G` > .5, TRUE, FALSE))
  p$table$`EB-L` = kableExtra::cell_spec(x = p$table$`EB-L`, "html",
                                          bold =  ifelse(p$table$`EB-L` > .5, TRUE, FALSE))
  p$table$g = kableExtra::cell_spec(x = p$table$g, "html",
                                     bold =  ifelse(p$table$g > .5, TRUE, FALSE))
  p$table$HG = kableExtra::cell_spec(x = p$table$HG, "html",
                                      bold =  ifelse(p$table$HG > .5, TRUE, FALSE))
  p$table$`HG-L` = kableExtra::cell_spec(x = p$table$`HG-L`,
                                          "html", bold =  ifelse(p$table$`HG-L` > .5, TRUE, FALSE))
  p$table$`HG-N` = kableExtra::cell_spec(x = p$table$`HG-N`,
                                          "html", bold =  ifelse(p$table$`HG-N` > .5, TRUE, FALSE))
  #  p$table$JZS = kableExtra::cell_spec(x = p$table$JZS, "html", bold =  ifelse(p$table$JZS > .5, TRUE, FALSE))
  p$table$ZS = kableExtra::cell_spec(x = p$table$ZS,
                                      "html", bold =  ifelse(p$table$ZS > .5, TRUE, FALSE))
  
  return(p)
}
