#==============================================================================#
#                   Bayes Model Averaging Analysis Functions                   #
#==============================================================================#

#------------------------------------------------------------------------------#
#                   Posterior Distributino of Coefficients                     #
#------------------------------------------------------------------------------#
#' postDist
#'
#' \code{postDist} Reports the posterior distribution of coefficients for a model.
#'
#' @param m A BAS.lm object.
#' @param estimator Estimator to be used when calculating the coefficients
#' @return List with descriptive statistics and credible intervals for the means
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
postDist <- function(m, estimator = NULL) {
  
  if (is.null(estimator)) estimator <- c("BMA", "BPM", "HPM", "MPM")
  
  pdc <- lapply(estimator, function(e) {
    pdc <- list()
    pdc$Prior <- m$prior
    pdc$desc <- m$priorDesc
    pdc$estimator <- e
    cf <- coef(m, estimator = e)
    ci <- confint(cf)
    df <- data.frame(Probability = cf$probne0,
                     Mean = cf$postmean,
                     SD = cf$postsd)
    pdc$df <- cbind(df, ci[,1:2])
    pdc$ci <- ci
    plotData <- data.frame(Term = rownames(pdc$df), Mean = pdc$df$Mean, 
                           Low = pdc$df[,4], High = pdc$df[,5])
    pdc$plot <- plotCIBars(plotData, 
                           plotTitle = paste(pdc$desc, 
                                             "(", e, ")"))
    pdc
  })
  names(pdc) <- estimator
  
  return(pdc)
}
