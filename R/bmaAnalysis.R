#==============================================================================#
#                       Bayes Model Averaging Reports                          #
#==============================================================================#
#' bmaAnalysis
#'
#' \code{bmaAnalysis} Reports results from a series of BMA Regression for a series of models.
#'
#' @param models List of BMA models
#' @return list of reports
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaAnalysis <- function(models, top = FALSE) {
  
  # Obtain parameter inclusion probabilities
  pip <- bmaPIP(models = models)
  
  # Plot model complexity vis-a-vis posterior probability
  complexity <- bmaComplexity(models = models) 
  
  # Obtain posterior probability of coefficients under BMA
  pdc <- lapply(models, function(m) {
    bmaPDC(m)
  })
  
  analysis = list(
    pip = pip,
    pdc = pdc,
    complexity = complexity
  )
  return(analysis)
}
