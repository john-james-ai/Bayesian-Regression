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
  
  # Obtain top model summaries
  m1s <- bmaModel1(models = models)
  
  # Obtain posterior probability of coefficients under BMA
  pdc <- lapply(models, function(m) {
    bmaPDC(m)
  })
  
  analysis = list(
    pip = pip,
    m1s = m1s,
    pdc = pdc
  )
  return(analysis)
}
