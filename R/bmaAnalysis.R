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
#' @param estimator The estimator to be used for the coefficient computation.
#' @return List with descriptive statistics and credible intervals for the means
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
postDist <- function(m, estimator = "BMA") {

  pdc <- list()
  pdc$Prior <- m$prior
  pdc$desc <- m$priorDesc
  cf <- coef(m, estimator = estimator)
  ci <- confint(cf)
  df <- data.frame(Probability = cf$probne0,
                   Mean = cf$postmean,
                   SD = cf$postsd)
  pdc$df <- cbind(df, ci[,1:2])
  return(pdc)
}
