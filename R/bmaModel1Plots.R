#==============================================================================#
#                   Bayes Model Averaging Model 1 Plots                        #
#==============================================================================#
#' bmaModel1Plots
#'
#' \code{bmaModel1Plots} Renders plots for BMA model 1 for a series of models 
#'
#' @param m1s Data frame containing the following Model 1 summary information:
#' \itemize{
#'  \item Prior - Character string containing the name of the prior
#'  \item Posterior Probability Numeric - containing the posterior probability of the top model for each prior
#'  \item R2 - Numeric representing the R2 value for the top model for each prior.
#'  \item Size - Integer indicating the number of parameters in the top model
#' }
#' @return list of containing the following plots:
#' \itemize{
#'  \item postProbs - Plot of posterior model probabilities for the top model of each prior.
#'  \item r2 - Plot of the R2 score for the top model of each prior.
#'  \item size - Plot containing the model size for the top model of each prior. 
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaModel1Plots <- function(m1s) {
  m1Plots <- list()
  m1Plots$postProbs <- plotBar(data = m1s[,c(1,2)], yLab = "Posterior Probability",
                            xLab = "Prior", legend = FALSE, horizontal = TRUE,
                            plotTitle = "Highest Probability Model",
                            values = TRUE)
  m1Plots$r2 <- plotBar2(data = m1s[,c(1,3)], yLab = "R2",
                            xLab = "Prior",
                            plotTitle = "R2",
                            values = TRUE)
  m1Plots$size <- plotBar2(data = m1s[,c(1,4)], yLab = "Model Size",
                       xLab = "Prior",
                       plotTitle = "Model Size",
                       values = TRUE)
  return(m1Plots)
}
