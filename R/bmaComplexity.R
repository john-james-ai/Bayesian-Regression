#==============================================================================#
#                       Bayes Model Averaging Complexity                       #
#==============================================================================#
#' bmaComplexity
#'
#' \code{bmaComplexity} Plots model complexity vis-a-vis posterior probability
#'
#' @param models List of BMA models
#' @return list of reports
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaComplexity <- function(models) {
  
  plots <- lapply(models, function(m) {
    plotData <- data.frame(Prob = m$postprobs, Size = m$size)
    plotScatter(data = plotData, xLab = "Model Dimension",
                yLab = "Probability", smooth = FALSE,
                plotTitle = paste(m$prior,"Model Complexity"))
  })
  
  return(plots)
}
