#==============================================================================#
#                                BMA Evaluation                                #
#==============================================================================#
#' bmaEvaluation
#'
#' \code{bmaEvaluation} Prepares data by which the designated models would be evaluated.
#'
#' @param mList List of candidate BAS.lm models
#' @param candidates Dataframe containing the candidate models to be evaluated.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaEvaluation <- function(mList, candidates) {
  
  eval <- list()
  
  eval <- apply(candidates[c(1:4),], 1, function(x) {
    bmaPredict(model = mList[[getElement(x, "Prior")]], estimator = getElement(x, "Estimator"), prediction = FALSE, 
                    rvp = TRUE, pe = TRUE, predObj = TRUE)
  })
  
  eval$rvp <- lapply(seq(1:4), function(x) {
    plotScatter(data = eval[[x]]$rvp, xLab = "Fitted", yLab = "Residuals",
                          plotTitle = paste0( "Residual vs Fitted ",
                                              eval[[x]]$prior, " (", 
                                              eval[[x]]$estimator, ")"))
  })
  
  eval$pdc <- lapply(seq(1:4), function(x) { eval[[x]]$pe[[1]]$plot })
  
  return(eval)
}