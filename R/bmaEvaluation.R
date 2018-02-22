#==============================================================================#
#                                BMA Evaluation                                #
#==============================================================================#
#' bmaEvaluation
#'
#' \code{bmaEvaluation} Prepares data by which the designated models would be evaluated.
#'
#' @param models List of candidate BAS.lm models
#' @param candidates Dataframe containing the candidate models to be evaluated.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaEvaluation <- function(models, candidates) {
  
  eval <- list()
  
  eval$pred <- apply(candidates[c(1:4),], 1, function(x) {
    cat(paste("\nProcessing", getElement(x, "Prior"), getElement(x, "Estimator")))
    bmaPredict(model = models[[getElement(x, "Prior")]], estimator = getElement(x, "Estimator"), prediction = FALSE, 
                    rvp = TRUE, pe = TRUE, pi = TRUE)
  })
  
}