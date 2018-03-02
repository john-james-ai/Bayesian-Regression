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
  
  eval$models <- apply(candidates[c(1:4),], 1, function(x) {
    bmaPredict(model = mList[[getElement(x, "Prior")]], estimator = getElement(x, "Estimator"), prediction = FALSE, 
                    rvp = TRUE, pe = TRUE, predObj = TRUE)
  })
  
  eval$rvp <- lapply(seq(1:4), function(x) {
    plotScatter(data = eval$models[[x]]$rvp, xLab = "Fitted", yLab = "Residuals",
                          plotTitle = paste0( "Residual vs Fitted ",
                                              eval$models[[x]]$prior, " (", 
                                              eval$models[[x]]$estimator, ")"))
  })
  
  eval$pdc <- lapply(seq(1:4), function(x) {
    n <- rownames(eval$models[[1]]$pe[[1]]$df)
    df <- eval$models[[x]]$pe[[1]]$df
    df <- cbind(Terms = n, df)
    df <- df %>% filter(Probability == 1) %>% select(-Probability)
    df
  }) 
  names(eval$pdc) <- unlist(lapply(seq(1:4), function(x) {
    paste0(eval$models[[x]]$prior, " (", eval$models[[x]]$estimator,")" )
  }))
  return(eval)
}