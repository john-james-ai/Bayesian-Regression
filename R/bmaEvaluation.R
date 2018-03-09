#==============================================================================#
#                                BMA Evaluation                                #
#==============================================================================#
#' bmaEvaluation
#'
#' \code{bmaEvaluation} Prepares data by which the designated models would be evaluated.
#'
#' @param mList List of candidate BAS.lm models
#' @param candidates Dataframe containing the candidate models to be evaluated.
#' @param top Integer indicating the number of top models to evaluate
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaEvaluation <- function(mList, candidates, top = 10) {
  
  eval <- list()
  
  eval$models <- apply(candidates[c(1:top),], 1, function(x) {
    bmaPredict(model = mList[[getElement(x, "Prior")]], estimator = getElement(x, "Estimator"), prediction = FALSE, 
                    rvp = TRUE, pe = TRUE, predObj = TRUE)
  })
  
  eval$rvp <- lapply(seq(1:top), function(x) {
    plotScatter(data = eval$models[[x]]$rvp, xLab = "Fitted", yLab = "Residuals",
                          plotTitle = paste0( "Residual vs Fitted ",
                                              eval$models[[x]]$prior, " (", 
                                              eval$models[[x]]$estimator, ")"))
  })
  
  eval$pdc <- lapply(seq(1:top), function(x) {
    n <- rownames(eval$models[[1]]$pe[[1]]$df)
    df <- eval$models[[x]]$pe[[1]]$df
    df <- cbind(Terms = n, df)
    df <- df %>% filter(Probability > 0) %>% select(-Probability)
    df
  }) 
  names(eval$pdc) <- unlist(lapply(seq(1:top), function(x) {
    paste0(eval$models[[x]]$prior, " (", eval$models[[x]]$estimator,")" )
  }))
  
  eval$pe <- lapply(seq_along(eval$pdc), function(x) {
    pdc <- cbind(Model = data.frame(names(eval$pdc[x])),eval$pdc[[x]])
    pdc <- pdc %>% select(c(1:3))
  })
  
  # Render parameter estimate matrix
  eval$pe <- rbindlist(eval$pe)
  names(eval$pe) <- c('Model', 'Terms', 'Mean')
  eval$pe <- dcast(eval$pe, Terms ~ Model, value.var = "Mean")
  eval$pe[is.na(eval$pe)] <- 0
  intercept <- eval$pe %>% filter(Terms == "Intercept")
  eval$pe <- rbind(intercept, eval$pe %>% filter(Terms != "Intercept"))
  return(eval)
}