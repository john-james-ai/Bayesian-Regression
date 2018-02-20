#==============================================================================#
#                    BMA Parameter Inclusion Probability                       #
#==============================================================================#
#' 
#'
#' \code{bmaPIP} Summarizes parameter inclusing probabilities across models
#'
#' @param models List of BMA models
#' @return Data frame containing parameter inclusion probabilities for each
#' model
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaPIP <- function(models) {

  #---------------------------------------------------------------------------#
  #                             Format Data                                   #
  #---------------------------------------------------------------------------#
  pip <-list()
  pip$data <- do.call(cbind, lapply(models, function(x) x$probne0))
  rownames(pip$data) <- gsub("yes", "", c(models[["BIC"]]$namesx))
  pip$plots <- bmaPIPPlots(pip$data)
  return(pip)
}
