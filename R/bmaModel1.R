#==============================================================================#
#                       Bayes Model Averaging Model 1                          #
#==============================================================================#
#' bmaModel1
#'
#' \code{bmaModel1} Summarizes model 1 under BMA for a series of models
#'
#' @param models List of BMA models
#' @return A list containing:
#' \itemize{
#'  \item m1s Data frame containing posterior model probabilities, R2 scores and model sizes for each prior.
#'  \item m1Plots - List of plots summarizing posterior probabilities, R2, and model sizes across each prior. 
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaModel1 <- function(models) {
  
  m1s <- list()
  
  m1s$data <- rbindlist(lapply(models, function(m) {
    s <- summary(m)
    m1 <- list()
    m1$Prior <- m$prior
    m1$`Posterior Probability` <- s[18,2]
    m1$R2 <- s[19,2]
    m1$Size <- s[20,2]
    m1
  })) 
  
  m1s$plots <- bmaModel1Plots(m1s = m1s$data)
  m1s$data <- m1s$data %>% arrange(desc(`Posterior Probability`))
  meanProb <- mean(m1s$data$`Posterior Probability`)
  m1s$data$pctMean <- round((m1s$data$`Posterior Probability` - meanProb) / meanProb * 100, 1)
  return(m1s)
}