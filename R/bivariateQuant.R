#==============================================================================#
#                             bivariateQuant                                   #
#==============================================================================#
#' bivariateQuant
#'
#' \code{bivariateQuant} Performs a bivariate analysis of quantitative independent
#' variables vis-a-vis the quantitative dependent variable.  Analyses include
#' the presentation of scatter plots, and correlation tests.
#'
#' @param data Two column data frame, containing the dependent and independent variables.
#' @param xLab Capitalized character string for the variable name or label
#'
#' @return analysis List containing:
#' \itemize{
#'  \item Scatterplot evincing the relationship between the independent and dependent variables.
#'  \item Correlation test.
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family EDA functions
#' @export
bivariateQuant <- function(data, xLab) {

  # Render boxplot
  sp <- plotScatter(data, xLab = xLab, yLab = "Audience Score", plotTitle = paste("Audience Score and", xLab))
  ct <- tidy(cor.test(y = data$y, x = data$x, method = "pearson"))

  # Render statement
  direction <- ifelse(ct$estimate > 0, "positive", "negative")
  strength <- ifelse(abs(ct$estimate) <= 0.3, "weak",
                     ifelse(abs(ct$estimate) <= 0.7, "moderate", "strong"))
  effect <- ifelse(direction == "positive", "an increase", "a decrease")
  seen <- sample(c("extant", "observed"), 1)

  statement <- paste0("A Pearson product-moment correlation coefficient was ",
                      "computed to assess the relationship between ", tolower(xLab),
                      " and audience scores. There was a ", direction,
                      " correlation between the two variables, r = ", round(ct$estimate, 3),
                      " n = ", ct$parameter, " p = ", round(ct$p.value, 3),
                      ". As supported by the scatterplot, ",
                      "a ", strength, " ", direction, " correlation ",
                      "between ", tolower(xLab), " and audience score was ", seen, ".")


  analysis = list(
    scatterPlot = sp,
    corTest = ct,
    statement = statement
  )

  return(analysis)
}
