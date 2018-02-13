#==============================================================================#
#                             bivariateQual                                    #
#==============================================================================#
#' bivariateQual
#'
#' \code{bivariateQual} Performs a bivariate analysis of categorical independent
#' variables vis-a-vis the quantitative dependent variable.  Analyses include
#' the presentation of box plots, summary statistics by categorical level, and
#' ANOVA tests of the difference in mean values of the dependent variable by
#' categorical level.
#'
#' @param data Two column data frame, containing the dependent and independent variables.
#' @param xLab Capitalized character string for the variable name or label
#'
#' @return analysis List containing:
#' \itemize{
#'  \item Data frame containing summary statistics by categorical level.
#'  \item Boxplot showing the distribution of dependent variable by categorical level.
#'  \item Anova test of equal means.
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family EDA functions
#' @export
bivariateQual <- function(data, xLab) {

  # Render boxplot
  bp <- plotBox(data, xLab = xLab, yLab = "Audience Score", plotTitle = paste("Audience Scores by", xLab))

  # Summary statistics by level
  stats <- as.data.frame(data %>% group_by(x) %>%
                           summarise(Min = min(y),
                                     Max = max(y),
                                     Median = median(y),
                                     Mean = round(mean(y), 1),
                                     IQR = IQR(y),
                                     SD = round(sd(y), 1),
                                     N = n()))

  # Anova test
  at <- tidy(aov(formula = y ~ x, data = data))
  significance <- ifelse(at$p.value[1] < .05, "significant", "non significant")

  # Statement

  statement <- paste0("A one-way ANOVA was conducted to compare the effect of " ,
                      tolower(xLab), " on average audience scores. The effect ",
                      "was ", significance, " at the p<.05 level ",
                      "[F(",at$df[1],",",at$df[2],") = ", round(at$statistic[1],0),
                      ", p = ", round(at$p.value[1], 3),"].")

  analysis = list(
    boxPlot = bp,
    stats = stats,
    test = at,
    statement = statement
  )

  return(analysis)
}
