#==============================================================================#
#                             univariateQual                                   #
#==============================================================================#
#' univariateQual
#'
#' \code{univariateQual} Performs univariate analysis on a single qualitative
#' or categorical variable. The function returns a contingency table as well
#' as a stacked bar plot showing frequencies and percentages.
#'
#' @param data Single column data frame containing the categorical variable
#' @param xLab Capitalized character string for the variable name or label
#'
#' @return analysis List containing:
#'  1. Contingency table
#'  2. Frequency Proportion Barplot
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family EDA functions
#' @export
univariateQual <- function(data, xLab, yLab = NULL, plotTitle = NULL) {

  # Format Data
  df <- as.data.frame(data)
  df <- df %>%  group_by(.[[1]]) %>%
    summarize(N = n()) %>%
    mutate(Proportion = round(N / sum(N), 2),
           Cumulative = round(cumsum(Proportion), 2),
           pos = N / 2)
  colnames(df)[1] <- xLab
  df <- df %>% arrange(N)

  if (nrow(df) == 2) {
    chart <- pieChart(df, xLab = xLab)
  } else {
    chart <- plotBar2(df, yLab = yLab, xLab = xLab)
  }

  visual <- list(
    stats = df[,1:4],
    plot = chart
  )
  return(visual)
}
