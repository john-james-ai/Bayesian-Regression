#==============================================================================#
#                             univariateQual                                   #
#==============================================================================#
#' univariateQual
#'
#' \code{univariateQual} Performs univariate analysis on a group of dichotomous
#' qualitative variables with uniform category levels. The function returns
#' a combined contingency table for all groups and categories, as well as
#' a stacked bar plot showing frequencies and percentages of categories
#' for each group.
#'
#' @param data Wide data frame containing:
#' \itemize{
#'  \item Character string containing the grouping variale
#'  \item Character string containing the category level
#'  \item Numeric containing the frequencies by category level
#'  \item Numeric indicating the proportions by category level
#'  \item Numeric containing the cumulative proportion by group and category level
#'  \item pos the vertical position of the labels on the bars
#' }
#' @param type Character string indicating whether data is dichotomous, ordinal, or nominal 
#' @param groups Character vector containing the group names to be rendered
#' along the x-axis
#' @param xLab Character string containing the label for the x-axis
#' @param yLab Character string indicating the label for the y-axis
#' @param plotTitle Character string indicating the plot title.
#'
#' @return analysis List containing:
#'  1. Contingency table
#'  2. Stacked Frequency Proportion Barplot
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family EDA functions
#' @export
univariateQual <- function(data, type = "dichotomous", groups, xLab, yLab = NULL, plotTitle = NULL) {

  # Format Data
  df <- rbindlist(lapply(seq_along(data), function(d) {
    df <- data[d] %>%  group_by("Category" = .[[1]]) %>%
      summarize(N = n()) %>%
      arrange(desc(Category)) %>%
      mutate(Group = groups[d],
             Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             pos = cumsum(N) - N /2) %>%
      select(Group, Category, N, Proportion, Cumulative, pos)
  }))
  
  if (type == "dichotomous") {
    chart <- plotStackedBar(data = df, xLab = xLab, yLab = yLab, plotTitle = plotTitle)
  } else if (type == "ordinal") {
    chart <- plotBar2(data = df, yLab = yLab, xLab = xLab, plotTitle = plotTitle)
  }

  visual <- list(
    stats = df[,1:5],
    plot = chart
  )
  return(visual)
}
