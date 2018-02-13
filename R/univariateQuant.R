#==============================================================================#
#                             univariateQuant                                  #
#==============================================================================#
#' univariateQuant
#'
#' \code{univariateQuant} Performs univariate analysis on a single quantitative
#' variable.  The function returns descriptive statistics, a histogram
#' and a box plot to check outliers.
#'
#' @param data Single column data frame containing the quantitative variable
#' @param yLab Capitalized character string for the variable name or label
#' @param units Character string indicating the units associated with the quantitative variable.
#'
#' @return analysis List containing summary statistics, plots and intepretation text.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family EDA functions
#' @export
univariateQuant <- function(data, yLab, xLab = NULL, units = NULL) {

  stats <- getSummaryStats(as.data.frame(data[[2]]))
  hist <- plotHist(as.data.frame(data[[2]]), yLab = yLab)
  qq <- plotQQ(as.data.frame(data[[2]]), yLab = yLab)
  box  <- plotBox(as.data.frame(data[[2]]), yLab = yLab)

  #---------------------------------------------------------------------------#
  #                            Interpretation                                 #
  #---------------------------------------------------------------------------#
  # Compute values required for interpretation
  d <- ifelse(stats$CV <= 25, "low",
              ifelse(stats$CV <= 50, "moderate",
                     ifelse(stats$CV <= 75, "high", "very high")))
  s <- ifelse(round(stats$Skewness,0) == 0, "approximately symmetric",
              ifelse(round(stats$Skewness,0) < 0, "left-skewed", "right-skewed"))
  k <- ifelse(stats$Kurtosis == 0, "approximately normal",
              ifelse(stats$Kurtosis < 0, "platykurtic or light-tailed",
                     "leptokurtic or heavy-tailed"))
  lower <- stats$Q1 - (1.5 * stats$IQR)
  upper <- stats$Q3 + (1.5 * stats$IQR)
  outliers <- subset(data, data[[2]] < lower | data[[2]] > upper)
  outliers <- outliers %>%
    mutate(Title = as.character(.[[1]]),
           Variable = yLab,
           Value = .[[2]],
           PctIQR = round(ifelse(.[[2]] < lower, (lower - .[[2]]) / stats$IQR * 100,
                                 (.[[2]] - upper) / stats$IQR * 100)), 2) %>%
    select(Title, Variable, Value, PctIQR)
  numOutliers <- ifelse(stats$Outliers == 0, "no", stats$Outliers)


  central <- paste("the central tendency for", tolower(yLab), "was",
                   stats$Median, units, "and",  stats$Mean, units,
                   "for the median and mean, respectively.  ")
  disp <- paste0("The standard deviation, s = ", stats$SD,", corresponds with ",
                 "a coefficient of variation of ", stats$CV, "%, indicating ",
                 "a ", d, " degree of dispersion.  ")
  skew <- paste0("The sample skewness (", round(stats$Skewness, 2), "), ",
                 "indicated that the distribution of ", tolower(yLab), " ",
                 "was ", s, ".  ")
  kurt <- paste0("The sample kurtosis (", round(stats$Kurtosis, 2), "), ",
                 "indicated that the distribution of ", tolower(yLab), " ",
                 "was ", k, ".  ")
  out <- paste0("The 25%, 75%, and IQR were ", stats$Q1, ", ", stats$Q3,", and ",
                stats$IQR, ", respectively. This yielded a 1.5xIQR 'acceptable' ",
                "range [", lower,", ", upper, "]. Indeed, this confirmed the ",
                "existence of ", numOutliers, " outliers. A case-wise review ",
                "of the influential points revealed no data quality errors. As ",
                "such, the influential points would be retained for further ",
                "analysis.  ")


  analysis <- list(
    stats = stats,
    hist = hist,
    qq = qq,
    box = box,
    central = central,
    disp = disp,
    skew = skew,
    kurt =kurt,
    out = out,
    outliers = outliers
  )

  return(analysis)
}
