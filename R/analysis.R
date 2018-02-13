#==============================================================================#
#                              Analysis Functions                              #
#==============================================================================#

#------------------------------------------------------------------------------#
#                   Get Summary Stats for Quantitative Variable                #
#------------------------------------------------------------------------------#
#' getSummaryStats
#'
#' \code{getSummaryStats} Provides summary stats for quantitative variable
#'
#' @param data Data frame or vector containing a single quantitative variable
#' @param xLab Character string containing the name of the variable or categorical level
#'
#' @return Data frame containing summary statistics
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family analysis functions
#' @export
getSummaryStats <- function(data, xLab = NULL) {

  df <- data.frame(N = length(data[[1]]),
                   Min = round(min(data[[1]]), 1),
                   Q1 = round(quantile(data[[1]], 0.25), 1),
                   Median = round(median(data[[1]]), 1),
                   Mean = round(mean(data[[1]]), 1),
                   Q3 = round(quantile(data[[1]], 0.75), 1),
                   IQR = round(quantile(data[[1]], 0.75) - quantile(data[[1]], 0.25) , 1),
                   Max = round(max(data[[1]]), 1),
                   `NA's` = sum(is.na(data[[1]])),
                   SD = round(sd(data[[1]]), 2),
                   CV = round(sd(data[[1]]) / mean(data[[1]]) * 100, 1),
                   Kurtosis = e1071::kurtosis(data[[1]], type = 1),
                   Skewness = e1071::skewness(data[[1]], type = 1),
                   row.names = NULL)

  lower <- df$Q1 - (1.5 * df$IQR)
  upper <- df$Q3 + (1.5 * df$IQR)
  df$Outliers <- nrow(subset(data, data[[1]] < lower | data[[1]] > upper))

  # If x is present, add to data frame as first column
  if (!is.null(xLab)) {
    g <- data.frame(Group = xLab)
    df <- cbind(g, df)
  }

  return(df)

}


#------------------------------------------------------------------------------#
#                 Estimate Sample Size Required for a Proportion               #
#------------------------------------------------------------------------------#
#' getPSampleSize
#'
#' \code{getPSampleSize} Computes the required sample size for a proportion, such
#' that the population proportion is within a designated margin of error of the
#' sample proportion with a designated level of confidence.
#'
#' @param p Numeric proportion between 0 and 1
#' @param conf Numeric confidence level between 0 and 1
#' @param me Numeric margin of error between 0 and 1
#'
#' @return sampleSize Minimum required sample size
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family analysis functions
#' @export
getPSampleSize <- function(p, conf = 0.95, me = 0.05) {

  alpha <- 1 - conf
  z <- alpha/2
  ss <- round((qnorm(z)^2 * p * (1 - p)) / me^2, 0)

  return(ss)
}




#------------------------------------------------------------------------------#
#            Estimate Sample Size Required for Continuous Outcomes             #
#------------------------------------------------------------------------------#
#' getCSampleSize
#'
#' \code{getCSampleSize} Computes the required sample size for a proportion, such
#' that the population proportion is within a designated margin of error of the
#' sample proportion with a designated level of confidence.
#'
#' @param e Numeric indicator of effect size
#' @param p Numeric indicator of the target power
#' @param me Numeric indicator of alpha level
#'
#' @return sampleSize Minimum required sample size
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family analysis functions
#' @export
getCSampleSize <- function(data, e, p = 0.80, me = 0.05) {

  z1 <- qnorm(me/2, lower.tail = FALSE)   # Area under curve for the desired margin of error
  z2 <- qnorm(p)                          # Area under curve associated with power
  z <- z1 + z2                            # Effect size in standard errors
  s <- sd(data)                           # Estimate for population standard deviation
  n <- (z * s / e)^2                      # Sample size estimate

  return(n)
}

