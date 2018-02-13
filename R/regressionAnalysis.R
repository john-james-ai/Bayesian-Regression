#==============================================================================#
#                             regressionAnalysis                               #
#==============================================================================#
#' regressionAnalysis
#'
#' \code{regressionAnalysis} Performs regression analysis
#'
#' @param mod Linear model
#' @param mName Capitalized character string for the name of the linear model
#' @param yVar Character string containing the name of the dependent variable
#' @param yLab Capitalized character string for the dependent variable
#' @param full Logical indicating whether a full analysis should be conducted
#'
#' @return analysis plots, summary statistics, test results and interpretive text
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
regressionAnalysis <- function(mod, mName, yVar, yLab, full = TRUE) {

  #---------------------------------------------------------------------------#
  #                                 Summary                                   #
  #---------------------------------------------------------------------------#
  # Summarize Model Coefficients
  modCoef <- coef(mod)
  modConfInt <- confint(modCoef)
  cNames <- c("Term", "Post Mean", "Post SD", colnames(modConfInt))
  rNames <- data.frame(Term = rownames(modConfInt))
  modCoefDf <- as.data.frame(cbind(modCoef$postmean, modCoef$postsd, modConfInt), row.names = FALSE)
  modCoefDf <- cbind(rNames, modCoefDf)
  colnames(modCoefDf) <- cNames
  modCoefDf <- modCoefDf[modCoefDf[,3] > 0,1:5]
  modConfIntDf <- modCoefDf[,c(1,2,4,5)]

  analysis <- list()
  analysis[["name"]] <- mName
  analysis[["mod"]] <- mod
  analysis[["coefficients"]] <- modCoefDf

  if (full == TRUE) {

    #---------------------------------------------------------------------------#
    #                             Format Data                                   #
    #---------------------------------------------------------------------------#
    # Extract model predictors
    varMask <- c(0, mod$probne0[2:length(mod$probne0)])
    vars <- mod$model[,as.logical(varMask), drop = F]

    # Computed fitted vs residual
    y <- mod$model[1]$audience_score
    yHat <- fitted(mod, estimator = "BMA")
    res <- y - yHat
    rvf <- data.frame("Residual" = res, "Fitted" = yHat)

    #---------------------------------------------------------------------------#
    #                                  Plots                                    #
    #---------------------------------------------------------------------------#
    plots <- list()

    plots[["multicollinearity"]] <- plotCorr(mod = vars, yVar = yVar)


    # Residuals Histogram
    plots[["res_hist"]] <- plotHist(data = as.data.frame(res), yLab = paste("Residuals:", yLab),
                                    plotTitle = paste("Distribution of Residuals:", yLab))

    # Residuals Normal QQ Plot
    plots[["res_qq"]] <- plotResQQ(res = data.frame(Residuals = res), mName = mName)

    # Residuals vs Fitted
    plots[["res_fitted"]] <- p <- list(plot = plot(mod, which = 1))

    # Coefficient Plots
    plots[["coefficients"]] <- p <-  plot(modCoef, ask = F)

    plots[["confint"]] <- plotCIBars(data = modConfIntDf,
                                     plotTitle = paste(mName, "Posterior Means and Credible Intervals"))

    analysis[["plots"]] <- plots

    #---------------------------------------------------------------------------#
    #                           Assumptions Tests                               #
    #---------------------------------------------------------------------------#
    tests <- list()

    # Normality Test (nt)
    tests[["normal_res"]] <- shapiro.test(res)

    # Correlation Test (ct) if more than 1 factor and all numeric
    classes <- lapply(vars, class)
    if ((isTRUE(grepl("numeric", classes))) & length(vars) > 2) {
      tests[["correlation"]] <- psych::corr.test(vars)
    }

    # Influential Points
    cd <- stats::cooks.distance(mod)
    tests[["influential"]] <- as.numeric(names(cd)[(cd > 4 * mean(cd, na.rm=T))])

    analysis[["tests"]] <- tests

    #---------------------------------------------------------------------------#
    #                             Format Writeup                                #
    #---------------------------------------------------------------------------#

    comments <- list()

    phrase0 <- c("The effect of ", "The influence of ", "The significance of ", "The force of ")
    phrase1 <- c(" yielded an F statistic of F(", " presented an F statistic of F(",
                 " indicated an F statistic of F(", " produced an F statistic of F(")
    phrase2 <- c(" representing ", " accounting for ", " expressing ", " exhibiting ")
    phrase3 <- c(" represented ", " accounted for ", " expressed ", " exhibited ")
    phrase4 <- c("some ", "approximately ", "a ")
    intro <- paste0("A two-way analysis of variance was conducted on the influence ",
                    "of ", (nrow(a)-1), " independent ", ifelse(nrow(a)-1 == 1, "variable", "variables"),
                    " on the ", tolower(yLab), ". ")
    close <- paste0("The model was significant (F(", s$df[1], ", ", s$df[2], ")",
                    " = ", round(s$fstatistic[1], 3), ", ", "p < ",
                    ifelse(a$`Pr(>F)`[1] < .001, ".001",
                           ifelse(a$`Pr(>F)`[1] < .01, ".01",
                                  ifelse(a$`Pr(>F)`[1] < .05, ".05", round(a$`Pr(>F)`[1], 3)))),
                    "), with an adjusted R-squared of ", round(s$adj.r.squared, 3),
                    ". ")

    if (tests$homoscedasticity$p  < 0.05) {
      comments[["homoscedasticity"]] <- paste0("An examination of the residuals plot revealed unequal ",
                                               "disperson of residuals about the mean. ",
                                               "A Breusch-Pagan test was conducted to ",
                                                "test the homoscedasticity assumption.  The results ",
                                                "were significant (F(", tests$homoscedasticity$Df,
                                                "), p < ",
                                                ifelse(tests$homoscedasticity$p < .001, ".001",
                                                       ifelse(tests$homoscedasticity$p < .01, ".01",
                                                              ifelse(tests$homoscedasticity$p < .05,
                                                                     ".05", round(tests$homoscedasticity$p, 3)))),
                                                ").  As such, the null hypothesis of homoscedasticity is ",
                                               "rejected and therefore, the homoscedasticity assumption ",
                                               "was not met this case. ")
    } else {
      comments[["homoscedasticity"]] <- paste0("The residuals plot above indicated equal dispersion ",
                                               "disperson of residuals about the mean. ",
                                               "A Breusch-Pagan test was conducted to ",
                                               "test the homoscedasticity assumption.  The results ",
                                               "were not significant (F(", tests$homoscedasticity$Df,
                                               "), p = ", round(tests$homoscedasticity$p, 3),
                                               ").  As such the homoscedasticity assumption was met in this case.  ")
    }

    if (tests$normal_res$p.value[1] > 0.001) {
      comments[["normality"]] <- paste0("The histogram and normal Q-Q plot suggested a nearly normal ",
                                        "distribution of residuals.  A review of the Shapiro-Wilk ",
                                        "test (alpha = 0.001, SW = ", round(tests$normal_res$statistic, 3), ", p = ",
                                        round(tests$normal_res$p.value,3),
                                        ") and the skewness (",round(moments::skewness(res), 3), ") and ",
                                        "kurtosis (", round(moments::kurtosis(res), 3), ") supported ",
                                        "the assumption of normaility.  ")
    } else {
      comments[["normality"]] <- paste0("The histogram and normal Q-Q plot did not suggest a normal ",
                                        "distribution of residuals.  A review of the Shapiro-Wilk  ",
                                        "test (alpha = 0.001, SW = ", round(tests$normal_res$statistic, 3), ", p = ",
                                        round(tests$normal_res$p.value, 3),
                                        ") and the skewness (", round(moments::skewness(res), 3), ") and ",
                                        "kurtosis (", round(moments::kurtosis(res),3), ") indicated that ",
                                        "normality of residuals was not a reasonable assumption ",
                                        "for this model. ")
    }

    if (g$Size > 1) {

      if (maxVif < 4) {
        comments[["collinearity"]] <- paste0("collinearity did not appear extant for this model.  ",
                                             "Variance inflation factors were computed for each predictor in ",
                                             " the model.  The maximum VIF of ", round(maxVif,1),
                                             " did not exceed the threshold of 4. As such, the absense of ",
                                             "multicollinearity was assumed for this model.  ")
      } else {
        comments[["collinearity"]] <- paste0("collinearity appeared extant for this model.  ",
                                             "Variance inflation factors were computed for each predictor in ",
                                             " the model.  The maximum VIF of ", round(maxVif,1),
                                             " exceeded the threshold of 4. As such, the correlation among the  ",
                                             "predictors would require further consideration.  ")
      }
    }

    if (length(tests$influential) > 0) {
      comments[["outliers"]] <- paste0("Examination of the residuals versus leverage plot and case-wise ",
                                       "diagnostics such as Cook's distance revealed ", length(tests$influential),
                                       " cases exerting undue influence on the model.  ")
    } else {
      comments[["outliers"]] <- paste0("Examination of the residuals versus leverage plot and case-wise ",
                                       "diagnostics such as Cook's distance revealed no ",
                                       "cases exerting undue influence on the model.  ")
    }



    analysis[["comments"]] <- comments
  }
  return(analysis)
}
