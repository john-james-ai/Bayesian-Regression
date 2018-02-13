#==============================================================================#
#                                 associate                                    #
#==============================================================================#
#' associate
#'
#' \code{associate} Performs the associate analysis between a set of
#' independent variables (x) and a dependent variable (y).  Conducts several
#' ANOVA tests of independence
#'
#' @param x Data frame containing the explanatory variable data
#' @param y Data frame containing the response data
#' @param yLab Character string containing label for the response variable
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family regression functions
#' @export
associate <- function(x, y, yLab = NULL) {


  associations <- list()

  # Get variable names
  xNames <- colnames(x)
  yName <- ifelse(is.null(yLab), colnames(y), yLab)

  # Conduct association tests with response variable
  aTests <- data.table::rbindlist(lapply(seq_along(xNames), function(idx) {
    df <- data.frame(dependent = y[[1]],
                     independent = x[[idx]])
    mod <- lm(dependent ~ independent,  data = df)
    t <- anova(mod)
    data.frame(Dependent = yName,
               Independent = xNames[[idx]],
               `R-squared` = t$`Sum Sq`[1] / sum(t$`Sum Sq`),
               `F-value` = t$`F value`[1],
               `p-value` = ifelse(round(t$`Pr(>F)`[1], 3) < 0.05, "< 0.05",
                                  round(t$`Pr(>F)`[1], 3))
               )
  }))
  associations[["tests"]] <- aTests %>% arrange(desc(`R.squared`))

  return(associations)
}
