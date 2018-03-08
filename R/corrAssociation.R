#------------------------------------------------------------------------------#
#                                  ChiSquare                                   #
#------------------------------------------------------------------------------#
#' x2
#'
#' \code{x2} Chi square tests are conducted to test the significance
#' of association between a set of categorical variables.
#'
#' @param data Data frame or vector containing a single quantitative variable
#'
#' @return Data frame containing the p-values for the associations between 
#' each of the categorical variables in the data set. 
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family analysis functions
#' @export
x2 <- function(data) {
  
  # Extract categorical data
  vars <- c("Feature Film", "Drama", "R Rating", "Year", "Oscar Season", "Summer Season",
            "Best Pic Nom", "Best Pic Win", "Best Actor", "Best Actress", 
            "Best Director", "Top 200")
  df <- data %>% select(feature_film, drama, mpaa_rating_R, thtr_rel_year, 
                             oscar_season, summer_season, best_pic_nom,
                             best_pic_win, best_actor_win, best_actress_win,
                             best_dir_win, top200_box) 
  
  x <- matrix(data = NA, nrow = 12, ncol = 12)
  
  for (i in 1:ncol(df)) {
    for (j in 1:ncol(df)) {
      if (i != j) {
        tbl <- table(unlist(df[,i]), unlist(df[,j]))
        test <- chisq.test(tbl)
        x[i,j] = round(test$p.value, 3)
      }
    }
  }
  
  dimnames(x) <- list(vars, vars)
  x <- as.data.frame(x)

  return(x)
}
#------------------------------------------------------------------------------#
#                                  Cramer's V                                  #
#------------------------------------------------------------------------------#
#' cramers
#'
#' \code{cramers} Cramer's V test of strength of association
#'
#' @param data Data frame or vector containing a single quantitative variable
#'
#' @return Data frame containing the Cramer's V values for the associations between 
#' each of the categorical variables in the data set. 
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family analysis functions
#' @export
cramers <- function(data) {
  
  # Extract categorical data
  vars <- c("Feature Film", "Drama", "R Rating", "Year", "Oscar Season", "Summer Season",
            "Best Pic Nom", "Best Pic Win", "Best Actor", "Best Actress", 
            "Best Director", "Top 200")
  df <- data %>% select(feature_film, drama, mpaa_rating_R, thtr_rel_year, 
                        oscar_season, summer_season, best_pic_nom,
                        best_pic_win, best_actor_win, best_actress_win,
                      best_dir_win, top200_box) 
  
  y <- matrix(data = NA, nrow = 12, ncol = 12)
  
  for (i in 1:ncol(df)) {
    for (j in 1:ncol(df)) {
      if (i != j) {
        tbl <- table(unlist(df[,i]), unlist(df[,j]))
        test <- assocstats(tbl)
        y[i,j] = round(test$cramer, 3)
      }
    }
  }
  
  dimnames(y) <- list(vars, vars)
  y <- as.data.frame(y)
  
  return(y)
}


#------------------------------------------------------------------------------#
#                                  Pearsons v                                  #
#------------------------------------------------------------------------------#
#' pearsons
#'
#' \code{pearsons} Pearsons coefficients are computed to measure correlation
#' among quantitative variables
#'
#' @param data Data frame or vector containing a single quantitative variable
#'
#' @return Correlation plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family analysis functions
#' @export
pearsons <- function(data) {
  
  vars <- c("Runtime", "IMDb Rating", "IMDb Num Votes (Log)", "Critics Score",
            "Critics Score (Log)")
  df <- data.frame(data$runtime, data$imdb_rating, data$imdb_num_votes_log,
                   data$critics_score, data$imdb_num_votes_sqrt)
  
  cp <- plotCorr(data = df)
  return(cp)
}