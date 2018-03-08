#==============================================================================#
#                                 univariate                                   #
#==============================================================================#
#' univariate
#'
#' \code{univariate} Performs univariate analysis of variables
#'
#' @param data Data frame containing data for analysis
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family EDA functions
#' @export
univariate <- function(data) {
  
  analysis <- list()
  #---------------------------------------------------------------------------#
  #                       Conduct Qualitative Analysis                        #
  #---------------------------------------------------------------------------#
  analysis$qual <- list()
  
  # Film Summary
  #---------------------------------------------------------------------------#
  groups <- c("Feature Film", "Drama", "MPAA R Rating")
  df <- data.frame(data$feature_film, data$drama, data$mpaa_rating_R, 
                   stringsAsFactors = FALSE)
  analysis$qual$summary <- univariateQual(data = df, groups = groups,
                                              xLab = "",
                                              yLab = "Films",
                                              plotTitle = "Films Summary")
  #---------------------------------------------------------------------------#
  
  # Performance Summary
  #---------------------------------------------------------------------------#
  groups <- c("Best Actor", "Best Actress", "Best Director",
              "Best Picture Nomination", "Best Picture",
              "Top 200 Box Office")
  df <- data.frame(data$best_actor_win, data$best_actress_win,
                            data$best_dir_win, data$best_pic_nom,
                            data$best_pic_win, data$top200_box,
                            stringsAsFactors = FALSE)
  analysis$qual$performance <- univariateQual(data = df, groups = groups,
                                              xLab = "",
                                              yLab = "Films",
                                              plotTitle = "Performance Summary")
  
  
  # Release Season
  #---------------------------------------------------------------------------#
  groups <- c("Oscar Season", "Summer Season")
  df <- data.frame(data$oscar_season, data$summer_season,
                   stringsAsFactors = FALSE)
  analysis$qual$season <- univariateQual(data = df, groups = groups,
                                              xLab = "",
                                              yLab = "Films",
                                              plotTitle = "Season of Theatrical Release")
  
  # Release Year
  #---------------------------------------------------------------------------#
  groups <- c("Year")
  df <- data.frame(as.character(data$thtr_rel_year), stringsAsFactors = FALSE)
  analysis$qual$year <- univariateQual(data = df, type = "ordinal", groups = groups,
                                          xLab = "Year",
                                          yLab = "Films",
                                          plotTitle = "Year of Theatrical Release")

  #---------------------------------------------------------------------------#
  #                        Conduct Quantative Analysis                        #
  #---------------------------------------------------------------------------#
  analysis$quant$runtime <- univariateQuant(data.frame(data$title, data$runtime),
                             yLab = "Runtime", units = "minutes")
  analysis$quant$imdb_rating <- univariateQuant(data.frame(data$title, data$imdb_rating),
                                 yLab = "IMDB Rating", units = "points")
  analysis$quant$imdb_num_votes <- univariateQuant(data.frame(data$title, data$imdb_num_votes),
                               yLab = "IMDB Num Votes", units = "votes")
  analysis$quant$imdb_num_votes_log <- univariateQuant(data.frame(data$title, data$imdb_num_votes_log),
                                    yLab = "IMDB Num Votes (Log)", units = "votes")
  analysis$quant$imdb_num_votes_sqrt <- univariateQuant(data.frame(data$title, data$imdb_num_votes_sqrt),
                                                       yLab = "IMDB Num Votes (Sqrt)", units = "votes")
  analysis$quant$critics_score <- univariateQuant(data.frame(data$title, data$critics_score),
                                   yLab = "Critics Score", units = "points")
  analysis$quant$audience_score <- univariateQuant(data.frame(data$title, data$audience_score),
                             yLab = "Audience Score", units = "points")

  return(analysis)
}
