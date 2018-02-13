#==============================================================================#
#                                  bivariate                                   #
#==============================================================================#
#' bivariate
#'
#' \code{bivariate} Performs the bivariate analysis of independent variables on audience score.
#'
#' @param data Data frame containing data to be analyzed
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family EDA functions
#' @export
bivariate <- function(data) {

  #---------------------------------------------------------------------------#
  #                       Conduct Qualitative Analysis                        #
  #---------------------------------------------------------------------------#
  feature_film <- bivariateQual(data.frame(y = data$audience_score,
                                           x = data$feature_film),
                                xLab = "Feature Film")

  drama <- bivariateQual(data.frame(y = data$audience_score,
                                    x = data$drama),
                         xLab = "Drama")

  mpaa_rating_R <- bivariateQual(data.frame(y = data$audience_score,
                                            x = data$mpaa_rating_R),
                                 xLab = "MPAA R Rating")

  oscar_season <- bivariateQual(data.frame(y = data$audience_score,
                                           x = data$oscar_season),
                                xLab = "Oscar Season Release")

  summer_season <- bivariateQual(data.frame(y = data$audience_score,
                                            x = data$oscar_season),
                                 xLab = "Summer Season Release")

  best_pic_nom <- bivariateQual(data.frame(y = data$audience_score,
                                           x = data$best_pic_nom),
                                xLab = "Best Picture Oscar Nomination")

  best_pic_win <- bivariateQual(data.frame(y = data$audience_score,
                                           x = data$best_pic_win),
                                xLab = "Best Picture Oscar")

  best_dir_win <- bivariateQual(data.frame(y = data$audience_score,
                                           x = data$best_dir_win),
                                xLab = "Best Director Oscar")

  best_actor_win <- bivariateQual(data.frame(y = data$audience_score,
                                             x = data$best_actor_win),
                                  xLab = "Best Actor Oscar")

  best_actress_win <- bivariateQual(data.frame(y = data$audience_score,
                                               x = data$best_actress_win),
                                    xLab = "Best Actress Oscar")

  top200_box <- bivariateQual(data.frame(y = data$audience_score,
                                         x = data$top200_box),
                              xLab = "Top 200 Box Office")

  #---------------------------------------------------------------------------#
  #                        Conduct Quantative Analysis                        #
  #---------------------------------------------------------------------------#
  thtr_rel_year <- bivariateQuant(data = data.frame(y = data$audience_score,
                                                    x = as.numeric(data$thtr_rel_year)),
                                  xLab = "Year of Theatrical Release")

  runtime <- bivariateQuant(data.frame(y = data$audience_score,
                                       x = data$runtime),
                            xLab = "Runtime")

  imdb_rating <- bivariateQuant(data.frame(y = data$audience_score,
                                           x = data$imdb_rating),
                                xLab = "IMDB Rating")

  imdb_num_votes <- bivariateQuant(data.frame(y = data$audience_score,
                                              x = data$imdb_num_votes),
                                   xLab = "IMDB Num Votes")

  imdb_num_votes_log <- bivariateQuant(data.frame(y = data$audience_score,
                                                  x = data$imdb_num_votes_log),
                                       xLab = "IMDB Num Votes (Log)")

  critics_score <- bivariateQuant(data.frame(y = data$audience_score,
                                             x = data$critics_score),
                                  xLab = "Critics Score")

  # Return analysis
  analysis <- list(
    feature_film = feature_film,
    drama = drama,
    mpaa_rating_R = mpaa_rating_R,
    oscar_season = oscar_season,
    summer_season = summer_season,
    best_pic_nom = best_pic_nom,
    best_pic_win = best_pic_win,
    best_dir_win = best_dir_win,
    best_actor_win = best_actor_win,
    best_actress_win = best_actress_win,
    top200_box = top200_box,
    thtr_rel_year = thtr_rel_year,
    runtime = runtime,
    imdb_rating = imdb_rating,
    imdb_num_votes = imdb_num_votes,
    imdb_num_votes_log = imdb_num_votes_log,
    critics_score = critics_score
  )
  return(analysis)
}
