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

  #---------------------------------------------------------------------------#
  #                       Conduct Qualitative Analysis                        #
  #---------------------------------------------------------------------------#
  feature_film <- univariateQual(as.data.frame(data$feature_film), xLab = "Feature Film")
  drama <- univariateQual(as.data.frame(data$drama), xLab = "Drama")
  mpaa_rating_R <- univariateQual(as.data.frame(data$mpaa_rating_R), xLab = "MPAA R Rating")
  oscar_season <- univariateQual(as.data.frame(data$oscar_season), xLab = "Oscar Season Release")
  summer_season <- univariateQual(as.data.frame(data$summer_season), xLab = "Summer Season Release")
  best_pic_nom <- univariateQual(as.data.frame(data$best_pic_nom), xLab = "Best Picture Oscar Nomination")
  best_pic_win <- univariateQual(as.data.frame(data$best_pic_win), xLab = "Best Picture Oscar")
  best_dir_win <- univariateQual(as.data.frame(data$best_dir_win), xLab = "Best Director Oscar")
  best_actor_win <- univariateQual(as.data.frame(data$best_actor_win), xLab = "Best Actor Oscar")
  best_actress_win <- univariateQual(as.data.frame(data$best_actress_win), xLab = "Best Actress Oscar")
  top200_box <- univariateQual(as.data.frame(data$top200_box), xLab = "Top 200 Box Office")
  thtr_rel_year <- univariateQual(as.data.frame(as.character(data$thtr_rel_year)),
                                  yLab = "Films",
                                  xLab = "Year of Theatrical Release")

  #---------------------------------------------------------------------------#
  #                        Conduct Quantative Analysis                        #
  #---------------------------------------------------------------------------#
  runtime <- univariateQuant(data.frame(data$title, data$runtime),
                             yLab = "Runtime", units = "minutes")
  imdb_rating <- univariateQuant(data.frame(data$title, data$imdb_rating),
                                 yLab = "IMDB Rating", units = "points")
  imdb_num_votes <- univariateQuant(data.frame(data$title, data$imdb_num_votes),
                               yLab = "IMDB Num Votes", units = "votes")
  imdb_num_votes_log <- univariateQuant(data.frame(data$title, data$imdb_num_votes_log),
                                    yLab = "IMDB Num Votes (Log)", units = "votes")
  critics_score <- univariateQuant(data.frame(data$title, data$critics_score),
                                   yLab = "Critics Score", units = "points")
  audience_score <- univariateQuant(data.frame(data$title, data$audience_score),
                             yLab = "Audience Score", units = "points")


  # Return analysis
  analysis <- list(
    best_actor_win = best_actor_win,
    best_actress_win = best_actress_win,
    best_dir_win = best_dir_win,
    best_pic_nom = best_pic_nom,
    best_pic_win = best_pic_win,
    drama = drama,
    feature_film = feature_film,
    mpaa_rating_R = mpaa_rating_R,
    oscar_season = oscar_season,
    summer_season = summer_season,
    top200_box = top200_box,
    thtr_rel_year = thtr_rel_year,
    audience_score = audience_score,
    critics_score = critics_score,
    imdb_num_votes = imdb_num_votes,
    imdb_num_votes_log = imdb_num_votes_log,
    imdb_rating = imdb_rating,
    runtime = runtime
  )
  return(analysis)
}
