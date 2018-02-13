#==============================================================================#
#                                 preprocess                                   #
#==============================================================================#
#' preprocess
#'
#' \code{preprocess} Performs preprocessing of data for analysis
#'
#' Adds the following variables to the data data set
#' \itemize{
#'  \item feature_film: "yes" if "title_type" is "Feature Film", "no" otherwise
#'  \item drama: "yes" if "genre" is "Drama", "no" otherwise
#'  \item mpaa_rating_R: "yes" if "mpaa_rating" is "R", "no" otherwise
#'  \item oscar_season: "yes" if thtr_rel_month is 10, 11, or 12, "no" otherwise
#'  \item summer_season: "yes" if thtr_rel_month is 5,6,7, or 8, "no" otherwise
#' }
#'
#' @param data frame containing the data data set
#' @return mdbSets movie data base training and test set
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family data functions
#' @export
preprocess <- function(data) {

  # Add additional features
  data <- data[data$title_type != "TV Movie",]
  data$feature_film <- ifelse(data$title_type == "Feature Film", "yes", "no")
  data$drama <- ifelse(data$genre == "Drama", "yes", "no")
  data$mpaa_rating_R <- ifelse(data$mpaa_rating == "R", "yes", "no")
  data$oscar_season <- ifelse(data$thtr_rel_month %in% c(10,11,12), "yes", "no")
  data$summer_season <- ifelse(data$thtr_rel_month %in% c(5,6,7,8), "yes", "no")

  # Extract required features
  data <- data %>% select(title, feature_film, drama, runtime, mpaa_rating_R,
                              thtr_rel_year, oscar_season, summer_season,
                              imdb_rating, imdb_num_votes, critics_score,
                              best_pic_nom, best_pic_win, best_actor_win,
                              best_actress_win, best_dir_win, top200_box, audience_score)

  # Transform number of IMDB votes
  data$imdb_num_votes_log <- log2(data$imdb_num_votes)
  data <- data[complete.cases(data),]

  data <- na.omit(data)
  data <- data %>% mutate_if(is.factor, as.character)


  return(data)
}
