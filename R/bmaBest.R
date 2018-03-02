#==============================================================================#
#                     Bayes Model Averaging Fit for Best Models                #
#==============================================================================#
#' bmaBest
#'
#' \code{bmaBest} Performs Bayes Model Averaging for select priors
#'
#' @param yX Data frame containing the vector y and matrix X of parameters.
#' @return list of BMA models
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaBest <- function(yX) {

  #---------------------------------------------------------------------------#
  #                           Model Averaging                                 #
  #---------------------------------------------------------------------------#
  n = nrow(yX)

  models <- list()

  
  
  models[["AIC"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                   runtime + mpaa_rating_R + thtr_rel_year +
                                   oscar_season + summer_season + imdb_rating +
                                   critics_score + best_pic_nom + best_pic_win + 
                                   best_actor_win + best_actress_win + 
                                   best_dir_win + top200_box + imdb_num_votes,
                                 data = yX, prior = "AIC",
                                 modelprior = uniform(), method = "BAS")
  models[["AIC"]]$priorDesc <- 'Akaike Information Criterion (AIC)'
  
  
  
  models[["EB-global"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                    runtime + mpaa_rating_R + thtr_rel_year +
                                    oscar_season + summer_season + imdb_rating +
                                    critics_score + best_pic_nom + best_pic_win + 
                                    best_actor_win + best_actress_win + 
                                    best_dir_win + top200_box + imdb_num_votes,
                                  data = yX, prior = "EB-global", initprobs = "eplogp",
                                  modelprior = uniform(), method = "BAS")
  models[["EB-global"]]$priorDesc <- 'Empirical Bayes (Global)'
  
  
  
  models[["g-prior"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                       runtime + mpaa_rating_R + thtr_rel_year +
                                       oscar_season + summer_season + imdb_rating +
                                       critics_score + best_pic_nom + best_pic_win + 
                                       best_actor_win + best_actress_win + 
                                       best_dir_win + top200_box + imdb_num_votes,
                               data = yX, prior = "g-prior", alpha = 13,
                               modelprior = uniform(), method = "BAS")
  models[["g-prior"]]$priorDesc <- "Zellner's g-prior"
  
  
  
  models[["hyper-g-laplace"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                    runtime + mpaa_rating_R + thtr_rel_year +
                                    oscar_season + summer_season + imdb_rating +
                                    critics_score + best_pic_nom + best_pic_win + 
                                    best_actor_win + best_actress_win + 
                                    best_dir_win + top200_box + imdb_num_votes,
                                  data = yX, prior = "hyper-g-laplace",
                                  modelprior = uniform(), method = "BAS")
  models[["hyper-g-laplace"]]$priorDesc <- 'Hyper-g Laplace'
  

  return(models)
}
