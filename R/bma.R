#==============================================================================#
#                     Bayes Model Averaging Fit                                #
#==============================================================================#
#' bma
#'
#' \code{bma} Performs Bayes Model Averaging using several default priors
#'
#' @param yX Data frame containing the vector y and matrix X of parameters.
#' @return list of BMA models
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bma <- function(yX) {

  #---------------------------------------------------------------------------#
  #                           Model Averaging                                 #
  #---------------------------------------------------------------------------#
  n = nrow(yX)

  models <- list()

  models[["BIC"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                   runtime_trans + mpaa_rating_R + thtr_rel_year +
                                   oscar_season + summer_season + imdb_rating_trans +
                                   critics_score_trans + best_pic_nom + best_pic_win + 
                                   best_actor_win + best_actress_win + 
                                   best_dir_win + top200_box + imdb_num_votes_trans,
                                 data = yX, prior = "BIC",
                                 modelprior = uniform(), method = "BAS")
  models[["BIC"]]$priorDesc <- 'Bayesian Information Criteria (BIC)'
  
  
  models[["AIC"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                   runtime_trans + mpaa_rating_R + thtr_rel_year +
                                   oscar_season + summer_season + imdb_rating_trans +
                                   critics_score_trans + best_pic_nom + best_pic_win + 
                                   best_actor_win + best_actress_win + 
                                   best_dir_win + top200_box + imdb_num_votes_trans,
                                 data = yX, prior = "AIC",
                                 modelprior = uniform(), method = "BAS")
  models[["AIC"]]$priorDesc <- 'Akaike Information Criterion (AIC)'
  
  
  
  models[["EB-global"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                    runtime_trans + mpaa_rating_R + thtr_rel_year +
                                    oscar_season + summer_season + imdb_rating_trans +
                                    critics_score_trans + best_pic_nom + best_pic_win + 
                                    best_actor_win + best_actress_win + 
                                    best_dir_win + top200_box + imdb_num_votes_trans,
                                  data = yX, prior = "EB-global", initprobs = "eplogp",
                                  modelprior = uniform(), method = "BAS")
  models[["EB-global"]]$priorDesc <- 'Empirical Bayes (Global)'
  
  
  models[["EB-local"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                    runtime_trans + mpaa_rating_R + thtr_rel_year +
                                    oscar_season + summer_season + imdb_rating_trans +
                                    critics_score_trans + best_pic_nom + best_pic_win + 
                                    best_actor_win + best_actress_win + 
                                    best_dir_win + top200_box + imdb_num_votes_trans,
                                  data = yX, prior = "EB-local", initprobs = "eplogp",
                                  modelprior = uniform(), method = "BAS")
  models[["EB-local"]]$priorDesc <- 'Empirical Bayes (Local)'
  
  models[["g-prior"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                       runtime_trans + mpaa_rating_R + thtr_rel_year +
                                       oscar_season + summer_season + imdb_rating_trans +
                                       critics_score_trans + best_pic_nom + best_pic_win + 
                                       best_actor_win + best_actress_win + 
                                       best_dir_win + top200_box + imdb_num_votes_trans,
                               data = yX, prior = "g-prior", alpha = 13,
                               modelprior = uniform(), method = "BAS")
  models[["g-prior"]]$priorDesc <- "Zellner's g-prior"
  
  
  models[["hyper-g"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                  runtime_trans + mpaa_rating_R + thtr_rel_year +
                                  oscar_season + summer_season + imdb_rating_trans +
                                  critics_score_trans + best_pic_nom + best_pic_win + 
                                  best_actor_win + best_actress_win + 
                                  best_dir_win + top200_box + imdb_num_votes_trans,
                                data = yX, prior = "hyper-g",alpha = 3,
                                modelprior = uniform(), method = "BAS")
  models[["hyper-g"]]$priorDesc <- 'Hyper-g'
  
  
  models[["hyper-g-laplace"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                    runtime_trans + mpaa_rating_R + thtr_rel_year +
                                    oscar_season + summer_season + imdb_rating_trans +
                                    critics_score_trans + best_pic_nom + best_pic_win + 
                                    best_actor_win + best_actress_win + 
                                    best_dir_win + top200_box + imdb_num_votes_trans,
                                  data = yX, prior = "hyper-g-laplace",
                                  modelprior = uniform(), method = "BAS")
  models[["hyper-g-laplace"]]$priorDesc <- 'Hyper-g Laplace'
  
  
  models[["hyper-g-n"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                    runtime_trans + mpaa_rating_R + thtr_rel_year +
                                    oscar_season + summer_season + imdb_rating_trans +
                                    critics_score_trans + best_pic_nom + best_pic_win + 
                                    best_actor_win + best_actress_win + 
                                    best_dir_win + top200_box + imdb_num_votes_trans,
                                  data = yX, prior = "hyper-g-n",
                                  modelprior = uniform(), method = "BAS")
  
  models[["hyper-g-n"]]$priorDesc <- 'Hyper-g-n'
  
  # models[["jzs"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
  # runtime + mpaa_rating_R + thtr_rel_year +
  #   oscar_season + summer_season + imdb_rating +
  #   critics_score +
  #   best_pic_nom + best_pic_win + best_actor_win +
  #   best_actress_win + best_dir_win + top200_box +
  #   imdb_num_votes_log, data = yX, prior = "JZS",
  #                        alpha = 1, modelprior = uniform(), method = "BAS")
  # models[["jzs"]]$priorDesc <- 'Jeffreys-Zellner-Siow'
  
  
  models[["ZS-null"]] <- BAS::bas.lm(audience_score ~ feature_film + drama +
                                  runtime_trans + mpaa_rating_R + thtr_rel_year +
                                  oscar_season + summer_season + imdb_rating_trans +
                                  critics_score_trans + best_pic_nom + best_pic_win + 
                                  best_actor_win + best_actress_win + 
                                  best_dir_win + top200_box + imdb_num_votes_trans,
                                data = yX, alpha = n, prior = "ZS-null", 
                                modelprior = uniform(), method = "BAS")
  models[["ZS-null"]]$priorDesc <- 'Zellner-Siow (NULL)'

  return(models)
}
