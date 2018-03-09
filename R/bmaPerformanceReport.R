#==============================================================================#
#                           BMA Compare Report                                 #
#==============================================================================#
#' bmaPerformanceReport
#'
#' \code{bmaPerformanceReport} Renders tables and plots for BMA Compare function
#'
#' @param performance List containing performance data from \code{bmaPerformance}
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaPerformanceReport <- function(performance) {
  
  p <- list()
  p$plots <- list()
  p$plots$line <- list()
  p$plots$box <- list()
  
  # Reshape results
  p$dfl <- performance$MSE
  p$dfw <- dcast(performance$MSE, PriorDesc ~ Estimator, mean, value.var = 'MSE')
  p$dfs <- performance$MSE %>%  group_by(PriorDesc, Estimator) %>%
    summarize(`Average MSE` = mean(MSE)) %>% 
    select(PriorDesc, Estimator, `Average MSE`) %>% arrange(`Average MSE`)
  names(p$dfs) <- c("Prior", "Estimator", 'Average MSE')
  
  # Compute distribution of MSE by prior and model
  p$dist <- p$dfl %>% group_by(Prior, Estimator) %>% summarise(Mean = mean(MSE),
                                                        SD = sd(MSE),
                                                        N = n()) %>%
    mutate(SE = SD / sqrt(N),
           `2.5%` = Mean - qt(1 - (0.05 / 2), N - 1) * SE,
           `97.5%` = Mean + qt(1 - (0.05 / 2), N - 1) * SE) %>%
    select(Prior, Estimator, Mean, `2.5%`, `97.5%`)
    
  # Conduct t-tests of difference in mean MSE
  mu <- min(p$dist$Mean)
  p$ttests <- p$dfl %>% select(Prior, PriorDesc, Estimator, MSE) %>%
    group_by(Prior, PriorDesc, Estimator) %>%
    summarize(`Mean MSE` = mean(MSE),
              pValue = t.test(MSE, data = .,
                   mu = mu, 
                   paired = FALSE, 
                   conf.level = 0.95)$p.value)
  p$best <- p$ttests %>% arrange(desc(pValue))
  
  # Format mean MSE table
  p$table <- p$dfw
  p$table$BMA <- kableExtra::cell_spec(x = round(p$dfw$BMA, 3), format = "html",
                                           color =  ifelse(p$dfw$BMA == mu, 
                                                           "red", "black"),
                                           bold =  ifelse(p$dfw$BMA %in% p$best$`Mean MSE`[1:5], 
                                                          TRUE, FALSE))
  p$table$BPM <- kableExtra::cell_spec(x = round(p$dfw$BPM,3), format = "html",
                                       color =  ifelse(p$dfw$BPM == mu, 
                                                       "red", "black"),
                                       bold =  ifelse(p$dfw$BPM %in% p$best$`Mean MSE`[1:5], 
                                                      TRUE, FALSE))
  p$table$HPM <- kableExtra::cell_spec(x = round(p$dfw$HPM, 3), format = "html",
                                       color =  ifelse(p$dfw$HPM == mu, 
                                                       "red", "black"),
                                       bold =  ifelse(p$dfw$HPM %in% p$best$`Mean MSE`[1:5], 
                                                      TRUE, FALSE))
  p$table$MPM <- kableExtra::cell_spec(x = round(p$dfw$MPM, 3), format = "html",
                                       color =  ifelse(p$dfw$MPM == mu, 
                                                       "red", "black"),
                                       bold =  ifelse(p$dfw$MPM %in% p$best$`Mean MSE`[1:5], 
                                                      TRUE, FALSE))
  colnames(p$table) <- c("Prior", "BMA", "BPM", "HPM", "MPM")
  
  #   # Line plots
  mTypes <- unique(p$dfl$Estimator)
  p$plots$line <- lapply(mTypes, function(m) {
    data <- p$dfl %>% filter(Estimator == m) %>% select(Trial, MSE, Prior)
    plotLine(data = data, xticks = TRUE, 
             xLab = paste0("N=", max(unique(p$dfl$Trial)), " Trials"), 
             yLab = "Squared Error", 
             yLow = min(data$MSE), yHigh = max(data$MSE),
             plotTitle = paste0("Predictive Performance by Prior (",m,")"))
  })
  names(p$plots$line) <- mTypes

  # Box plots for top models
  best <- p$best[c(1:4),] %>% mutate(Model = paste0(Prior, " (", Estimator, ")"))
  all <- p$dfl %>% mutate(Model = paste0(Prior, " (", Estimator, ")"))
  pd <- merge(best, all, by = "Model") %>% select(MSE, Model)
  p$plots$box <- plotBox(data = pd, xLab = "Model", 
                         yLab = "Squared Error",
                         plotTitle = "MSE Distributions (n = 400 trials)",
                         rotate = TRUE,
                         showMean = TRUE)

  return(p)
}
