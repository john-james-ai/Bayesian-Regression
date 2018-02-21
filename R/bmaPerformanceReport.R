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
  p$dfl <- performance
  p$dfw <- dcast(performance, PriorDesc ~ id, mean, value.var = 'mse')
  
  # Compute distribution of MSE by prior and model
  p$dist <- p$dfl %>% group_by(Prior, id) %>% summarise(Mean = mean(mse),
                                                        SD = sd(mse),
                                                        N = n()) %>%
    mutate(SE = SD / sqrt(N),
           `2.5%` = Mean - qt(1 - (0.05 / 2), N - 1) * SE,
           `97.5%` = Mean + qt(1 - (0.05 / 2), N - 1) * SE) %>%
    select(Prior, id, Mean, `2.5%`, `97.5%`)
  
  # Conduct t-tests of difference in mean MSE
  mu <- min(p$dist$Mean)
  p$ttests <- p$dfl %>% select(Prior, PriorDesc, id, mse) %>%
    group_by(Prior, PriorDesc, id) %>%
    summarize(`Mean MSE` = mean(mse),
              pValue = t.test(mse, data = .,
                   mu = mu, 
                   paired = FALSE, 
                   conf.level = 0.99)$p.value)
  p$best <- head(p$ttests %>% filter(pValue > .05) %>% arrange(desc(pValue)))
  
  # Format mean MSE table
  p$table <- p$dfw
  p$table$BMA <- kableExtra::cell_spec(x = round(p$dfw$BMA, 3), format = "html",
                                           color =  ifelse(p$dfw$BMA == mu, 
                                                           "red", "black"),
                                           bold =  ifelse(p$dfw$BMA %in% p$best$`Mean MSE`, 
                                                          TRUE, FALSE))
  p$table$BPM <- kableExtra::cell_spec(x = round(p$dfw$BPM,3), format = "html",
                                       color =  ifelse(p$dfw$BPM == mu, 
                                                       "red", "black"),
                                       bold =  ifelse(p$dfw$BPM %in% p$best$`Mean MSE`, 
                                                      TRUE, FALSE))
  p$table$HPM <- kableExtra::cell_spec(x = round(p$dfw$HPM, 3), format = "html",
                                       color =  ifelse(p$dfw$HPM == mu, 
                                                       "red", "black"),
                                       bold =  ifelse(p$dfw$HPM %in% p$best$`Mean MSE`, 
                                                      TRUE, FALSE))
  p$table$MPM <- kableExtra::cell_spec(x = round(p$dfw$MPM, 3), format = "html",
                                       color =  ifelse(p$dfw$MPM == mu, 
                                                       "red", "black"),
                                       bold =  ifelse(p$dfw$MPM %in% p$best$`Mean MSE`, 
                                                      TRUE, FALSE))
  colnames(p$table) <- c("Prior", "BMA", "BPM", "HPM", "MPM")
  
  #   # Line plots
  mTypes <- unique(p$dfl$id)
  p$plots$line <- lapply(mTypes, function(m) {
    data <- p$dfl %>% filter(id == m) %>% select(Trial, mse, Prior)
    plotLine(data = data, xticks = TRUE, 
             xLab = paste0("N=", max(unique(p$dfl$Trial)), " Trials"), 
             yLab = "Squared Error", 
             yLow = min(data$mse), yHigh = max(data$mse),
             plotTitle = paste0("Predictive Performance by Prior (",m,")"))
  })
  names(p$plots$line) <- mTypes

  # Box plots for top models
  pd <- p$dfl %>% 
    mutate(Model = paste0(p$best$Prior, " (", p$dfl$id, ")")) %>% 
    filter(Prior %in% p$best$Prior & id %in% p$best$id) %>% 
    select(mse, Model)
  p$plots$box <- plotBox(data = pd, xLab = "Model", 
                         yLab = "Squared Error",
                         plotTitle = "MSE Distributions",
                         rotate = TRUE,
                         showMean = TRUE)

  return(p)
}
