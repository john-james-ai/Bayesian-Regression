#==============================================================================#
#                             Visualization Functions                          #
#==============================================================================#

#------------------------------------------------------------------------------#
#                     Frequency and Proportion Bar Plot                        #
#------------------------------------------------------------------------------#
#' plotFreqProp
#'
#' \code{plotFreqPropTbl} Renders a stacked bar plot and a contingency table
#' showing frequencies and proportions
#'
#' @param data Data frame containing the variables:
#' \itemize{
#'  \item Character string containing the values for each categorical level
#'  \item N: The number of observations for the categorical level
#'  \item Proportion: The proportion of observation for the categorical level
#'  \item Cumulative: The cumulative proportion
#'  \item Pos: The y position for the frequency and proportion text
#' }
#' @param yLab Capital case character string describing the y variable
#' @param xLab Capital case character string containing the name of the variable x variable
#' @param plotTitle Capital case character string for the title of the plot
#'
#' @return List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotFreqProp <- function(data, yLab = "Movies", xLab, plotTitle = NULL) {

  # Format title
  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  barPlot <- ggplot2::ggplot(data = df,
                             ggplot2::aes(x = df[[1]],
                                          y = df[[2]],
                                          fill = df[[1]]))  +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(x = df[[1]],
                   y = df[[5]],
                   label = paste0(df[[2]], " (",
                                  round(df[[3]] * 100, 0),
                                  "%)")),
      colour="black", family="Tahoma", size = 8) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.position = "right") +
    ggplot2::scale_fill_manual(values = myPal(length(df[[1]]))) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::ylab(yLab) +
    ggplot2::labs(fill = xLab)

    visual <- list(
      stats = df[,1:4],
      plot = barPlot
    )
    return(visual)
}


#------------------------------------------------------------------------------#
#                               Plot Histogram                                 #
#------------------------------------------------------------------------------#
#' plotHist
#'
#' \code{plotHist} Renders a histogram with a normal curve as well as a table
#' of summary statistics.
#'
#' @param data Data frame containing a single numeric variable
#' @param xLab Capital case character string the group or subset of data being printed (optional)
#' @param yLab Capital case character string describing y is being printed
#' @param plotTitle Capital case character string for the plotTitle of the plot
#'
#' @return List containing a summary statistics and a histogram
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotHist <- function(data, xLab = NULL, yLab, plotTitle = NULL) {

  if (is.null(plotTitle)) {
    if (is.null(xLab)) {
      plotTitle <- yLab
    } else {
      plotTitle <- paste(yLab, "by", xLab)
    }
  }

  hist <- ggplot2::ggplot(data = data,
                        ggplot2::aes(x = data[[1]])) +
    ggplot2::geom_histogram(position ='identity', color = "white", fill = "palegreen4", bins = 30) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::ylab(yLab) +
    ggplot2::scale_x_continuous(labels = scales::comma)

  return(hist)
}

#------------------------------------------------------------------------------#
#                               Plot Quantile                                  #
#------------------------------------------------------------------------------#
#' plotQQ
#'
#' \code{plotQQ} Renders a histogram with a normal curve as well as a table
#' of summary statistics.
#'
#' @param data Data frame or vector containing a single numeric variable
#' @param xLab Capital case character string containing the name of the grouping or subset variable (optional)
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Capital case character string for the title of the plot
#'
#' @return List containing a summary statistics and QQ plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotQQ <- function(data, xLab = NULL, yLab, plotTitle = NULL) {

  if (is.null(plotTitle)) {
    if (is.null(xLab)) {
      plotTitle <- paste("Normal Q-Q Plot:", yLab)
    } else {
      plotTitle <- paste("Normal Q-Q Plot:", yLab, "by", xLab)
    }
  }


  # Render QQ Plot
  qq <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(sample = data[[1]])) +
    qqplotr::stat_qq_band() +
    qqplotr::stat_qq_line() +
    qqplotr::stat_qq_point() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggplot2::ggtitle(plotTitle)

  return(qq)
}

#------------------------------------------------------------------------------#
#                                 Boxplot                                      #
#------------------------------------------------------------------------------#
#' plotBox
#'
#' \code{plotBox} Renders a single or grouped box plot.
#'
#' @param data Data frame or vector containing two columns:
#'          1: Numeric response variable (y)
#'          2: Categorical independent variable (x)
#' @param xLab Capital case character string containing the name of the x variable (optional)
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Character case character string containing the title for the plot
#' @param rotate Logical indicating whether to rotate the plot by 90 degrees
#' @param showMean Logical indicating whether the mean value should be displayed.
#'
#' @return List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotBox <- function(data, xLab = NULL, yLab, plotTitle = NULL, rotate = FALSE, showMean = TRUE) {

  if (length(data) > 2) stop(paste("Error in plotBox: Dimension of data frame must be 1 or 2, not", length(data)))
  if (length(data) == 1) {
    data <- data.frame(x = rep("x", nrow(data)),
                       y = data[[1]], row.names = NULL)
  } else {
    data <- data.frame(y = data[[1]],
                       x = data[[2]],
                       row.names = NULL)
  }

  # Format title
  if (is.null(plotTitle)) {
   if (is.null(xLab)) {
     plotTitle <- yLab
   } else {
     plotTitle <- paste(yLab, "by", xLab)
   }
  }

  # Calculate means
  means <- function(x){
    return(data.frame(y=mean(x),label=round(mean(x,na.rm=T), 1)))
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))


  bp <- ggplot2::ggplot(data = data,
                             ggplot2::aes(x = reorder(x, -y, mean),
                                          y = data$y,
                                          fill = data$x))  +
    ggplot2::geom_boxplot(outlier.colour = "black") +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   legend.position = "none") +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::scale_fill_manual(values = myPal(length(unique(data$x))))
  
  if (showMean == TRUE) {
    bp <- bp + ggplot2::stat_summary(fun.y = mean, colour = "black", 
                                     geom = "point", shape = 18, 
                                     size = 5, show.legend = FALSE) + 
      ggplot2::stat_summary(fun.data = means, geom = "text", 
                            colour = "black", vjust = -0.0,
                            hjust = -1)
  }

  if (is.null(xLab)) {
    bp <- bp + ggplot2::coord_flip() +
      ggplot2::theme(legend.position = "none",
                     axis.text.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
  } else {
    if (rotate == TRUE) {
      bp <- bp + ggplot2::coord_flip() +
        ggplot2::labs(y = yLab, x = xLab, fill = xLab)
    } else {
      bp <- bp + ggplot2::labs(y = yLab, x = xLab, fill = xLab)
    }
  }
  

  return(bp)
}


#------------------------------------------------------------------------------#
#                               Plot Scatterplot                               #
#------------------------------------------------------------------------------#
#' plotScatter
#'
#' \code{plotScatter} Renders a scatterplot for two numerical variablesa
#'
#' @param data Data frame containing the quantitative variables
#' @param xLab Capital case character string containing the name of the grouping or subset variable
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Capital case character string for title of plot
#'
#' @return Scatterplot object
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotScatter <- function(data, xLab, yLab, plotTitle = NULL) {

  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }

  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

  scatter <- ggplot2::ggplot(data = data,
                             ggplot2::aes(y = as.numeric(unlist(data[,1])),
                                          x = as.numeric(unlist(data[,2])))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::geom_smooth(method = lm, se = FALSE) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   legend.position = "right") +
    ggplot2::labs(x = xLab, y = yLab) +
    ggplot2::ggtitle(plotTitle)

  return(scatter)

}

#------------------------------------------------------------------------------#
#                             Plot Residual QQ                                 #
#------------------------------------------------------------------------------#
#' plotResQQ
#'
#' \code{plotResQQ} Plots regression diagnostics
#'
#' @param res Vector of residual values
#' @param mName Capital case character string for the model name
#'
#' @return resQQ Residual Normal QQ Plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotResQQ <- function(res, mName) {

  # Obtain diagnostics and render plot
  resQQ <- ggplot2::ggplot(data = res, ggplot2::aes(sample = res)) +
    ggplot2::stat_qq() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
    ggplot2::ggtitle(paste0(mName, ": Normal-QQ Plot"))

  return(resQQ)
}

#------------------------------------------------------------------------------#
#                           Plot Residual vs. Fit                              #
#------------------------------------------------------------------------------#
#' plotResFit
#'
#' \code{plotResFit} Plots regression diagnostics
#'
#' @param x Linear model object or a data frame containing the residuals for each observation
#' @param model Capital case name of the model
#'
#' @return resFit Residual vs. Fit Plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotResFit <- function(x, model) {

  if ("data.frame" %in% class(x)) {
    resFit <- plotScatter(x, xLab = "Predicted", yLab = "Residual",
                          plotTitle = paste("Residual vs. Predicted:", model))
  } else {
    # Obtain diagnostics and render plot
    resFit <- lindia::gg_diagnose(mod, theme = ggplot2::theme_minimal(), plot.all = FALSE)
    resFit <- resFit$res_fitted +
      ggplot2::theme_minimal(base_size = 16) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
      ggplot2::ggtitle(paste("Residuals vs. Fit:", yLab))
  }
  return(resFit)
}

#------------------------------------------------------------------------------#
#                            Plot Correlation                                  #
#------------------------------------------------------------------------------#
#' plotCorr
#'
#' \code{plotCorr} Creates correlation plot
#'
#' @param data Data frame containing data for which the correlations will be plotted.
#'
#' @return Correlation plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotCorr <- function(data, yVar) {

  p <-corrplot::corrplot(cor(data), diag = FALSE,
                     order = "hclust", number.cex = 1,
                     addCoef.col = "black", tl.col = "black",
                     tl.srt = 90, tl.pos = "td", tl.cex = 1,
                     method = "color", type = "upper",
                     col = RColorBrewer::brewer.pal(n = 11,
                                                    name = "PiYG"))
  return(p)
}


#------------------------------------------------------------------------------#
#                                 Line Plot                                    #
#------------------------------------------------------------------------------#
#' plotLine
#'
#' \code{plotLine} Renders a line plot
#'
#' @param data Data frame containing three variables: (1) x data, (2), y data, and (3) 
#' the grouping variable.
#' @param xticks Logical indicating whether xticks should be rendered.
#' @param xLab Capital case character string containing the name of the x variable (optional)
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Character case character string containing the title for the plot
#'
#' @return List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotLine <- function(data, xticks = TRUE, xLab, yLab, yLow = NULL, yHigh = NULL, 
                     plotTitle = NULL) {

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

  lp <- ggplot2::ggplot(data = data,
                        ggplot2::aes(x = data[[1]],
                                     y = data[[2]], 
                                     group = data[[3]], color = data[[3]]))  +
    ggplot2::geom_line() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   legend.position = "right") +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::labs(y = yLab, x = xLab, color = names(data[3])) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_y_continuous()
  
  if (!is.null(yLow) & !is.null(yHigh)) {
    lp <- lp + ggplot2::scale_y_continuous(limits = c(yLow, yHigh))
  }
  if (xticks == FALSE) {
    lp <- lp + ggplot2::theme(axis.text.x = ggplot2::element_blank())
  }

  return(lp)
}


#------------------------------------------------------------------------------#
#                                Bar Plot                                      #
#------------------------------------------------------------------------------#
#' plotBar
#'
#' \code{plotBar} Renders a bar plot with bars sequenced by value left to right.
#'
#' @param data Data frame or vector containing a single categorical factor variable
#' @param yLab Capital case character string describing the y variable
#' @param xLab Capital case character string containing the name of the variable x variable
#' @param plotTitle Capital case character string for the title of the plot
#'
#' @return Bar plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotBar <- function(data, yLab, xLab, plotTitle = NULL) {

  # Format title
  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  barPlot <- ggplot2::ggplot(data = data,
                             ggplot2::aes(x = reorder(data[[1]], -data[[2]]),
                                          y = data[[2]],
                                          fill = data[[1]]))  +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::scale_fill_manual(values = myPal(length(data[[1]]))) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::ylab(yLab) +
    ggplot2::labs(fill = xLab) +
    ggplot2::scale_y_continuous(labels = scales::comma)


  return(barPlot)
}

#------------------------------------------------------------------------------#
#                             Grouped Bar Plot                                 #
#------------------------------------------------------------------------------#
#' groupBarPlot
#'
#' \code{groupBarPlot} Renders a grouped bar plot with confidence interval bars.
#'
#' @param data Data frame containing 4 columns: (1) The group parameter; (2)
#' the subgoup variable; and (3), the mean value, and (4), the length
#' of the confidence interval.
#' @param plotTitle Capital case character string for the title of the plot
#' @param
#'
#' @return Grouped bar plot with confidence / credible interval lines.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
groupBarPlot <- function(data, plotTitle = NULL, values = FALSE) {

  # Format title
  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  barPlot <- ggplot2::ggplot(data = data,
                             ggplot2::aes(x = Parameter,
                                          y = Mean,
                                          fill = Prior))  +
    ggplot2::geom_bar(position = position_dodge(),  stat='identity') +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-ci, ymax = mean+ci),
                           width = .2, position = position_dodge(.9)) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   legend.position = "none") +
    ggplot2::scale_fill_manual(values = myPal(length(data[[1]]))) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    ggplot2::scale_y_continuous(labels = scales::comma)

  if (values == TRUE) {
    barPlot <- barPlot + ggplot2::geom_text(aes(label=round(data[[2]], 3)),
                                            family="Open Sans",
                                            vjust=-.2, color="black",
                                            size=5)
  }


  return(barPlot)
}


#------------------------------------------------------------------------------#
#                                Bar Plot2                                     #
#------------------------------------------------------------------------------#
#' plotBar2
#'
#' \code{plotBar2} Renders a bar plot
#'
#' @param data Data frame or vector containing a single categorical factor variable
#' @param yLab Capital case character string describing the y variable
#' @param xLab Capital case character string containing the name of the variable x variable
#' @param plotTitle Capital case character string for the title of the plot
#' @param
#'
#' @return Bar plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotBar2 <- function(data, yLab, xLab, plotTitle = NULL, values = FALSE) {

  # Format title
  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  barPlot <- ggplot2::ggplot(data = data,
                             ggplot2::aes(x = data[[1]],
                                          y = data[[2]],
                                          fill = data[[1]]))  +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   legend.position = "none") +
    ggplot2::scale_fill_manual(values = myPal(length(data[[1]]))) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::ylab(yLab) +
    ggplot2::labs(fill = xLab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  ggplot2::scale_y_continuous(labels = scales::comma)

  if (values == TRUE) {
    barPlot <- barPlot + ggplot2::geom_text(aes(label=round(data[[2]], 3)),
                                            family="Open Sans",
                                            vjust=-.2, color="black",
                                            size=5)
  }


  return(barPlot)
}



#------------------------------------------------------------------------------#
#                                Pie Plot                                      #
#------------------------------------------------------------------------------#
#' pieChart
#'
#' \code{pieChart} Renders a pie plot
#'
#' @param data Data frame or vector containing a single categorical factor variable
#' @param yLab Capital case character string describing the y variable
#' @param xLab Capital case character string containing the name of the variable x variable
#' @param plotTitle Capital case character string for the title of the plot
#'
#' @return Bar plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
pieChart <- function(data, xLab, plotTitle = NULL) {

  # Format title
  if (is.null(plotTitle)) {
    plotTitle <- xLab
  }

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

  pie <- ggplot2::ggplot(data = data,
                         ggplot2::aes(x = "",
                                      y = data[[2]],
                                      fill = data[[1]]))  +
    ggplot2::geom_bar(width = 1, stat = "identity") +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.line = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust=0.5)) +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::labs(fill = xLab,
                  x = NULL,
                  y = NULL) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::coord_polar(theta = "y", start=0) +
    ggplot2::scale_fill_manual(values = myPal(length(data[[1]]))) +
    ggplot2::geom_text(ggplot2::aes(y = data[[2]]/nrow(data) + c(0, cumsum(data[[2]][-length(data[[2]])])),
                                    label = paste0(data[[2]], "(",scales::percent(data[[3]]),")")),
                       colour = "white", size = 6)
  return(pie)
}


#------------------------------------------------------------------------------#
#                         Plot Confidence Inteval Bars                         #
#------------------------------------------------------------------------------#
#' plotCIBars
#'
#' \code{plotCIBars} Plots confidence interval bars
#'
#' @param data Data frame containing terms, means, and lower and upper bounds of confidence interval
#' @param plotTitle Capital case character string for the title of the plot
#'
#' @return confIntPlot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotCIBars <- function(data, plotTitle = NULL) {

  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

  confIntPlot <- ggplot2::ggplot(data = data,
                                 ggplot2::aes(x = factor(data[[1]], levels = unique(data[[1]])), y = data[[2]],
                                                           fill = data[[1]])) +
    ggplot2::geom_bar(position = ggplot2::position_dodge(), stat = "identity") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = data[[3]], ymax = data[[4]]),
                                        width = .2, position = ggplot2::position_dodge(.9)) +
    ggplot2::theme_minimal(base_size = 28) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                   legend.position = "none") +
    ggplot2::geom_text(ggplot2::aes(label = round(data[[2]], 2)), 
                       family="Open Sans", size = 5,
                       vjust=-0.35) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::scale_fill_manual(values = myPal(length(data[[1]])))
  return(confIntPlot)
}

#------------------------------------------------------------------------------#
#                       Plot Two Lines with Two Y Axes                         #
#------------------------------------------------------------------------------#
#' plotLines2Y
#'
#' \code{plotLines2Y} Plots two lines on single plot with two Y axes.
#'
#' @param data Data frame containing three variables: (1) x data, (2), y1 data 
#' (left axis), and (3) y2 data (right axis)
#' @param xticks Logical indicating whether xticks should be rendered.
#' @param xLab Capital case character string containing the name of the x variable (optional)
#' @param yLabL Capital case character string containing the name of the y1 variable
#' @param yLabR Capital case character string containing the name of the y2 variable
#' @param plotTitle Character case character string containing the title for the plot
#'
#' @return List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotLines2Y <- function(data, xticks = TRUE, xLab, yLabL, yLabR, yLow = NULL, yHigh = NULL, 
                     plotTitle = NULL) {
  
  #TODO: Get working
  
  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  
  lp <- ggplot2::ggplot(data = data, ggplot2::aes(x = data[[1]]))  +
    ggplot2::geom_line(ggplot2::aes(y = data[[2]], colour = yLabL)) +
    ggplot2::geom_line(ggplot2::aes(y = data[[3]], colour = yLabR)) +
    ggplot2::scale_y_continuous(sec.axis = sec_axis(~., name = yLabR)) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   legend.position = "right") +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::scale_x_discrete()
  
  if (xticks == FALSE) {
    lp <- lp + ggplot2::theme(axis.text.x = ggplot2::element_blank())
  }
  
  return(lp)
}