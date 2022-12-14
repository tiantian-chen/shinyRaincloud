#ggplot2
library(ggplot2)
#组合图形以及绘制网格等
library(cowplot)
# 用于统计
library(ggpubr)
# 95%置信区间
library(Hmisc)
# Beeswarm dots 蜜蜂图
library(ggbeeswarm)

source(file = "source/halfViolinPlots.R")

createPlot <- function(input) {
  
  p <- 'ggplot(plotData, aes(x = condition, y = value, \\
fill = condition, color = condition)) + '
  
  p <- paste0(p, 'ggtitle("{input$plotTitle}") + \\
ylab("{input$yAxisTitle}") + \\
xlab("{input$xAxisTitle}") + \\
{input$plotTheme} + \\
scale_shape_identity() + ')
  
  if (input$plotLegend == FALSE) {
    p <- paste0(p, 'theme(legend.position = "none",
             plot.title = element_text(size = {input$titleFontSize},hjust = 0.5),
             axis.title = element_text(size = {input$axisLabelFontSize}),
             axis.text = element_text(size = {input$scaleFontSize}),
             axis.text.x = element_text(angle = {input$xAxisAngle}, 
                hjust = {input$xAxishjust},
                vjust = {input$xAxisvjust})) + ')
  } else {
    p <- paste0(p, 'theme(plot.title = element_text(size = {input$titleFontSize},hjust = 0.5),
             axis.title = element_text(size = {input$axisLabelFontSize}),
             axis.text = element_text(size = {input$scaleFontSize}),
             axis.text.x = element_text(angle = {input$xAxisAngle}, 
                hjust = {input$xAxishjust},
                vjust = {input$xAxisvjust})) + ')
  }
  
  if (input$plotMajorGrid == TRUE) {
    if (input$plotMinorGrid == TRUE) {
      p <- paste0(p, 'background_grid(major = "xy", minor = "xy") + ')
    } else {
      p <- paste0(p, 'background_grid(major = "xy", minor = "none") + ')
    }
  }
  
  if (input$plotPalette != "default") {
    if (input$plotPalette == "NPG") {
      p <- paste0(p, 'scale_color_npg() + scale_fill_npg() + ')
    } else if (input$plotPalette == "AAAS") {
      p <- paste0(p, 'scale_color_aaas() + scale_fill_aaas() + ')
    } else if (input$plotPalette == "NEJM") {
      p <- paste0(p, 'scale_color_nejm() + scale_fill_nejm() + ')
    } else if (input$plotPalette == "Lancet") {
      p <- paste0(p, 'scale_color_lancet() + scale_fill_lancet() + ')
    } else if (input$plotPalette == "JAMA") {
      p <- paste0(p, 'scale_color_jama() + scale_fill_jama() + ')
    } else if (input$plotPalette == "JCO") {
      p <- paste0(p, 'scale_color_jco() + scale_fill_jco() + ')
    } else if (input$plotPalette == "UCSCGB") {
      p <- paste0(p, 'scale_color_ucscgb() + scale_fill_ucscgb() + ')
    } else if (input$plotPalette == "LocusZoom") {
      p <- paste0(p, 'scale_color_locuszoom() + scale_fill_locuszoom() + ')
    } else if (input$plotPalette == "IGV") {
      p <- paste0(p, 'scale_color_igv() + scale_fill_igv() + ')
    } else if (input$plotPalette == "UChicago") {
      p <- paste0(p, 'scale_color_uchicago() + scale_fill_uchicago() + ')
    } else if (input$plotPalette == "UChicago Light") {
      p <- paste0(p, 'scale_color_uchicago("light") + scale_fill_uchicago("light") + ')
    } else if (input$plotPalette == "UChicago Dark") {
      p <- paste0(p, 'scale_color_uchicago("dark") + scale_fill_uchicago("dark") + ')
    } else if (input$plotPalette == "Star Trek") {
      p <- paste0(p, 'scale_color_startrek() + scale_fill_startrek() + ')
    } else if (input$plotPalette == "Tron Legacy") {
      p <- paste0(p, 'scale_color_tron() + scale_fill_tron() + ')
    } else if (input$plotPalette == "Futurama") {
      p <- paste0(p, 'scale_color_futurama() + scale_fill_futurama() + ')
    } else if (input$plotPalette == "Rick and Morty") {
      p <- paste0(p, 'scale_color_rickandmorty() + scale_fill_rickandmorty() + ')
    } else if (input$plotPalette == "The Simpsons") {
      p <- paste0(p, 'scale_color_simpsons() + scale_fill_simpsons() + ')
    } else {
      ## Color Brewer 颜色
      p <- paste0(p, 'scale_colour_brewer(palette = "{input$plotPalette}") \\
+ scale_fill_brewer(palette = "{input$plotPalette}") + ')
    }
  }
  
  ## Background Horizontal Line 水平线
  if (input$horizontalLine) {
    p <- paste0(p, 'geom_hline(yintercept = {input$horizontalLinePosition}, 
             linetype = "{input$horizontalLineType}", 
             size = {input$horizontalLineSize}, 
             alpha = {input$horizontalLineAlpha}) + ')
  }
  
  ## DOts 点图
  if(input$plotDots) {
    if (input$dotColumnType == 'jitterDots') {
      ## Jitter Dots 散点图
      p <- paste0(p, 'geom_point(position = position_jitter({input$dotsWidth}), 
             size = {input$dotSize}, 
             alpha = {input$dotAlpha}, 
             aes(shape = {input$dotShape})) + ')
    } else {
      ## Beeswarm 蜜蜂图
      p <- paste0(p, 'geom_beeswarm(size = {input$dotSize}, 
                alpha = {input$dotAlpha}, 
                aes(shape = {input$dotShape})) + ')
    }
  }
  
  ## Violin Plots 小提琴图
  if (input$plotViolins) {
    p <- paste0(p, {input$violinType}, '(position = position_nudge(x = {input$violinNudge}, y = 0),
             adjust = {input$violinAdjust},', ifelse (input$violinQuantiles && input$violinType == "geom_violin",'
             draw_quantiles = c(0.5),',''),'
             alpha = {input$violinAlpha}, 
             trim = {input$violinTrim}, 
             scale = "{input$violinScale}") + ')
  }
  
  ## BoxPlots 箱图
  if (input$boxPlots) {
    p <- paste0(p, 'geom_boxplot(aes(x = as.numeric(condition)+ {input$boxplotNudge}, y = value), 
             notch = {input$boxplotNotch}, 
             width = {input$boxplotWidth}, 
             varwidth = {input$boxplotBoxWidth}, 
             outlier.shape = ', ifelse(input$boxplotOutliers, 16, NA), ', 
             alpha = {input$boxplotAlpha}, 
             colour = "black", 
             show.legend = FALSE) + ')
  }
  ## Mean  平均值
  if (input$statsMean) {
    ## Just the mean
    p <- paste0(p, 'stat_summary(fun.ymin = mean, 
             fun.ymax = mean,
             geom = "errorbar",
             width = {input$statsMeanWidth},
             position = position_nudge(x = {input$statsMeanNudge}, y = 0),
             size = {input$statsMeanSize},
             color = "black") + ')
    
    ## Adding error bars to the mean 误差棒
    if (input$statsMeanErrorBars != 'none') {
      p <- paste0(p, 'stat_summary(fun.data = {input$statsMeanErrorBars},
             geom = "errorbar",
             width = {input$statsMeanWidth},
             position = position_nudge(x = {input$statsMeanNudge}, y = 0),
             size = {input$statsMeanSize},
             color = "black") + ')
    }
  }
  
  
  ## Significance 显著性分析
  if (input$statistics) {
    if (!is.null(input$statsCombinations)) {
      ## Get the Combinations for the pairwise comparisons as text
      ## Would be nice to clean and search for something more elegant.
      statsPairwiseTestsText <- getSelectedCombinations(input$statsCombinations)
      if (input$statsType == 'parametric' && input$statsTtest) {
        statsMethods <- 't.test'
      } else if (input$statsType == 'nonParametric' && input$statsWilcoxon) {
        statsMethods <- 'wilcox.test'
      }
      ## Double check to prevent 'statsMethods not found'.
      if(input$statsTtest || input$statsWilcoxon) {
        p <- paste0(p, 'stat_compare_means(aes(label = ',
                    ifelse(input$statsLabelFormat == "..p.signif..", 
                           '{input$statsLabelFormat}',
                           'format.pval({input$statsLabelFormat}, digits = {input$statsLabelDigits})'),'),
             method = "',statsMethods,'", 
             comparisons = ',statsPairwiseTestsText,') + ')
      }
    }
    
    if (input$statsAnova) {
      ## ANOVA 方差分析
      p <- paste0(p, 'stat_compare_means(method = "anova", 
             label.y = {input$statsLabelY}) + ')
    } else if (input$statsKruskal) {
      ## Kruskal-Wallis检验
      p <- paste0(p, 'stat_compare_means(label.y = {input$statsLabelY}) + ')
    }
  }
  
  if (input$autoScale == FALSE) {
    p <- paste0(p, 'ylim({input$minScale}, {input$maxScale}) + ')
  }
  if (input$plotFlip) {
    p <- paste0(p, 'coord_flip() + ')
  }
  
  ## Remove the 3 last characters of p, as we don't know where is the end
  p <- substr(p,1,nchar(p)-3)
  
  return(p)
}

getSelectedCombinations <- function(selectedCombinations) {
  ## Get the combinations in a list.
  statsPairwiseTests <- strsplit(selectedCombinations, 'vs')
  ## Start the list string.
  statsPairwiseTestsText <- "list("

  for (i in 1:length(statsPairwiseTests)) {
    statsPairwiseTestsText <- paste0(statsPairwiseTestsText,
                                     glue('c("{statsPairwiseTests[[i]][1]}",\\
                                          "{statsPairwiseTests[[i]][2]}"), '))}
  
  ## Remove trailing ", " and add the closing parenthesis of the list.
  statsPairwiseTestsText <- substr(statsPairwiseTestsText,1,nchar(statsPairwiseTestsText)-2)
  statsPairwiseTestsText <- paste0(statsPairwiseTestsText, ')')
  return(statsPairwiseTestsText)
}
