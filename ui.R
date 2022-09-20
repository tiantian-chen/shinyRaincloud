library(shiny)
library(shinythemes)
library(shinycssloaders)
source("source/dataUploadUI.R", local = TRUE)
source("source/paletteColours.R", local = TRUE)

ui <- fluidPage ( theme = shinytheme("yeti"),
  
  # Application title
  titlePanel("Raincloud"),
  
  # SidebarLayout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type = "pills",
              #part1  Data
                  tabPanel("Data",
                           column(12,
                                  hr()),
                           dataUploadUI("rainCloud", label = "File input"),
                           column(12,
                                  uiOutput('DataFilterColumnsUI')),
                           column(12,
                                  hr())),
              #part2 Plot options
                  tabPanel("Plot Options", 
                           hr(),
                           tabsetPanel(type = "pills",
                                       
                                       #part2.1 General Options
                                       tabPanel("General Options",
                                                column(12,
                                                       h4("Size")),
                                                column(6,
                                                       numericInput("height", 
                                                                    label = h5("Plot Height"), 
                                                                    value = 600)
                                                ),
                                                column(6,
                                                       numericInput("width", 
                                                                    label = h5("Plot Width"), 
                                                                    value = 600)
                                                ),
                                                column(6,
                                                       checkboxInput("plotLegend", 
                                                                     "Show Legend", 
                                                                     TRUE)
                                                ),
                                                column(6,
                                                       checkboxInput("plotFlip", 
                                                                     "Make it rain! (Flip the Axis)", 
                                                                     TRUE)
                                                ),
                                                column(6,
                                                       checkboxInput("plotMajorGrid", 
                                                                     "Plot Major Grid", 
                                                                     FALSE)
                                                ),
                                                column(6,
                                                       conditionalPanel(
                                                         condition = 'input.plotMajorGrid == true',
                                                         checkboxInput("plotMinorGrid", 
                                                                       "Plot Minor Grid", 
                                                                       FALSE))
                                                ),
                                                column(12,
                                                       h4('Extra Horizontal Line')),
                                                column(12,
                                                       checkboxInput("horizontalLine", 
                                                                     "Show Horizontal Line", 
                                                                     FALSE)),
                                                conditionalPanel(
                                                  condition = 'input.horizontalLine == true',
                                                  column(6,
                                                         numericInput("horizontalLinePosition", 
                                                                      label = h5("Position"), 
                                                                      value = 100)
                                                  ),
                                                  column(6,
                                                         selectInput("horizontalLineType", 
                                                                     label = h5("Linetype"),
                                                                     choices = list(
                                                                       "solid",
                                                                       "dashed",
                                                                       "dotted",
                                                                       "dotdash"
                                                                     ),
                                                                     selected = "dashed")
                                                  ),
                                                  column(6,
                                                         sliderInput("horizontalLineSize", 
                                                                     label = h5("Line Size"),
                                                                     min = 0,
                                                                     max = 2,
                                                                     value = 0.2,
                                                                     step = 0.1)
                                                  ),
                                                  column(6,
                                                         sliderInput("horizontalLineAlpha", 
                                                                     label = h5("Transparency"),
                                                                     min = 0,
                                                                     max = 1,
                                                                     value = 1,
                                                                     step = 0.05)
                                                  )),
                                                column(12,
                                                       hr())),
                                       
                                       #part2.2 Theme and colors
                                       tabPanel("Theme and colors",
                                                
                                                #part2.2.1 Theme
                                                column(12,
                                                       selectInput("plotTheme", 
                                                                   label = h5("Theme"),
                                                                   choices = list(
                                                                     "Default (Grey)" = "theme_grey()",
                                                                     "Black & White" = "theme_bw()",
                                                                     "Linedraw" = "theme_linedraw()",
                                                                     "Light" = "theme_light()",
                                                                     "Dark" = "theme_dark()",
                                                                     "Minimal" = "theme_minimal()",
                                                                     "Classic" = "theme_classic()",
                                                                     "Void" = "theme_void()",
                                                                     "Cowplot" = "theme_cowplot()"
                                                                   ),
                                                                   selected = "theme_cowplot()")),
                                                
                                                #part2.2.2 Palette
                                                column(12,
                                                       pickerInput(
                                                         inputId = "plotPalette", 
                                                         label = h5("Palette"),
                                                         choices = colors_pal, 
                                                         selected = "Set1", 
                                                         width = "100%",
                                                         choicesOpt = list(
                                                           content = sprintf(
                                                             "<div style='width:100%%;padding:2px;border-radius:4px;background:%s;color:%s'>%s</div>",
                                                             unname(background_pals), 
                                                             colortext_pals, 
                                                             names(background_pals)
                                                           )
                                                         )
                                                       )),
                                                column(12,
                                                       hr())),
                                       
                                       #part2.3 Titles
                                       tabPanel("Titles",
                                                column(12,
                                                       textInput("plotTitle", 
                                                                 label = h5("Main Plot Title"), 
                                                                 value = "Main Plot Title")),
                                                column(6,
                                                       textInput("xAxisTitle", 
                                                                 label = h5("x Axis Title"), 
                                                                 value = "x Axis Title")
                                                ),
                                                column(6,
                                                       textInput("yAxisTitle", 
                                                                 label = h5("y Axis Title"), 
                                                                 value = "y Axis Title")
                                                ),
                                                column(6,
                                                       numericInput("titleFontSize", 
                                                                    label = h5("Main Title Font Size"), 
                                                                    value = 20,
                                                                    min = 0)),
                                                column(6,
                                                       numericInput("axisLabelFontSize", 
                                                                    label = h5("Axis Title Font Size"), 
                                                                    value = 15,
                                                                    min = 0)),
                                                column(12,
                                                       hr())),
                                       
                                       #part2.4 Scale
                                       tabPanel("Scale",
                                                column(6,
                                                       numericInput("scaleFontSize", 
                                                                    label = h5("Scale Text Font Size"), 
                                                                    value = 15,
                                                                    min = 0)),
                                                column(6,
                                                       numericInput("xAxisAngle", 
                                                                    label = h5("X Axis Scale Angle"), 
                                                                    value = 0,
                                                                    min = 0,
                                                                    max = 360)),
                                                column(6,
                                                       selectInput("xAxishjust", 
                                                                   label = h5("Horizontal Scale Justification"), 
                                                                   choices = list(
                                                                     "Left" = 0,
                                                                     "Middle" = 0.5,
                                                                     "Right" = 1),
                                                                   selected = 0)),
                                                column(6,
                                                       selectInput("xAxisvjust", 
                                                                   label = h5("Vertical Scale Justification"), 
                                                                   choices = list(
                                                                     "Top" = 0,
                                                                     "Middle" = 0.5,
                                                                     "Bottom" = 1),
                                                                   selected = 0)),
                                                column(12,
                                                       checkboxInput("autoScale", 
                                                                     "Automatic Scale Limits", 
                                                                     TRUE)),
                                                conditionalPanel(
                                                  condition = "input.autoScale == false",
                                                  uiOutput("scaleLimitsUI")
                                                ),
                                                column(12,
                                                       hr())))),
              
              #part3 Dots
                  tabPanel("Dots",
                           column(12,
                                  hr()),
                           column(12,
                                  h4("Data Points")),
                           column(12,
                                  checkboxInput("plotDots", 
                                                "Plot Data Points", 
                                                TRUE)),
                           conditionalPanel(
                             condition = 'input.plotDots == true',
                             column(12,
                                    selectInput("dotColumnType", 
                                                label = h5("Column Type"),
                                                choices = list(
                                                  "Jitter" = 'jitterDots',
                                                  "Beeswarm" = 'beeswarm'
                                                ),
                                                selected = 'jitterDots')),
                             column(6,
                                    numericInput("dotSize", 
                                                 label = h5("Dot Size"), 
                                                 value = 2)
                             ),
                             column(6,
                                    selectInput("dotShape", 
                                                label = h5("Dot Shape"),
                                                choices = list(
                                                  "Empty Dot" = 1,
                                                  "Filled Dot" = 16,
                                                  "Square" = 15,
                                                  "Triangle" = 17),
                                                selected = 16)
                             ),
                             column(6,
                                    sliderInput("dotsWidth", 
                                                label = h5("Plot Width"),
                                                min = 0,
                                                max = 0.5,
                                                value = 0.15,
                                                step = 0.05)),
                             column(6,
                                    sliderInput("dotAlpha", 
                                                label = h5("Transparency"),
                                                min = 0,
                                                max = 1,
                                                value = 1,
                                                step = 0.05))),
                           column(12,
                                  hr())),
              
              #part4 Violinplots
                  tabPanel("Violin",
                           column(12,
                                  hr()),
                           column(12,
                                  h4("Violin Plots")),
                           column(12,
                                  checkboxInput("plotViolins", 
                                                "Plot Violins", 
                                                TRUE)),
                           conditionalPanel(
                             condition = 'input.plotViolins == true',
                             column(6,
                                    selectInput("violinType", 
                                                label = h5("Type of Violin"),
                                                choices = list(
                                                  "Full Violin" = "geom_violin",
                                                  "Half Violin" = "geom_flat_violin"
                                                ),
                                                selected = "geom_flat_violin")
                             ),
                             column(6,
                                    sliderInput("violinNudge", 
                                                label = h5("Center Offset"),
                                                min = 0,
                                                max = 0.5,
                                                value = 0.20,
                                                step = 0.05)
                             ),
                             column(6,
                                    selectInput("violinScale", 
                                                label = h5("Scale of the violin"),
                                                choices = list(
                                                  "Same Area" = "area",
                                                  "Maximum Width" = "width"
                                                ),
                                                selected = "width")
                             ),
                             column(6,
                                    numericInput("violinAdjust", 
                                                 label = h5("Bandwidth Adjustement"), 
                                                 value = 2,
                                                 min = 1)
                             ),
                             column(6,
                                    checkboxInput("violinTrim", 
                                                  "Trim Edges to Data Points", 
                                                  TRUE),
                                    conditionalPanel(
                                      condition =' input.violinType == "geom_violin"',
                                      checkboxInput("violinQuantiles",
                                                    "Draw 50% Quantile",
                                                    TRUE)
                                    )
                             ),
                             column(6,
                                    sliderInput("violinAlpha", 
                                                label = h5("Transparency"),
                                                min = 0,
                                                max = 1,
                                                value = 0.6,
                                                step = 0.05)
                             )
                           ),
                           
                           column(12,
                                  hr())),
              
              #part5 Boxplots
                  tabPanel("Boxplot",
                           column(12,
                                  hr()),
                           column(12,
                                  h4("Boxplots")),
                           column(12,
                                  checkboxInput("boxPlots", 
                                                "Plot Boxplots", 
                                                TRUE)
                           ),
                           conditionalPanel(
                             condition = 'input.boxPlots == true',
                             column(6,
                                    checkboxInput("boxplotNotch", 
                                                  "Add Notch", 
                                                  FALSE),
                                    checkboxInput("boxplotBoxWidth",
                                                  "Width Proportional to Data",
                                                  FALSE),
                                    checkboxInput("boxplotOutliers",
                                                  "Plot Outliers",
                                                  FALSE)
                             ),
                             column(6,
                                    sliderInput("boxplotWidth", 
                                                label = h5("Boxplot Width"),
                                                min = 0,
                                                max = 0.5,
                                                value = 0.1,
                                                step = 0.05)
                             ),
                             column(6,
                                    sliderInput("boxplotNudge", 
                                                label = h5("Center Offset"),
                                                min = 0,
                                                max = 0.5,
                                                value = 0.20,
                                                step = 0.05)
                             ),
                             column(6,
                                    sliderInput("boxplotAlpha", 
                                                label = h5("Transparency"),
                                                min = 0,
                                                max = 1,
                                                value = 0.3,
                                                step = 0.05)
                             )),
                           column(12,
                                  hr())), 
              
              #part6 Statistics
                  tabPanel("Statistics",
                           column(12,
                                  hr()),
                           column(12,
                                  h4("Significance")),
                           column(12,
                                  checkboxInput("statistics", 
                                                "Compare the means", 
                                                FALSE)
                           ),
                           conditionalPanel(
                             condition = 'input.statistics == true',
                             column(6,
                                    selectInput("statsType", 
                                                label = h5("Type of Test"),
                                                choices = list(
                                                  "Parametric" = "parametric",
                                                  "Non-parametric" = "nonParametric"),
                                                selected = "nonParametric")
                             ),
                             ## Parametric
                             conditionalPanel(
                               condition = 'input.statsType == "parametric"',
                               column(6,
                                      checkboxInput("statsTtest", 
                                                    "Pairwise (T-test)", 
                                                    FALSE),
                                      checkboxInput("statsAnova", 
                                                    "Multiple (ANOVA)", 
                                                    FALSE)
                               )),
                             ## Non Parametric
                             conditionalPanel(
                               condition = 'input.statsType == "nonParametric"',
                               column(6,
                                      checkboxInput("statsWilcoxon", 
                                                    "Pairwise (Wilcoxon Test)", 
                                                    FALSE),
                                      checkboxInput("statsKruskal", 
                                                    "Multiple (Kruskal-Wallis)", 
                                                    FALSE))),
                             div(class="clearfix"),
                             conditionalPanel(
                               ## Pairwise
                               condition = '(input.statsType == "nonParametric" && input.statsWilcoxon == true) || (input.statsType == "parametric" && input.statsTtest == true)',
                               column(12,
                                      uiOutput("statsCombinationsUI")),
                               column(12,
                                      selectInput('statsLabelFormat',
                                                  label = h5("Pairwise value Format"),
                                                  choices = list(
                                                    'Significance (stars)' = '..p.signif..',
                                                    'p-values' = '..p.format..'
                                                  )))
                             ),
                             conditionalPanel(
                               condition = '(input.statsType == "nonParametric" && input.statsKruskal == true) || (input.statsType == "parametric" && input.statsAnova == true)',
                               column(6,
                                      uiOutput("statsLabelUI")))),
                           ## Mean
                           column(12,
                                  h4("Mean")),
                           column(12,
                                  checkboxInput("statsMean", 
                                                "Plot mean", 
                                                FALSE)
                           ),
                           conditionalPanel(
                             condition = "input.statsMean == true",
                             column(6,
                                    selectInput('statsMeanErrorBars',
                                                label = h5("Add error bars to the mean"),
                                                choices = list(
                                                  'None' = 'none',
                                                  '95% Confidence Interval' = 'mean_cl_boot',
                                                  'Stardard Error' = 'mean_se',
                                                  'Standard Deviation' = 'mean_sd'
                                                ),
                                                selected = 'none')
                             ),
                             column(6,
                                    sliderInput("statsMeanWidth", 
                                                label = h5("Mean Width"),
                                                min = 0,
                                                max = 1,
                                                value = 0.5,
                                                step = 0.05)
                             ),
                             column(6,
                                    sliderInput("statsMeanNudge", 
                                                label = h5("Center Offset"),
                                                min = 0,
                                                max = 0.5,
                                                value = 0.20,
                                                step = 0.05)
                             ),
                             column(6,
                                    sliderInput("statsMeanSize", 
                                                label = h5("Line Size"),
                                                min = 0,
                                                max = 2,
                                                value = 0.2,
                                                step = 0.1)
                             )
                           ),
                           
                           column(12,
                                  hr())),
      ),
      column(12,
             h4("Download the plot")),
      column(12,
             p("Select the image format or download a zip file with all the 
               images, the script and data used to generate the plot.")),
      column(12,
             selectInput("downloadFormat",
                         label = "Image format",
                         choices = list(
                           "Vectorial" = list(
                             "pdf" = "pdf",
                             "svg" = "svg",
                             "eps" = "eps",
                             "jpg" ="jpg"
                           ),
                           "Non-vectorial" = list(
                             "tiff" = "tiff",
                             "png" = "png")
                         ),
                         selected = "pdf")),
      
      
      ## Clearfix
      tags$div(class = 'clearfix')
    ),
    
    # The mainPanel output
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           withSpinner(
                             plotOutput("rainCloudPlot", 
                                      height = "auto")),
                           
                           column(4,
                                  downloadButton("downloadPlot", 
                                                 label = "Download Image"))
                           ),
                  tabPanel("About",
                           includeHTML("source/about.html"))
                  
      )
    )
  )
)
