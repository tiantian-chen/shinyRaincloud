#用于拼接
library(glue)
#处理字符串
library(stringr)
#生成svg图片文件格式
library(svglite)

source("source/createPlot.R", local = TRUE)
source("source/dataUpload.R", local = TRUE)

server <- function(input, output, session) {
  
  # Read the input data.
  inputData <- callModule(dataUpload, "rainCloud")
  
  # Process the data.
  processedData <- reactive({callModule(dataManipulation, "rainCloud", 
                              inputData,
                              input$filterColumns)})

  # UI - Data - Filter the data.
  output$DataFilterColumnsUI <- renderUI({
    req(inputData$conditions())
    selectInput('filterColumns',
                label = HTML("<h5>Detected columns</h5>
                             <p>Use this input to filter out or move columns.</p>"), 
                choices = inputData$conditions(),
                selected = inputData$conditions(),
                multiple = TRUE)
  })
  
  # UI - Stats - pairwise comparison input.
  output$statsCombinationsUI <- renderUI({
    combinationList <- combn(input$filterColumns, 2, FUN = paste, 
                             collapse = 'vs')
    selectInput("statsCombinations", 
                label = h5("Conditions To Test"),
                choices = combinationList,
                multiple = TRUE)
  })
  
  # UI - Stats - default multiple comparison label height.
  output$statsLabelUI <- renderUI({
    numericInput('statsLabelY',
                 label = h5("Multiple Significance Label Y height"), 
                 min = 0, 
                 value = round(max(processedData()$df()$value)*1.05))
  })
  
  # UI - Plot - default scale limits.
  output$scaleLimitsUI <- renderUI({
    tagList(
      column(6,
             numericInput("minScale", 
                          label = h5("Min Scale Limit"), 
                          value = 0)
      ),
      column(6,
             numericInput("maxScale", 
                          label = h5("Max Scale Limit"), 
                          value = round(max(processedData()$df()$value)*1.1))
      )
    )
  })
  
  
  # Generate the plot code based on input options but do not evaluate yet.
  plotCode <- reactive({createPlot(input)})

  
  # Evaluate the code based on the processed data.
  plotFigure <- reactive({
    plotData <- processedData()$df()
    eval(parse(text = glue(plotCode())))
  })
  
  # Render the plot.
  output$rainCloudPlot <- renderPlot({
    # We don't render the plot without inputData.
    req(inputData$name())
    plotFigure()},
    height = function(x) input$height,
    width = function(x) input$width)
  
  # Download sample data
  
  output$downloadsampledata <- downloadHandler(
    filename <- function() {
      paste('1.txt')
    },
    content <- function(file) {
      input_file <- "1.txt"
      example_dat <- read.table(input_file, head = T, as.is = T, sep = "\t", quote = "")
      write.table(example_dat, file = file, row.names = F, quote = F, sep = "\t")
    }, contentType = 'text/csv') 
  

  # Download button
  output$downloadPlot <- downloadHandler(
    filename = function() {
      # rainCloudPlot-inputdata.txt.pdf
      paste(paste('rainCloudPlot-',inputData$name(), sep = ""), 
            input$downloadFormat, sep = ".")
    },
    content = function(file) {
      if(input$downloadFormat == 'tiff') {
        ggsave(file,
               plot = plotFigure(),
               device = input$downloadFormat,
               # Width and height are in inches. We increase the dpi to 300, so we
               # have to divide by 72 (original default pixels per inch) 
               width = input$width / 72,
               height = input$height / 72,
               compression = "lzw",
               units = "in",
               dpi = 300)
      } else {
        ggsave(file,
               plot = plotFigure(),
               device = input$downloadFormat,
               width = input$width / 72,
               height = input$height / 72,
               units = "in",
               dpi = 300)
      }
    }
  )
  
  
}