dataUploadUI <- function(id, label = "File input") {
  
  ns <- NS(id)
  
  tagList(
    column(12,
           h4("Data Upload"),
           p("Upload a .txt or .csv file with the different 
                             conditions and values separated by columns."),
           fileInput(ns("excelFile"), 
                     label = h5(label),
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           column(6,
                  checkboxInput(ns("sampleData"), "Drawing with sample data", FALSE)),
           column(6,
                  downloadButton("downloadsampledata", 
                                 label = "Download sample data")),
           p("If the drawing does not appear directly on the right, select the details of the uploaded file here.")),
    column(6,
           checkboxInput(ns("header"), "Header", TRUE)),
    column(6,
           radioButtons(ns("decimalPoint"), "Decimal separator",
                        choices = c(Comma = ',',
                                    Point = '.'),
                        selected = ',',
                        inline = TRUE)),
    column(6,
           radioButtons(ns("sep"), "Column Delimiter",
                        choices = c(Comma = ",",
                                    Semicolon = ";",
                                    Tab = "\t"),
                        selected = "\t")
    ),
    column(6,
           radioButtons(ns("quote"), "Quote",
                        choices = c(None = "",
                                    "Double Quote" = "\"",
                                    "Single Quote" = "'"),
                        selected = ""))
    
  )
}
