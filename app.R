
library(shiny)
library(xml2)
library(tidyverse)
library(DT)
library(plotly)

options(shiny.maxRequestSize=30*1024^2) 

source("Targetlynx_functions.r", local = TRUE)

# ------------------- UI





# Define UI for application that draws a histogram
ui <- navbarPage("Masslynx quantification explorer",
                 tabPanel("File input & summary",
                          fluidPage(sidebarLayout(
                              sidebarPanel(
                                  fileInput('file1', 'Choose XML File', accept = c(".xml")),
                                  actionButton("click_file", "Select"),
                                  checkboxInput("blanks", "Exclude blanks"),
                                  checkboxInput("standards", "Exclude standards")
                                  ),
                              mainPanel(
                                  DT::dataTableOutput("table_summary"),
                                  plotOutput("plot1")
                              )
                          )
                          )
                 ),
                 tabPanel("Raw data table",
                          fluidPage(sidebarLayout(
                              sidebarPanel(
                                numericInput("decimal", "Number of decimals", value = 2),
                                downloadButton("dl", "Save as xlsx")
                                  
                              ),
                              mainPanel(
                                  DT::dataTableOutput("Table1")
                              )
                          )
                          )
                 ),
                 tabPanel("Recoveries",
                          fluidPage(sidebarLayout(
                              sidebarPanel(
                                  
                              ),
                              mainPanel(
                                  DT::dataTableOutput("Table2")
                              )
                          )
                          )
                 )
)
                 




# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- reactive({
        req(input$file1) ## ?req #  require that the input is available
        df <- read_xml(input$file1$datapath)
    })
    
    
    result_amount <- reactive({
        input_data <- data()
        t <- get_amounts(data = input_data, decimal = input$decimal, blanks = input$blanks, standards = input$standards)
    })
    
    observeEvent(input$click_file, {
        output$Table1 <- DT::renderDataTable({
            dt1 <- DT::datatable(result_amount(),
                                 options = list(
                                     pageLength = 50)
                                 )
            })
        
        output$table_summary <- DT::renderDataTable({
            summary_tab <- result_amount() %>%
                group_by(sample_type) %>%
                summarise_if(is.numeric, mean)
            DT::datatable(summary_tab,
                          options = list(
                              pageLength = 25)
                          )
            })
        
        output$plot1 <- renderPlot({
            plot_summary(result_amount())
        })
    
    
    })
    output$dl <- downloadHandler(
        filename = function() { "data.csv"},
        content = function(file) {write_excel_csv2(result_amount(), path = file)}
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
