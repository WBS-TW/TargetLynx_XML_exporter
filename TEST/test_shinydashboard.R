
library(shiny)
library(shinydashboard)
library(xml2)
library(tidyverse)
library(DT)
library(plotly)

options(shiny.maxRequestSize=30*1024^2) 

source("Targetlynx_functions.r", local = TRUE)


#HEADER---------------------------------------------------------------------------------

header <- dashboardHeader(title = "ML_XML_exp"
)

#SIDEBAR---------------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  fileInput('file1', 'Choose XML File', accept = c(".xml")),
  actionButton("click_file", "Select"),
  sidebarMenu(
    menuItem("Filtering", tabName = "filter", icon = icon("filter"),
             checkboxInput("blanks", "Exclude blanks"),
             checkboxInput("standards", "Exclude standards"),
             numericInput("decimal", "Number of decimals", value = 2)
    ),
    menuItem("Sample summary", tabName = "summary", icon = icon("chart-bar")),
    menuItem("Raw data table", tabName = "rawdata", icon = icon("table"))
  )
)

#BODY---------------------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "summary",
            fluidRow(
              DT::dataTableOutput("table_summary")
            ),
            fluidRow(
              plotOutput("plot1")
            )
    ),
    
    # Second tab content
    tabItem(tabName = "rawdata",
            fluidRow(
              DT::dataTableOutput("Table1")
            )
    )
  )
)


ui <- dashboardPage(header, sidebar, body)


#SERVER---------------------------------------------------------------------------------

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

shinyApp(ui, server)