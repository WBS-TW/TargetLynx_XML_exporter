
library(shiny)
library(shinydashboard)
library(xml2)
library(tidyverse)
library(DT)
library(plotly)

options(shiny.maxRequestSize=30*1024^2) 
options(shiny.reactlog=TRUE) 
source("D:/R_projects/TargetLynx_XML_exporter/Targetlynx_functions.r", local = TRUE)


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
    menuItem("Raw data table", tabName = "rawdata", icon = icon("table")),
    menuItem("Recoveries", tabName = "recoveries", icon = icon("wine-glass-alt"))
  ),
  downloadButton("dl", "Save as xlsx")
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
              plotOutput("plot1", height = 700)
            )
    ),
    
    # Second tab content
    tabItem(tabName = "rawdata",
            fluidRow(
              DT::dataTableOutput("table_raw")
            )
    ),
    tabItem(tabName = "recoveries",
            fluidRow(
              DT::dataTableOutput("table_recoveries"),
              fluidRow(
                plotOutput("plot2", height = 700)
              )
            ))
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
  result_recovery <- reactive({
    input_recovery <- data()
    r <- get_recovery(data = input_recovery, blanks = input$blanks, standards = input$standards)
  })
  
  observeEvent(input$click_file, {
    
    output$table_raw <- DT::renderDataTable({
      dt1 <- DT::datatable(result_amount(),
                           extensions = 'FixedColumns',
                           options = list(
                             pageLength = 20,
                             dom = 't',
                             scrollX = TRUE,
                             fixedColumns = list(leftColumns = 3))
      )
    })
    
    output$table_summary <- DT::renderDataTable({
      summary_tab <- result_amount() %>%
        group_by(sample_type) %>%
        summarise_if(is.numeric, mean)
      DT::datatable(summary_tab,
                    extensions = 'FixedColumns',
                    options = list(
                      pageLength = 20,
                      dom = 't',
                      scrollX = TRUE,
                      fixedColumns = TRUE)
      )
    })
    
    
    output$table_recoveries <- DT::renderDataTable({
      rec_color <- sapply(result_recovery(), is.numeric)
      
      datatable(result_recovery(),
                extensions = 'FixedColumns',
                options = list(
                  pageLength = 20,
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 3))) %>%
        formatStyle(names(result_recovery()[rec_color]), 
                    color = styleInterval(c(0, 25, 50, 75, 125, 150),
                                          c("red", "red", "blue", "black", "black", "blue", "red")))
      
    })
    
    output$plot1 <- renderPlot({
      plot_summary(result_amount())
    })
    
    output$plot2 <- renderPlot({
      plot_summary(result_recovery())
    })
    
    
    
  })
  output$dl <- downloadHandler(
    filename = function() { "data.csv"},
    content = function(file) {write_excel_csv2(result_amount(), path = file)}
  )
}

shinyApp(ui, server)