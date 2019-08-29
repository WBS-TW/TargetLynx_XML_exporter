
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
    menuItem("Filtering", tabName = "Filter", icon = icon("filter"),
             checkboxInput("CheckBlanks", "Exclude blanks"),
             checkboxInput("CheckStandards", "Exclude standards"),
             numericInput("Decimal", "Number of decimals", value = 2)
    ),
    hr(),
    menuItem("Sample summary", tabName = "Summary", icon = icon("chart-bar")),
    checkboxInput("CheckSummaryPlot", "Show summary plots"),
    hr(),
    menuItem("Recoveries", tabName = "Recovery", icon = icon("wine-glass-alt")),
    checkboxInput("CheckRecoveryPlot", "Show recovery plots"),
    hr(),
    menuItem("Raw data table", tabName = "Rawdata", icon = icon("table"))
  ),
  hr(),
  downloadButton("dl", "Save as xlsx")
)

#BODY---------------------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    # Summary tab content
    tabItem(tabName = "Summary",
            fluidRow(
              DT::dataTableOutput("SummaryTable")
            ),
            fluidRow(
              uiOutput("SummaryPlot")
            )
    ),
    
    # Recovery tab content
    tabItem(tabName = "Recovery",
            fluidRow(
              DT::dataTableOutput("table_recoveries"),
              fluidRow(
                uiOutput("RecoveryPlot")
              )
            )),
    
    # Rawdata tab content
    tabItem(tabName = "Rawdata",
            fluidRow(
              DT::dataTableOutput("table_raw")
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
    t <- get_amounts(data = input_data, decimal = input$Decimal, blanks = input$CheckBlanks, standards = input$CheckStandards)
  })
  result_recovery <- reactive({
    input_recovery <- data()
    r <- get_recovery(data = input_recovery, blanks = input$CheckBlanks, standards = input$CheckStandards)
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
    
    output$SummaryTable <- DT::renderDataTable({
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
    
    # Plots
    
    output$SummaryPlot <- renderUI(
      if (input$CheckSummaryPlot) {
        plotOutput("UISummaryPlot", height = 700)
      })
    output$UISummaryPlot <- renderPlot({
      plot_summary(result_amount())
    })
    
    output$RecoveryPlot <- renderUI(
      if (input$CheckRecoveryPlot) {
        plotOutput("UIRecoveryPlot", height = 700)
      })
    output$UIRecoveryPlot <- renderPlot({
      plot_summary(result_recovery())
    })
    
    
    
    
    
  })
  output$dl <- downloadHandler(
    filename = function() { "data.csv"},
    content = function(file) {write_excel_csv2(result_amount(), path = file)}
  )
}

shinyApp(ui, server)