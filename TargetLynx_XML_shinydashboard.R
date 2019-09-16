
library(shiny)
library(shinydashboard)
library(xml2)
library(tidyverse)
library(DT)
library(pheatmap)
library(writexl)
library(RColorBrewer)

options(shiny.maxRequestSize=50*1024^2) 
options(shiny.reactlog=TRUE) 
source("Targetlynx_functions.r", local = TRUE)


#HEADER---------------------------------------------------------------------------------

header <- dashboardHeader(title = "ML_XML_exp")

#SIDEBAR---------------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  fileInput('file1', 'Choose XML File', accept = c(".xml")),
  actionButton("click_file", "Select"),
  sidebarMenu(
    menuItem(tags$b("Filtering"), tabName = "Filter", icon = icon("filter"),
             checkboxInput("CheckBlanks", "Exclude blanks"),
             checkboxInput("CheckStandards", "Exclude standards"),
             numericInput("Decimal", "Number of decimals", value = 2)
    ),
    hr(),
    menuItem(tags$b("Sample summary"), tabName = "Summary", icon = icon("chart-bar")),
    selectInput("SelectSummaryPlots", "Select plot", choices = c("None", "SummaryScatter", "SummaryHeatmap"), selected = "SummaryHeatmap"),
    uiOutput("HeatmapScale"),
    uiOutput("HeatmapClusterRows"),
    uiOutput("HeatmapClusterCols"),
    hr(),
    menuItem(tags$b("Recoveries"), tabName = "Recovery", icon = icon("wine-glass-alt")),
    selectInput("SelectRecoveryPlots", "Select output", choices = c("RecoveryHeatmap", "RecoveryScatter", "RecoveryTable"),
                selected = "RecoveryHeatmap"),
    hr(),
    menuItem(tags$b("Raw data table"), tabName = "Rawdata", icon = icon("table"))
  ),
  hr(),
  downloadButton("dl", "Save as xlsx")
)

#BODY---------------------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    # Summary tab content
    tabItem(tabName = "Summary",
            # fluidRow(
            #   DT::dataTableOutput("SummaryTable")
            # ),
            fluidRow(
              uiOutput("SummaryPlot")
            )
    ),
    
    # Recovery tab content
    tabItem(tabName = "Recovery",
            fluidRow(
              uiOutput("RecoveryPlot"),
              uiOutput("RecoveryTables")
            )
    ),
    
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

server <- function(input, output, session) {
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
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
    
    
    # output$SummaryTable <- DT::renderDataTable({
    #   summary_tab <- result_amount() %>%
    #     group_by(sample_type) %>%
    #     summarise_if(is.numeric, mean)
    #   DT::datatable(summary_tab,
    #                 extensions = 'FixedColumns',
    #                 options = list(
    #                   pageLength = 20,
    #                   dom = 't',
    #                   scrollX = TRUE,
    #                   fixedColumns = TRUE)
    #   )
    # })
    
    
    # UI dashboard
    output$HeatmapScale <- renderUI({
      if (input$SelectSummaryPlots == "SummaryHeatmap") {
        selectInput("SelectHeatmapScale", "Scaling", choices = c("none", "row", "column"), selected = "none")
      }
    })
    output$HeatmapClusterRows <- renderUI({
      if (input$SelectSummaryPlots == "SummaryHeatmap") {
        checkboxInput("SelectHeatmapClusterRows", "Cluster rows")
      }
    })
    output$HeatmapClusterCols <- renderUI({
      if (input$SelectSummaryPlots == "SummaryHeatmap") {
        checkboxInput("SelectHeatmapClusterCols", "Cluster columns")
      }
    })
    
    
    # Plots
    # Amounts
    output$SummaryPlot <- renderUI(
      if (input$SelectSummaryPlots == "SummaryScatter") {
        plotOutput("UISummaryPlot", height = 700)
      } else if (input$SelectSummaryPlots == "SummaryHeatmap") {
        plotOutput("UISummaryPlot", height = 700)
      } else if (input$SelectSummaryPlots == "None") {
        NULL
      })
    
    output$UISummaryPlot <- renderPlot({
      if (input$SelectSummaryPlots == "SummaryScatter") {
        plot_summary(result_amount())
      } else if (input$SelectSummaryPlots == "SummaryHeatmap") {
        plot_heatmap(result_amount(), scale = input$SelectHeatmapScale, 
                     cluster_rows = input$SelectHeatmapClusterRows, cluster_cols = input$SelectHeatmapClusterCols)
      } else if (input$SelectSummaryPlots == "None") {
        NULL
      }
    })
    
    
    # Recovery
    output$RecoveryPlot <- renderUI(
      if (input$SelectRecoveryPlots == "RecoveryScatter") {
        plotOutput("UIRecoveryPlot", height = 700)
      } else if (input$SelectRecoveryPlots == "RecoveryHeatmap") {
        plotOutput("UIRecoveryPlot", height = 700) 
      })
    
    output$RecoveryTables <- renderUI(
      if (input$SelectRecoveryPlots == "RecoveryTable") {
        DT::dataTableOutput("UIRecoveryTable", height = 700)
      })
    
    
    output$UIRecoveryTable <- DT::renderDataTable({
      if (input$SelectRecoveryPlots == "RecoveryTable") {
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
      }
    })
    
    output$UIRecoveryPlot <- renderPlot({
      if (input$SelectRecoveryPlots == "RecoveryScatter") {
        plot_summary(result_recovery())
      } else if (input$SelectRecoveryPlots == "RecoveryHeatmap") {
        plot_recovery_heatmap(result_recovery(), scale = "none", cluster_rows = FALSE, cluster_cols = FALSE)
      }
    })
    
    
    
    
    
    
    
    
  })
  output$dl <- downloadHandler(
    filename = function() { "data.xlsx"},
    content = function(file) {
      write_xlsx(list(Amounts = result_amount(), Recoveries = result_recovery()), path = file)
    }
  )
}

shinyApp(ui, server)