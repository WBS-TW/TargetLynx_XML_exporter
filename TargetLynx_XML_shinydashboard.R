
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
    menuItem(tags$b("Sample Amounts"), tabName = "Amounts", icon = icon("chart-bar")),
    selectInput("SelectAmountsPlots", "Select plot", choices = c("None", "AmountsScatter", "AmountsHeatmap"), selected = "AmountsHeatmap"),
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
  
  tags$head(tags$style(HTML('
                          /* logo */
                          .skin-blue .main-header .logo {
                          background-color: rgb(255,255,255); color:        rgb(0,144,197);
                          font-weight: bold;font-size: 24px;text-align: Right;
                          }

                          /* logo when hovered */

                          .skin-blue .main-header .logo:hover {
                          background-color: rgb(255,255,255);
                          }


                          /* navbar (rest of the header) */
                          .skin-blue .main-header .navbar {
                          background-color: rgb(255,255,255);
                          }

                          /* main sidebar */
                          .skin-blue .main-sidebar {
                          background-color: rgb(163, 172, 196);;
                          }

                          # /* main body */
                          # .skin-blue .main-body {
                          # background-color: rgb(0,120,150);
                          # }

                          /* active selected tab in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: rgb(6, 34, 117);
                          color: rgb(255,255,255);font-weight: bold;font-size: 18px;
                          }

                          /* other links in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                          background-color: rgb(19, 87, 72);
                          color: rgb(255,255,255);font-weight: bold;
                          }

                          /* other links in the sidebarmenu when hovered */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: rgb(232,245,251);color: rgb(0,144,197);font-weight: bold;
                          }

                          /* toggle button color  */
                          .skin-blue .main-header .navbar .sidebar-toggle{
                          background-color: rgb(255,255,255);color:rgb(0,144,197);
                          }

                          /* toggle button when hovered  */
                          .skin-blue .main-header .navbar .sidebar-toggle:hover{
                          background-color: rgb(0,144,197);color:rgb(255,255,255);
                          }

                           '))),
  
  tabItems(
    # Amounts tab content
    tabItem(tabName = "Amounts",
            # fluidRow(
            #   DT::dataTableOutput("AmountsTable")
            # ),
            fluidRow(
              uiOutput("AmountsPlot")
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
    
    
    # output$AmountsTable <- DT::renderDataTable({
    #   Amounts_tab <- result_amount() %>%
    #     group_by(sample_type) %>%
    #     summarise_if(is.numeric, mean)
    #   DT::datatable(Amounts_tab,
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
      if (input$SelectAmountsPlots == "AmountsHeatmap") {
        selectInput("SelectHeatmapScale", "Scaling", choices = c("none", "row", "column"), selected = "none")
      }
    })
    output$HeatmapClusterRows <- renderUI({
      if (input$SelectAmountsPlots == "AmountsHeatmap") {
        checkboxInput("SelectHeatmapClusterRows", "Cluster rows")
      }
    })
    output$HeatmapClusterCols <- renderUI({
      if (input$SelectAmountsPlots == "AmountsHeatmap") {
        checkboxInput("SelectHeatmapClusterCols", "Cluster columns")
      }
    })
    
    
    # Plots
    # Amounts
    output$AmountsPlot <- renderUI(
      if (input$SelectAmountsPlots == "AmountsScatter") {
        plotOutput("UIAmountsPlot", height = 700)
      } else if (input$SelectAmountsPlots == "AmountsHeatmap") {
        plotOutput("UIAmountsPlot", height = 700)
      } else if (input$SelectAmountsPlots == "None") {
        NULL
      })
    
    output$UIAmountsPlot <- renderPlot({
      if (input$SelectAmountsPlots == "AmountsScatter") {
        plot_Amounts(result_amount())
      } else if (input$SelectAmountsPlots == "AmountsHeatmap") {
        plot_heatmap(result_amount(), scale = input$SelectHeatmapScale, 
                     cluster_rows = input$SelectHeatmapClusterRows, cluster_cols = input$SelectHeatmapClusterCols)
      } else if (input$SelectAmountsPlots == "None") {
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
        plot_Amounts(result_recovery())
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