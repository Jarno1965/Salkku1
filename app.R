#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(shiny)
library(ggplot2)
#library(DT)
library(echarts4r)

library(logger)

source("r\\Database.r")
source("View\\performance.r")
source("View\\maintenance.r")
source("View\\perf_ui.r")
source("View\\StartingState_ui.r")
source("View\\correlation_ui.r")

library(shiny)
library(tidyverse)




options(shiny.launch.browser = TRUE)



# Here is a container for the graph module
outer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(title = "Salkut",
               tabPanel(title = "Ylläpidä",
                        maintenance_ui(ns("maintenance"))),
               tabPanel(title = "Tuotot",
                        perf_ui(ns("performance"))),
               # tabPanel(title = "Sales Overview",
               #          graph_ui(ns("graph2"))),
               tabPanel(title = "Alkutila",
                        StartingState_ui(ns("StartingState"))),
               tabPanel(title = "Korrelaatio",
                        correlation_ui(ns("correlation"))),


               # tabPanel(title = "Sales Overview",
               #          graph_ui(ns("graph2"))),
               tabPanel(title = "Interactive Map",
                        "content 3")
    ),

  )
}

# Server counterpart
outer_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    maintenance_server("maintenance")
    # Making the graphs happen
    perf_server("performance")

    StartingState_server("StartingState")
    correlation_server("correlation")

  }
  )
}

# app ---------------------------------------------------------------------
ui <- fluidPage(

  outer_ui("mod1")
)

server <- function(input, output, session) {
  Sys.setenv("EXCHANGERATEHOST_ACCESS_KEY"="1961651d046d379145fa208c477e9639")
  log_setting()
  optionSettings()
  # baseDir <- getOption("baseDir")
  # options(DateFormat = "%d.%m.%Y")
  # options(DateFormatRH = "DD.MM.YY")
  # options(DatabaseFile = paste0(baseDir, "Data\\Datas.db"))
  # options( StockDataFile= paste0(baseDir,"Data\\stockData.RData"))
  # options( PortfolioResults= paste0(baseDir, "Data\\"))

  db<-NULL
  log_info("Luo server in app.R ")
  outer_server("mod1")
}



#shinyApp(ui, server)
# Run the application
shinyApp(ui = ui, server = server)


