library(shiny)
library(plotly)
library(rep)

correlation_ui <- function(id) {
  log_info("correlation_ui  <- function(id)")
  ns <- NS(id)

  selectGroup<-0



  fluidPage(
    fluidRow(
      selectizeInput(
        inputId = ns("SelectPortfolios"),
        label = "Valitse salkut",
        choices = c("bas.de", "upm.he"),
        selected = "HOSKL",
        multiple = TRUE
      ),
      selectizeInput(
        inputId = ns("SelectStocks"),
        label = "Valitse osakkeita",
        choices = c("bas.de", "upm.he"),
        selected = "HOSKL",
        multiple = TRUE
      ),

    ),
    echarts4r::echarts4rOutput(ns("Correlation"))
  )
}

# Server counterpart
correlation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    db <- readDB()

    log_info("correlation_server <- function(id)")
    updateSelectInput(session, "SelectStocks",
                      choices = levels(factor(db$Trickers$Tricker)),
                      selected = levels(factor(db$Trickers$Tricker))[1]
    )

    updateSelectInput(session, "SelectPortfolios",
                      choices = levels(factor(db$Portfolio$Name)),
                      selected = levels(factor(db$Portfolio$Name))[1]
    )
    output$Correlation = echarts4r::renderEcharts4r({
      ss <- input$SelectStocks
      pfn <- input$SelectPortfolios
      rep_showCorr(PortfolioNames = pfn, StockNames = ss)
          })

    #
    #
    # )


  })
}
