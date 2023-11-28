library(shiny)
library(plotly)
library(rep)

perf_ui <- function(id) {
  log_info("performance_ui <- function(id)")
  ns <- NS(id)

  selectGroup<-0



  fluidPage(
    fluidRow(
      column( 1,
    selectizeInput(
      inputId = ns("SelectPortfolios"),
      label = "Valitse portfoliorymä",
      choices = c("bas.de", "upm.he"),
      selected = "HOSKL",
      multiple = TRUE
    )),
    column( 8,
            selectizeInput(
      inputId = ns("SelectStock"),
      label = "Valitse osake",
      choices = c("bas.de", "upm.he"),
      selected = "HOSKL",
      multiple = TRUE
    )),
    column( 1, dateInput(
      inputId = ns("ZeroDate"),
      label = "Valitse päivä",
      value = "2022-05-20"
    )),
    column( 8, numericInput(
      inputId = ns("ZeroVal"),
      label = "0 päivän arvo",
      value = 10000
    ))),
    actionButton(ns("do"), "Päivitä"),
    plotlyOutput(outputId = ns("p"))
  )
}

# Server counterpart
perf_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    db <- readDB()


     updateSelectInput(session, "SelectPortfolios",
                       choices = levels(factor(db$Groups$Name)),
                       selected = levels(factor(db$Groups$Name))[1]
                       )
     updateSelectInput(session, "SelectStock",
                       choices = levels(factor(db$Trickers$Tricker)),
                       selected = levels(factor(db$Trickers$Tricker))[1]
     )
    #
    #
    # )

    output$p <- renderPlotly({
      rep_setOption("F:\\R codes\\Salkku1\\")

      selec <- input$SelectPortfolios
      st <- input$SelectStock

      retList <- rep_showPerformanceData(GroupName=selec, stocks = st)
      perf <- retList$perf
      colorss <- retList$colors
      valu <- retList$valu


      l <- list(
        font = list(
          family = "sans-serif",
          size = 10,
          color = "green"),
        bordercolor = "black",
        x = 0, y = 1,
        orientation = "h",
        borderwidth = 1)


       perf |>
        plotly::plot_ly(x=~perf$date) |>
        plotly::add_lines(y=~perf$End.Eq,
                          groups = ~perf$Portfolio,
                          color = ~perf$Portfolio
        ) |>
        plotly::layout(
          xaxis = list(
            title = "Päivä",
            rangeslider = list(type = "x"),
            tickformat="%d.%m.%Y"
          ),

          yaxis = list(
            title ="Salkkuarvo €",
            rangeslider = list(type = "y",
                               tickformat='0.0f',
                               tickprefix = '$'),
            tickformat = ".0f€",
            hoverformat = ".0f"
          ),
          legend = l
        ) |>
        plotly::config(displaylogo=FALSE)


        })
})
}
