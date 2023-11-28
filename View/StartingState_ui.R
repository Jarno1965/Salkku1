library(shiny)
library(plotly)
library(rep)

StartingState_ui <- function(id) {
  log_info("StartingState <- function(id)")
  ns <- NS(id)

  selectGroup<-0



  fluidPage(
    fluidRow(
              selectizeInput(
                inputId = ns("SelectPortfolios"),
                label = "Valitse salkku",
                choices = c("bas.de", "upm.he"),
                selected = "HOSKL",
                multiple = TRUE
              ),

      ),
    DT::dataTableOutput(ns("StartState"))
  )
}

# Server counterpart
StartingState_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    db <- readDB()

    log_info("StartingState_server <- function(id)")


    updateSelectInput(session, "SelectPortfolios",
                      choices = levels(factor(db$Portfolio$Name)),
                      selected = levels(factor(db$Portfolio$Name))[1]
    )
    output$StartState = DT::renderDataTable({

      rep_setOption("F:\\R codes\\Salkku1\\")
      pfs <- input$SelectPortfolios


      data <- rep_DividendData(GroupName = NULL, pfs)

      data |> DT::datatable(filter='top', rownames = FALSE,
                            options = list(
                              autoWidth=TRUE
                            )
      ) |>
        DT::formatRound(columns=c('Lkm', 'Hinta', "Summa"),
                        digits=2, mark = "", dec.mark = ",")|>
        DT::formatRound(columns=c("Saldo"),
                        digits=0, mark = "", dec.mark = ",")
    })

    #
    #
    # )


  })
}
