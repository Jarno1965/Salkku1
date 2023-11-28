library(RSQLite)
library(rhandsontable)
library(logger)
library(shinydashboard)
library(shinyWidgets)
library(tcltk)
library(DT)
library(priceR)
# APIKEY: 1961651d046d379145fa208c477e9639
library(waiter)

loadStockData <- function( stockData, StockInfo){

  tricker <-StockInfo$Tricker
  #mydb <- getOption("StockDataFile")
  # mydb <- "Data\\stockData.RData"
  # stDname <- load( file =mydb)
  # stockData <- get(stDname)
  # stockData=NULL

  #curr <- unique(curr$stocks$OsingonValuutta[!is.na(curr$stocks$OsingonValuutta)])
#browser()

  na <- load(file="Data\\stocks.RData")
  StockInfo <- get(na)
  currAndStocks<- readStockAndCurrencies()
  currAndStocks <- updateCurrencies(currAndStocks)

  st<-NULL
  for( tr in tricker){

    firstDate<-"2000-01-01"
    lastDate <- Sys.Date()
    tryCatch({
      div <- yf_get_dividends(ticker=tr, first_date =firstDate )
    }, error=function(e){
      print(paste("Ei löytynyt osinkoja ", tr))
      div<-NULL
    })

    # Check should we change dividends currency to Euros
    si <- StockInfo |> filter( Tricker==tr)
    if( !is.na(si$OsingonValuutta) & si$OsingonValuutta!="" & si$OsingonValuutta!="EUR"){
      for( i in 1:nrow(div)){
        currRate <- currAndStocks$currency |> filter(date == div[i,]$ref_date & currAndStocks$currency$currency==si$OsingonValuutta)
        if( nrow( currRate)>0){
          div[i,]$dividend <- div[i,]$dividend*currRate$rate[1]
        }
      }
    }

    df_yf <- yf_get(tickers = tr,
                    first_date = firstDate,
                    last_date = lastDate,
                    thresh_bad_data = 0)
    df_yf$CashDiv <- 0
    if( !is.null(div)>0 ){
      for( ii in 1:nrow(div)){
        df_yf[df_yf['ref_date']==toString(pull(div[ii,'ref_date'])) & df_yf['ticker']==pull(div[ii,'ticker']),]$CashDiv=
          as.numeric(div[ii, 'dividend'])

      }
    }

    df <- df_yf[, names(df_yf) %in% c( 'price_close', 'ref_date')]
    dff <- df_yf[, !(names(df_yf) %in% c( 'ret_adjusted_prices', 'ret_closing_prices',
                                          'price_adjusted', 'cumret_adjusted_prices', 'volume'))]
    dff$DivSum <- cumsum( dff$CashDiv)
    dff <- dff |> mutate( price_adjusted = price_close+DivSum)
    cr <- coredata(Return.calculate(xts(x=dff$price_adjusted, order.by=dff$ref_date)))

    dff$ret_closing_prices <- cr[,1]


    cd <- replace_na(coredata(dff$ret_closing_prices), 0)

    ddd<-dff |> mutate(cumret_adjusted_prices = cumprod(1 + cd))

    st <- rbind( st, ddd)
  }
  stockData<-st


  # mydb <- getOption("StockDataFile")
  # save( stockData, file =mydb)
  return(stockData)

  # st$cumret_adjusted_prices <- st$cumret_adjusted_prices*100
  # valu <- min(st$cumret_adjusted_prices)*0.95
  # st |> group_by(ticker) |> e_charts(ref_date) |> e_line(cumret_adjusted_prices) |> e_datazoom() |>
  #   e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
  #   e_title(text = "HOSKn mallisalkut", left = "left") |>
  #   e_toolbox_feature("dataZoom") |> e_theme("chalk") |> e_animation(duration = 1000) |>
  #   e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
  #   e_y_axis(
  #     formatter = e_axis_formatter(style=c("currency"),
  #                                  digits=0,
  #                                  currency="EUR"
  #     ))
  #
  #
  # ddd %>%
  #   ggplot(aes(x = ref_date, y = cumret_adjusted_prices)) +
  #   geom_line() +
  #   labs(x = 'Date',
  #        y = 'Cumulative Returns',
  #        title = 'Portfolio Cumulative Returns') +
  #   theme_classic() +
  #   scale_y_continuous(breaks = seq(1,2,0.1)) +
  #   scale_x_date(date_breaks = 'year',
  #                date_labels = '%Y')

}

updateCurrencies<-function(currAndStocks){
  # Tämä heitti poikkeuksen joten tutki
  #return(currAndStocks)


  log_info("updateCurrencies<-function(StockInfo) ")
  out <- tryCatch(
    {
  #currAndStocks<- readStockAndCurrencies() # poista testin jälkeen
#browser()
  curr<-unique(currAndStocks$stocks$OsingonValuutta[!is.na(currAndStocks$stocks$OsingonValuutta) &
                                                      currAndStocks$stocks$OsingonValuutta!="EUR"])
  log_info("curr {curr} ")
  lastDates=NULL
  if( nrow(currAndStocks$currency )>0) lastDates <- currAndStocks$currency |> group_by(currency) |> filter( max(date)==date)
for( cr in curr) {
 # browser()

  startDay= "2000-01-01"
  ld<-NULL
  if(!is.null(lastDates)) ld <- lastDates |>  filter(currency==cr)

  if( !is.null(ld)) if( nrow(ld)==1) {startDay= ld$date+1}
  if( startDay > today()) startDay <- today()

  da<-NULL
  if(startDay < today())  {
    #browser()
    da <- from_to_dates_rates(cr, "EUR", dates = list(startDay, today()))
  }
  if( !is.null(da)) {
    da$currency = cr
    currAndStocks$currency <- rbind( currAndStocks$currency, da)
  }
}
  #browser()
  writeCurrenciesToDB(currAndStocks$currency)
#browser()
    },
error = function(cond){
  message("Valuutan päivitys heitti virheen")
  return(NA)
})
  return(currAndStocks)
  }




maintenance_ui <- function(id) {
  log_info("maintenance_ui <- function(id) db={db}")
  ns <- NS(id)

  fluidPage(
    titlePanel("Osakkeet!"),


      mainPanel(
        fluidPage(
          use_waiter(),
        fluidRow(
          column(10,
                 h4("Osakelista"),
          rHandsontableOutput(ns("stocks"), width=800)),
          column(4,actionButton(ns("SaveStocks"), "Tallenna kaikki")),
          column(6,actionButton(ns("UpdateStocks"), "Päivitä osakelista")),
          column(9,actionButton(ns("ShowStockData"), "Näytä osaketiedot")),
          column(10, verbatimTextOutput(ns("UpdateStockMessages"))),
          uiOutput(ns("ExportStock")),
          column(4,actionButton(ns("hideStockData"), "Piilota osaketiedot")),
          column(6,actionButton(ns("updateCurrencies"), "Päivitä valuutat")),
          DT::dataTableOutput(ns("stockData2")),


          #rHandsontableOutput(ns("stockData")),

        ),
        fluidRow(
          HTML("<br/><br/>"),
          h4("Salkkulista"),
          rHandsontableOutput(ns("portfolio")),
          actionButton(ns("UpdatePortfolio"), "Päivitä salkkulista")

          ),

        fluidRow(
          HTML("<br/>"),
          h4("Salkun määrittäminen"),
          column(width=12, rHandsontableOutput(ns("changePortfolio"), width=800, height=400)),
          #DTOutput(ns("changePortfolioDT")),
          actionButton(ns("UpdatePortfolioData"), "Tallenna salkkudata"),
          actionButton(ns("UpdatePortfolioReturn"), "Päivitä salkkutuotto"),
          actionButton(ns("ShowPortfolioReturn"), "Näytä salkkutuotto"),
          #rHandsontableOutput(ns("showPortfolioResult"))
          #DT::dataTableOutput(ns("showPortfolioResult2")),


        ),

        fluidRow(
          h4("Salkkuryhmät"),
          column(10,
          rHandsontableOutput(ns("portfolioGroups")),
          actionButton(ns("UpdatePortfolioGroups"), "Päivitä salkkuryhmät")
          )


          )

        )
      )

  )
}


findPfData <- function(pfName){
  browser()
  mydb <- dbConnect(RSQLite::SQLite(), "Data\\Datas.db")
  retV <- dbReadTable(mydb, "Portfolios")
  dbDisconnect(mydb)

  ss<-filter( retV, PortfolioName==pfName)
  if( nrow(ss)==0){
    ss<-data.frame(
      PortfolioName =c(pfName),
      Date          = Sys.Date(),
      MoneyTransfer= c(""),
      Tricker = "",
      Qnt = 0,
      Price = 0 )

  }
  return(ss)
}

updatePfGroupnames<- function( tableData, dataBaseData){

  FbData<-dataBaseData
  tableData=hrr
}

ReadTrickers <- function( datas){

}


maintenance_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    w <- Waiter$new(id = "waiter")
    dataBaseData <- readDB()




    rv <- reactiveValues(dataBaseData = dataBaseData,
                         showResult = 1,
                         editablePortfolio=NULL,
                         portfolioReturn=NULL,
                         showStockUpdate="qqqqqqqq")

    dataBaseData<-NULL

    # mydb <- getOption("StockDataFile")
    load(getOption("StockDataFile")) # stockData


    observeEvent(input$updateCurrencies, {
      log_info("observeEvent(input$updateCurrencies {colnames(rv$dataBaseData$Trickers)}")
    })

    output$UpdateStockMessages <- renderText({ rv$showStockUpdate  })
    observeEvent(input$SaveStocks, {
      log_info("observeEvent(input$SaveStocks {colnames(rv$dataBaseData$Trickers)}")
      #browser()
      tr <- hot_to_r(input$stocks )
      rv$dataBaseData$Trickers <- tr


      saveDB(rv$dataBaseData)
    })



    observeEvent(input$UpdatePortfolioData, {
      log_info("observeEvent(input$UpdatePortfolioData, {colnames(rv$dataBaseData$Trickers)}")


      chgPortfolio<-hot_to_r(input$changePortfolio)
      pf <-  hot_to_r(input$portfolio)
      Name <- pf[input$portfolio_select$select$r, 1]
      #format(ss$date, "%d.%m.%Y")
       hrr<-hot_to_r(input$changePortfolio)
       hrr$PortfolioName<-Name
       # Save portfoliolist
       pfd<-hot_to_r(input$portfolio)
       rv$dataBaseData$Portfolio <- pfd

       rv$dataBaseData$Portfolios <- subset( rv$dataBaseData$Portfolios, rv$dataBaseData$Portfolios$PortfolioName!=Name)
       rv$dataBaseData$Portfolios<-rbind(rv$dataBaseData$Portfolios, hrr)
       saveDB(rv$dataBaseData)


    })

    observeEvent(input$UpdatePortfolioReturn, {


      # shinyjs::hide(id = "changePortfolio")
      # Sys.sleep(5)
      # shinyjs::show(id = "changePortfolio")
      # browser()
      waiter_show( # show the waiter
        html = spin_fading_circles() # use a spinner
      )

      log_info("observeEvent(input$UpdatePortfolioReturn, {colnames(rv$dataBaseData$Trickers)}")
      disable("UpdatePortfolioReturn")
      tryCatch({

      pf <-  hot_to_r(input$portfolio)
      Name <- pf[input$portfolio_select$select$r, 1]


      hrr<-hot_to_r(input$changePortfolio)
      hrr$PortfolioName<-Name
      # Save portfoliolist
      pfd<-hot_to_r(input$portfolio)
      rv$dataBaseData$Portfolio <- pfd

      rv$dataBaseData$Portfolios <- subset( rv$dataBaseData$Portfolios, rv$dataBaseData$Portfolios$PortfolioName!=Name)
      rv$dataBaseData$Portfolios<-rbind(rv$dataBaseData$Portfolios, hrr)

      saveDB(rv$dataBaseData)
      portfolioValues(Name, updateStock=TRUE)
      dbb <- readDB()

      rv$dataBaseData$Trickers <-    dbb$Trickers

      }, error=function(e){
        showModal(modalDialog(
          title = "Error!",
          paste0("Salkun päivittämisssä tapahtui virhe ",e$message),
        ))
      }

    )
      waiter_hide()
      enable("UpdatePortfolioReturn")
      rv$showResult <- rv$showResult+1
    }




)


    observeEvent(input$UpdateStocks, {
   rv$showStockUpdate <- "Päivittää osaketietoja"

      log_info("observeEvent(input$UpdateStocks")
      waiter_show( # show the waiter
        html = spin_fading_circles() # use a spinner
      )

      #str(as.Date(dataBaseData$Trickers$FirstData, getOption("DateFormat"))+1)
      # dd<-as.Date(rv$dataBaseData$Trickers$FirstData, getOption("DateFormat"))
      # dd<- dd+1
      # ddd <- format(dd, getOption("DateFormat"))

      #rv$dataBaseData$Trickers$FirstData <- ddd

      tryCatch({
        hrr<-hot_to_r(input$stocks)
        rv$dataBaseData$Trickers <- hrr

        rv$dataBaseData$Portfolio<- hot_to_r(input$portfolio)

        saveDB(rv$dataBaseData)

        rv$dataBaseData<-updateStockDatas(rv$dataBaseData)
        rv$dataBaseData$Trickers$FirstData <- format( as.Date(as.numeric(rv$dataBaseData$Trickers$FirstData), origin = "1970-01-01"), getOption("DateFormat"))
        rv$dataBaseData$Trickers$LastUpdate <- format( as.Date(as.numeric(rv$dataBaseData$Trickers$LastUpdate), origin = "1970-01-01"), getOption("DateFormat"))


        saveDB(rv$dataBaseData)
        log_info("Säilöi osakedatan")
        waiter_hide()
      }, error=function(e){
        waiter_hide()
        showModal(modalDialog(
          title = "Error!",
          paste0("Luultavasti sinulla on väärä trikkeri ",e$message),

        ))
      }
      )



      #log_info("after observeEvent(input$UpdateStocks, {colnames(rv$dataBaseData$Trickers)}")

      })


    observeEvent(input$UpdatePortfolioGroups, {
      log_info("input$UpdatePortfolioGroups, {colnames(rv$dataBaseData$Trickers)}")


      hrr<-hot_to_r(input$portfolioGroups)
      rv$dataBaseData$Groups<-hrr
      saveDB(rv$dataBaseData)

      pf <-  hot_to_r(input$portfolioGroups)
      NamePf <- pf[input$portfolioGroups_select$select$r, 1]
      message(paste0( "Päivitetään salkkuryhmä ", NamePf) )

      pfs <- pf |> filter( Name == NamePf)
      portf <- levels(factor(pfs$Portfolio))


      for( pf in portf){
      waiter_show( # show the waiter
        html = spin_fading_circles() # use a spinner
      )


      tryCatch({


        portfolioValues(pf, updateStock=TRUE)
        db <- readDB()

        rv$dataBaseData <- readDB()



      }, error=function(e){
        showModal(modalDialog(
          title = "Error!",
          paste0("Salkun päivittämisssä tapahtui virhe ",e$message),
        ))
      }

      )
      waiter_hide()
      } # for( pf in pfs)




    })


    observeEvent(input$hideStockData, {
      log_info("observeEvent(input$hideStockData {colnames(rv$dataBaseData$Trickers)}")

      df<-data.frame(
        Kuvaus = c("Ei dataa")
      )
        output$stockData <- renderRHandsontable({rhandsontable(df)})


    })


      # output$DownloadStockToExcel <- downloadHandler(
      #   filename <- function() {
      #     paste("output", "zip", sep=".")
      #   },
      #
      #   content <- function(fname) {
      #     fs <- c()
      #     tmpdir <- tempdir()
      #     setwd(tempdir())
      #     for (i in c(1,2,3,4,5)) {
      #       path <- paste0("sample_", i, ".csv")
      #       fs <- c(fs, path)
      #       write(i*2, path)
      #     }
      #     zip(zipfile=fname, files=fs)
      #   }
      # )




    # observeEvent(input$SaveToExcel, {
    #   browser()
    #   fileName <-file.choose(new = FALSE)
    #   browser()
    # })
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(data(), file)
    }

    output$SaveToExcel <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        rw <- input$stocks_select$select$r
        stName <- input$stocks_select$data[[rw]][[1]]
        paste0(str_replace_all(stName, "[.]", "_"), ".xlsx")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        ##########################
        rw <- input$stocks_select$select$r
        stName <- input$stocks_select$data[[rw]][[1]]
        std<-stockData |> filter( ticker==stName)

        write.xlsx(std, paste0(str_replace_all(stName, "[.]", "_"), ".xlsx"))
      }
    )

    observeEvent(input$ShowStockData, {
      log_info("observeEvent(input$ShowStockData {colnames(rv$dataBaseData$Trickers)}")
      #browser()
      ##########################
      # Piirrä uioutput
      output$ExportStock <- renderUI({
        tagList(

          #fileInput("DownloadStockToExcel", "Lataa osake Exceliin", accept = ".xls"),
          sliderInput("n", "N", 1, 1000, 500),
          #actionBttn(session$ns("SaveToExcel"), label="Tiedot Exceliin"),
          downloadButton(session$ns("SaveToExcel"), "Download"),
          textInput("label", "Label")
        )
      })


      rw <- input$stocks_select$select$r
      if(!is.null(rw)){
        stName <- input$stocks_select$data[[rw]][[1]]
        # mydb <- getOption("StockDataFile")
        # load(mydb) # stockData

        std<-stockData |> filter( ticker==stName)
        #output$stockData <- renderRHandsontable({rhandsontable(std)})

        output$stockData2 <- DT::renderDataTable({ datatable(std) })
          # datatable(std)|>
          #   formatRound( columns = "ret_adjusted_prices", digits=4) |>
          #   formatRound( columns = "price_open", digits=4) |>
          #   formatRound( columns = "price_high", digits=4) |>
          #   formatRound( columns = "price_low", digits=4) |>
          #   formatRound( columns = "price_close", digits=4) |>
          #   formatRound( columns = "price_adjusted", digits=4) |>
          #   formatRound( columns = "ret_closing_prices", digits=4) |>
          #   formatRound( columns = "cumret_adjusted_prices", digits=4)
          #
          # })

      #  })

        #output$stockData <- renderTable(std)
      }

    })

    observeEvent(input$ShowPortfolioReturn, {
      log_info("observeEvent(input$ShowPortfolioReturn, {colnames(rv$dataBaseData$Trickers)}")

      tryCatch({

      if(!is.null(input$portfolio_select$select$r)){
        hrr<-hot_to_r(input$portfolio)
        Name <- hrr[input$portfolio_select$select$r, 1]
        fi <- paste( getOption( "PortfolioResults"), Name, ".RData", sep="")
        ti <- load(fi)
        pfResult <- get(ti)

        pf <- pfResult |> dplyr::mutate(date = format( as.Date(date), format = "%d.%m%.%Y"))
        #browser()

      output$showPortfolioResult2<- DT::renderDataTable({
        datatable( pf)})



      }
      }, error=function(e){
        showModal(modalDialog(
          title = "Error!",
          paste0("Salkkutulosta ei löytynyt ",e$message),
        ))



      }
      )
      })

     output$stocks <- renderRHandsontable({
       log_info("output$stocks <- renderRHandsontable {colnames(rv$dataBaseData$Trickers)} date = {rv$dataBaseData$Trickers$FirstData[1]}")
       #browser()

       rhandsontable(rv$dataBaseData$Trickers, useTypes = FALSE, selectCallback = TRUE, readOnly = FALSE) |>
         hot_col("FirstData", readOnly = TRUE) |>
         hot_col("LastUpdate", readOnly = TRUE)
       #log_info("after output$stocks <- renderRHandsontable {colnames(rv$dataBaseData$Trickers)}")

     })


     output$portfolio <- renderRHandsontable({
       log_info("output$portfolio <- renderRHandsontable(")

       rhandsontable(rv$dataBaseData$Portfolio, selectCallback = TRUE)
     })


     output$portfolioGroups <-  renderRHandsontable({
       log_info("output$portfolioGroups <-  renderRHandsontable date = {rv$dataBaseData$Trickers$FirstData[1]}")
       #browser()
       #rv$dataBaseData$
       # grps <- rv$dataBaseData$Groups$Name
       # grpsName<-unique(grps)
       # dgr <- data.frame(Ryhmat=grpsName)
       #dataBaseData$Portfolio$Name
       pfn<- rv$dataBaseData$Portfolio$Name
       #browser()
        rhandsontable(rv$dataBaseData$Groups, selectCallback = TRUE, height = 300, width = 550) %>%
          hot_col(col="Portfolio", type="dropdown", source=pfn)
#browser()

       #
       # rhandsontable(dff, width = 700, height = 300) %>%
       #   hot_col(col="Portfolio", type="dropdown", source=pfn)
       #renderRHandsontable({rhandsontable(MyChanges())})
     })

#
#      observeEvent(
#        input$UpdatePortfolioGroupdata,{
#
#
#          hrr<-hot_to_r(input$portfolioGroups)
#          colnames(hrr)=c("Name")
#          dd<-data.frame(Name=hrr, Portfolio=NA)
#          for( i in 1:nrow(rv$dataBaseData$Groups)){
#            filter(rv$dataBaseData$Groups, Name)
#          }
#
#          rv$dataBaseData$Groups<-dd
#        })





     output$portfolioGroupsData <-  renderRHandsontable({
       log_info("output$portfolioGroupsData <-  renderRHandsontable(")
       #browser()

       if(!is.null(input$portfolioGroups_select$select$r)){

         pfName <- input$portfolioGroups_select$data[[
           input$portfolioGroups_select$select$r]][[1]]
         pfName <- rv$dataBaseData$Groups[input$portfolioGroups_select$select$r,1]
         rv$dataBaseData$Groups[rv$dataBaseData$Groups$Name==pfName]
         dff <- filter( rv$dataBaseData$Groups, rv$dataBaseData$Groups$Name==pfName)
         pfn <- unique(rv$dataBaseData$Portfolio$Name)


         rhandsontable(dff, width = 700, height = 300) %>%
           hot_col(col="Portfolio", type="dropdown", source=pfn)


       }

     })


     portfolioChange <- reactive({
       log_info("portfolioChange <- reactive")
       # Name <- pf[input$portfolio_select$select$r, 1]
       if(!is.null(input$portfolio_select$select$r)){
         hrr<-hot_to_r(input$portfolio)
         Name <- hrr[input$portfolio_select$select$r, 1]

         #rv$dataBaseData$Portfolios$date <- as.Date(as.numeric(rv$dataBaseData$Portfolios$date))
         ss <- filter( rv$dataBaseData$Portfolios, PortfolioName==Name)
         dff<-data.frame(
           PortfolioName =c(Name),
           date          = format(Sys.Date(), getOption("DateFormat")),
           MoneyTransfer= c(""),
           Tricker = "",
           Qnt = 0,
           Price = 0 )
         if( nrow(ss)==0) ss <- dff

         stData <-hot_to_r(input$stocks)
         #%>%
         # hot_col("date", dateFormat = getOption("DateFormat"), type="date")

         st<-stData[['Tricker']]
         st<-append(st,"")
         st<-sort(st, decreasing = FALSE)
         #browser()


         #ss$date <- format(ss$date, getOption("DateFormat"))
         #ss$date<-as.Date(ss$date, format="%d.%m.%Y")
         return (ss)
         # rhandsontable(ss) |>
         #   hot_col(col="Tricker", type="dropdown", source=st) |>
         #   hot_col("PortfolioName", readOnly = TRUE) |>
         #   hot_col("date", dateFormat = 'DD.MM.YYYY', type = "date")
         #
         #


       }else return(NULL)
     })


      observeEvent(input$portfolio_select, {

        log_info("observeEvent(input$portfolio_select")


           hrr<-hot_to_r(input$portfolio)
           Name <- hrr[input$portfolio_select$select$r, 1]

             #rv$dataBaseData$Portfolios$date <- as.Date(as.numeric(rv$dataBaseData$Portfolios$date))
              ss <- filter( rv$dataBaseData$Portfolios, PortfolioName==Name)
              dff<-data.frame(
                PortfolioName =c(Name),
                date          = format(Sys.Date(), getOption("DateFormat")),
                MoneyTransfer= c(""),
                Tricker = "",
                Qnt = 0,
                Price = 0,
                CloseVal = 0,
                Total = 0)
              if( nrow(ss)==0) {
                ss <- dff
              }
              #browser()

        stData <-hot_to_r(input$stocks)

              rv$editablePortfolio <- ss

        st<-stData[['Tricker']]
        st<-append(st,"")
        st<-sort(st, decreasing = FALSE)

      })


      # emptyPF<-data.frame(
      #   PortfolioName =c(""),
      #   date          = format(Sys.Date(), getOption("DateFormat")),
      #   MoneyTransfer= c(""),
      #   Tricker = "",
      #   Qnt = 0,
      #   Price = 0,
      #   CloseVal = 0,
      #   Total = 0)
      # previous <- reactive({emptyPF})
      #
      # MyChanges <- reactive({
      #
      #   log_info("MyChanges <- reactive")
      #
      #   r <- input$portfolio_select$select$r
      #
      #
      #   pfolioList <- hot_to_r(input$portfolio)
      #   pfNa <- pfolioList$Name[r]
      #   tbl <- input$changePortfolio
      #
      #   pr <- previous()
      #   pr$PortfolioName <- pfNa
      #   if( is.null(tbl)) {
      #     return (pr)
      #   }
      #
      #
      #   mytable <- as.data.frame(hot_to_r(input$changePortfolio))
      #   if( is.na(mytable$PortfolioName[1])){
      #     mytable$PortfolioName <- pfNa
      #   }
      #   if( mytable$PortfolioName[1] != pfNa) {
      #     mytable<-rv$editablePortfolio
      #   }
      #   mytable$PortfolioName <- pfNa
      #   mytable$Total <- mytable$Qnt * mytable$Price
      #
      #
      #   for( i in 1:nrow(mytable) ){
      #     mytbl <- mytable[i,]
      #     ddd<-as.Date( mytbl$date, getOption("DateFormat"))
      #     qq<- filter(stockData, ref_date == ddd & ticker == mytbl$Tricker )
      #     if( nrow(qq)==1){
      #       #print(paste0( "Ticker == ", mytbl$Tricker ))
      #       mytable$CloseVal[i] <- qq$price_close[1]
      #     }
      #     i <- i+1
      #   }
      #
      #
      #   mytable
      # })


     output$changePortfolio <-  renderRHandsontable({
       log_info("output$changePortfolio <-  renderRHandsontable(")
      # browser()
       #################
        # if(!is.null(input$portfolio_select$select$r)){
        #
        #   hrr<-hot_to_r(input$portfolio)
        #   Name <- hrr[input$portfolio_select$select$r, 1]
        #
        #     #rv$dataBaseData$Portfolios$date <- as.Date(as.numeric(rv$dataBaseData$Portfolios$date))
        #      ss <- filter( rv$dataBaseData$Portfolios, PortfolioName==Name)
        #      dff<-data.frame(
        #        PortfolioName =c(Name),
        #        date          = format(Sys.Date(), getOption("DateFormat")),
        #        MoneyTransfer= c(""),
        #        Tricker = "",
        #        Qnt = 0,
        #        Price = 0 )
        #      if( nrow(ss)==0) ss <- dff
        #
             stData <-hot_to_r(input$stocks)
            #browser()
             #if( !is.null( stData)) rv$dataBaseData$Trickers <- stData
        #      #%>%
        #       # hot_col("date", dateFormat = getOption("DateFormat"), type="date")
        #

         st<-stData[['Tricker']]
              st<-append(st,"")
              st<-sort(st, decreasing = FALSE)
        #
        #

              if(!is.null(rv$editablePortfolio)) {
                # vval <- MyChanges()
                # vval$PortfolioName[1]
                #browser()
                r <- input$portfolio_select$select$r
                pfolioList <- hot_to_r(input$portfolio)
                pfNa <- pfolioList$Name[r]
                ss <- rv$dataBaseData$Portfolios |> filter( PortfolioName==pfNa)
                if(nrow(ss)==0 & !is.na(pfNa)){
                  sss <- add_row(rv$dataBaseData$Portfolios)
                  sss$PortfolioName[nrow(sss)] <- pfNa
                  rv$dataBaseData$Portfolios <- sss
                  ss <- rv$dataBaseData$Portfolios |> filter( PortfolioName==pfNa)
                }

                #browser()

                rhandsontable(ss) |>
                 hot_col(col="Tricker", type="dropdown", source=st) |>
                 hot_col("PortfolioName", readOnly = TRUE) |>
                  hot_col("date", dateFormat = 'DD.MM.YYYY', type = "date") |>
                  hot_col("CloseVal",  readOnly = TRUE) |>
                  hot_col("Total",  readOnly = TRUE)
              }
        #
        #
        #
        #
        #      }
              #browser()
       #ss <- portfolioChange()
       #rv$editablePortfolio<-
              #rhandsontable(MyChanges())
              # DF = data.frame(integer = 1:10,
              #                 numeric = rnorm(10),
              #                 logical = rep(TRUE, 10),
              #                 character = LETTERS[1:10],
              #                 factor = factor(letters[1:10], levels = letters[10:1],
              #                                 ordered = TRUE),
              #                 factor_allow = factor(letters[1:10], levels = letters[10:1],
              #                                       ordered = TRUE),
              #                 date = seq(from = Sys.Date(), by = "days", length.out = 10),
              #                 stringsAsFactors = FALSE)
              #
              # rhandsontable(DF, width = 600, height = 300) %>%
              #   hot_col("factor_allow", allowInvalid = TRUE)
 #
 # if(!is.null(rv$editablePortfolio))  {
 #              rhandsontable(rv$editablePortfolio) |>
 #               hot_col(col="Tricker", type="dropdown", source=st) |>
 #               hot_col("PortfolioName", readOnly = TRUE) |>
 #               hot_col("date", dateFormat = 'DD.MM.YYYY', type = "date") |>
 #               hot_col("CloseVal",readOnly = TRUE )
 #
 #
 #    }
     }


)

     # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
     # output$showPortfolioResult2 <- renderDataTable({
     #   log_info("output$showPortfolioResult2 <- renderDataTable")
     #   hrr<-hot_to_r(input$portfolio)
     #   Name <- hrr[input$portfolio_select$select$r, 1]
     #   if( is.null(input$portfolio_select$select$r)){
     #
     #     df <- data.frame( Index = c(NULL),
     #                       Gross.Value = c(NULL),
     #                       Period.Realized.PL = c(NULL),
     #                       Total = c(NULL)
     #     )}else{
     #
     #       mydb <- paste("Data\\", Name, ".RData", sep="")
     #
     #
     #
     #       if( !file.exists(mydb)){
     #         df <- data.frame( Index = c(NULL),
     #                           Gross.Value = c(NULL),
     #                           Period.Realized.PL = c(NULL),
     #                           Total = c(NULL))
     #
     #       }else{
     #         na<-load(mydb)
     #         df<-get(na)
     #       }
     #
     #
     #
     #     }
     #   #browser()
     #   df
     # })

     output$showPortfolioResult  <-  renderRHandsontable({
       log_info("output$showPortfolioResult  <-  renderRHandsontable")
       #rvv$updateList
       #browser()
       hrr<-hot_to_r(input$portfolio)
       Name <- hrr[input$portfolio_select$select$r, 1]
       if( is.null(input$portfolio_select$select$r)){

         df <- data.frame( Index = c(NULL),
                     Gross.Value = c(NULL),
                     Period.Realized.PL = c(NULL),
                     Total = c(NULL)
         )}else{

           mydb <- paste("Data\\", Name, ".RData", sep="")



           if( !file.exists(mydb)){
             df <- data.frame( Index = c(NULL),
                               Gross.Value = c(NULL),
                               Period.Realized.PL = c(NULL),
                               Total = c(NULL))

           }else{
             na<-load(mydb)
             df<-get(na)
           }



         }
        #browser()
       rhandsontable(df)

       }
     )



     output$selected=renderPrint({
       log_info("output$selected=renderPrin")
       cat('\nSelected Row after control:',input$table_select$select$r)
         }
       )



     observeEvent(

       input$portfolio$changes$changes, # observe if any changes to the cells of the rhandontable
       {
         log_info("input$portfolio$changes$changes")
        # browser()
       })



     observeEvent(input$UpdatePortfolio,{
       #browser()
       log_info("observeEvent(input$UpdatePortfolio")
       hr<-hot_to_r(input$portfolio)
       #WriteToDb(database="Portfolio", data=hr)
     })

     # observeEvent(input$UpdatePortfolioData,{
     #
     #   pfName <- input$portfolio_select$data[[
     #     input$portfolio_select$select$r]][[1]]
     #   hr<-hot_to_r(input$changePortfolio)
     #   hr$PortfolioName = pfName
     #   WriteToDbPortfolios(data=hr, pfName)
     # })

     observeEvent(input$UpdateTrickers, {
       log_info("observeEvent(input$UpdateTrickers")
       hr<-hot_to_r(input$stocks)
       #WriteToDb(database="Trickers", data=hr)
     })

     observeEvent(input$update, {
       log_info("observeEvent(input$update")
       #updatePortfolios()

       #log_setting()

       shinyjs::disable('update')
       #portfolioValues()

       shinyjs::enable('update')
     })

  }
  )
}
