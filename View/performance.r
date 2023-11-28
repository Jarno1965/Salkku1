
library(shiny)
library(beepr)
library(blotter)
library(tidyquant)
library(quantmod)
library(yfR)
library(shinyjs)
library(tidyverse)
library(logger)
library(timetk)
library(xlsx)
library(data.table)
library(R.oo)
library(rep)
# source("r\\Database.r")
#source("r\\bl.r")




##########################################################
updateStockDatas <- function(dataBaseData, updateStocks=NULL, updateStocName=NULL){
#browser()
  if(is.null( dataBaseData ))
    dataBaseData <- readDB()

if( is.null(updateStocName)){
  updateStocName <- dataBaseData$Trickers$Tricker
}

  log_info("updateStockDatas <- function(dataBaseData, updateStocks=NULL)")


  #dataBaseData <- rv$dataBaseData

   log_info("Load stock courses")
   dd<-NULL

   mydb <- getOption("StockDataFile")
   dataf<- load(mydb) # stockData
   stockData <- get(dataf)

   #browser()
   # Poistetaan kaikki käsiteltävät tickerit (korjataan kanta)
   stockData <- stockData |> filter( !(ticker %in% updateStocName))
#browser()



   #stockData <- stockData |> filter( !(ticker %in% updateStocName & ref_date > today()-10) )

   # otetaan muutama pois jotta päästään testaamaan
  # stockData <- stockData|>filter(ref_date<"2023-03-03")

if( is.null(updateStocName)){
     tricker <- dataBaseData$Trickers[['Tricker']]
} else{
  tricker <- updateStocName[updateStocName!=""]
}


   dfff<-NULL


for(tr in tricker){
     #rv$showStockUpdate <- paste( "Käsittelee osketietoja ", tr, "aikavälillä ")

     if( !is.null(stockData)) {
        dd <- stockData|>filter( ticker == tr)
        } else{
          dd=NULL
          firstDate <- as.Date("2000-01-01")
        }


if( !is.null(dd)){
     if(nrow(dd)>0 ){
       las <- dd|>filter( ref_date == max(ref_date))

       firstDate <- pull(las[1, 'ref_date'])+1
     }else {
       firstDate <- as.Date("2000-01-01")
     }
}
     lastDate <- Sys.Date()
     if( lastDate - firstDate<10) {
       firstDate <- lastDate-10
       stockData <- stockData |> subset((ref_date < firstDate | ticker != tr ))
     }

     df_yf<-NULL

     if(lastDate>firstDate){
       message(paste("Ladataan ", tr, "lähtien: ", firstDate, "saakka ", lastDate))
       df_yf <- yf_get(tickers = tr,
                       first_date = firstDate,
                       last_date = lastDate,
                       thresh_bad_data = 0)
     }

     dfff<-rbind(dfff, df_yf )
   }

# browser()
if( !is.null(dfff))
{
dfff$div = 0
df_yf <- rbind(stockData, dfff)
}else
  df_yf <- stockData



  for (tr in tricker){

    fdd <- df_yf |> filter( ticker  == tr)
    fd <- fdd |> filter( ref_date == min(ref_date))
    if( nrow(fd)==1)
    {
      firstDate <- format( pull(fd[1,'ref_date']), getOption("DateFormat"))
    } else{
      firstDate <- NA}

    ld <- df_yf |> filter( ticker  == tr & ref_date == max(ref_date))
    if( nrow(ld)==1)
    {
      lastDate <- format( pull(ld[1,'ref_date']), getOption("DateFormat"))
    } else{
      lastDate <- NA}

  if( is.null(  updateStocName)){
    dataBaseData$Trickers[dataBaseData$Trickers$Tricker==tr,]$FirstData<-as.Date(firstDate, format = getOption("DateFormat") )
    dataBaseData$Trickers[dataBaseData$Trickers$Tricker==tr,]$LastUpdate<-as.Date(lastDate, format = getOption("DateFormat") )
  }

      }

#browser()
  currAndStocks<- readStockAndCurrencies()
  currAndStocks <- updateCurrencies(currAndStocks)
  # dataBaseData$Trickers on sama kuin StockInfo

  dd<-NULL
  firstDate<-"2000-01-01"
  for (tr in tricker){
    log_info("Lisätään osinkoja {tr}")
    print( paste0("Lisätään osinkoja ", tr))

    tryCatch({
      div <- yf_get_dividends(ticker=tr, first_date =firstDate )
      log_info("Löysi osinkotietoja {tr} ja niitä oli {nrow(tr)} kpl")
      ########################

      StockInfo <- dataBaseData$Trickers
      # Check should we change dividends currency to Euros
      si <- dataBaseData$Trickers |> filter( Tricker==tr)
      if( !is.na(si$OsingonValuutta) & si$OsingonValuutta!="" & si$OsingonValuutta!="EUR"){
        for( i in 1:nrow(div)){
          currRate <- currAndStocks$currency |> filter(date == div[i,]$ref_date & currAndStocks$currency$currency==si$OsingonValuutta)
          if( nrow( currRate)>0){
            div[i,]$dividend <- div[i,]$dividend*currRate$rate[1]
          }
        }
      }
      #######################

      dd<- rbind(dd, div)
    }, error=function(e){
      print(paste("Ei löytynyt osinkoja ", tr))
    })
  }

div<-dd
df<- df_yf





if( !is.null(div)){
    if( nrow(div)>0 ){
      for( ii in 1:nrow(div)){


        df[df['ref_date']==toString(pull(div[ii,'ref_date'])) & df['ticker']==pull(div[ii,'ticker']),]$div=
          as.numeric(div[ii, 'dividend'])

      }
    }
}





dff<-df |> group_by(ticker) |> mutate(DivSum = cumsum(div))
dff <- dff |> mutate( price_adjusted_close = price_close+DivSum)

buildAll=NULL
for( tr in tricker){
  print( paste0("Lasketaan tuottoja ", tr) )
  dffTr <- dff|>filter(ticker==tr)

  ret <- Return.calculate(xts(x=dffTr$price_adjusted_close, order.by=dffTr$ref_date))

cr <- coredata(ret)

dffTr$ret_closing_adj_prices <- cr[,1]

dffTr$ret_cumulative <- cumprod(ifelse(is.na(cr), 1, cr+1))



buildAll<- rbind(buildAll, dffTr )

}


lev <- levels(factor(buildAll$ticker))
if( !is.null(stockData)) {
  stockData <- stockData |> filter(!(ticker %in% lev))
}

stockData <- rbind(stockData, buildAll)


mintr <- stockData|>group_by(ticker)|>  filter(ref_date==min(ref_date))

maxtr <- stockData|>group_by(ticker)|>  filter(ref_date==max(ref_date))

for( tr in tricker){
  print( paste0("Päivitetään päivitystiedot ", tr) )
  dataBaseData$Tricker |> subset()
  nn <- dataBaseData$Trickers |> filter( Tricker== tr)
  if( nrow(nn)==0){
    dt <- add_row(dataBaseData$Trickers)
    dt[nrow(dt),]$Tricker <- tr
    dataBaseData$Trickers <- dt
  }

  dataBaseData$Trickers[dataBaseData$Trickers$Tricker==tr,]$FirstData =
    format(mintr$ref_date[mintr$ticker==tr], "%d.%m.%Y")

  dataBaseData$Trickers[dataBaseData$Trickers$Tricker==tr,]$LastUpdate =
    format(maxtr$ref_date[maxtr$ticker==tr], "%d.%m.%Y")

}


print( "Säilötään levylle tiedot ")
save( stockData, file =mydb)
print( "On säilötty tiedot ")
#browser()
saveDB(dataBaseData)
  return(dataBaseData)


  }




trickerPortf<- function(PFData, PortfolioName){
  PortfolioName<-"Unelmat"
  tt<-subset(PFData, Salkun_nimi==PortfolioName)
  tt<-unique(tt['Trikkeri'])
  tt<-tt[!is.na(tt)]
  tt<-str_replace(tt, "\\^", "")
  return(tt)

}





####################################################################
#
# buildTrades <- function(PFData, pfname)
# buildTrades(PortfolioData, PFname=PortfolioName)
#
####################################################################
buildTrades <- function(PFname, PFData=NULL){
 log_info("buildTrades <- function(PFData, {PFname})")


SalkkuNimi<-PFname

  stockAndCurr <- readStockAndCurrencies()


pfn<-paste("portfolio",SalkkuNimi,sep='.')
  require(FinancialInstrument)


  try(rm(".instrument"), silent=TRUE)
  if(!exists(".instrument")) .instrument <<- new.env()
  if(!exists(".blotter")) .blotter <<- new.env(hash=TRUE)
ll<-ls(.blotter)
rm(list=ll, envir=.blotter)

  df <- data.frame(c("Date", "Symbol", "TxnPrice", "TxnQty"))

tricker <- levels(factor(unlist(PFData$Tricker)))
tricker <- str_replace(tricker, "\\^", "")
tricker <- tricker[tricker!=""]

  if(length( tricker)==0 ){

    return( throw(paste0( "Salkussa ", PFname," ei ole määritelty yhtään osakekauppaa!")))
  }
  currency("EUR")


  stock( primary_id=tricker, currency="EUR", multiplier=1)
  frstTr <- get(tricker[1])
  indTr <- index(frstTr)

  lastDate <- indTr[length(indTr)]
  try(rm(Trades), silent = TRUE)
  try(rm(MoneyTransfer), silent = TRUE)

  pfn<-paste("portfolio.", "PORTF", sep="")
  accn<-paste("account.", "PORTF", sep="")

  dt = PFData['date']

  tra <- PFData|>filter(Tricker!="" & !is.na(Tricker))
  firstTrade <- sort(tra$date)[1]

  firstDate <- dt[1,1]



  amou<-as.numeric(PFData[,'MoneyTransfer'])
  amou<-amou[!is.na(amou) & amou!=0]
  # if( length(amou)==0){
  #   return( throw('Ei ole määritelty kassan suuruutta'))
  #   }
  initPortf("PORTF",symbols=tricker,initDate=firstDate-1)
  initAcct("PORTF", portfolios="PORTF", initEq = 0, initDate=firstDate-1)
  if( length(amou)!=0){
     for( i in 1:nrow(PFData)){
        # check money transfer

        if(!is.na( PFData[i, 'MoneyTransfer']) & PFData[i, 'MoneyTransfer']!="" ){

          if( PFData[i, 'date'] > firstTrade){
            da <- PFData[i, 'date']
          }else{
            da <- firstTrade
          }

          addAcctTxn(Account="PORTF", TxnDate=da,
                     TxnType = c("Additions"), Amount=as.numeric(PFData[i,'MoneyTransfer']),
                     verbose=FALSE)
          }
     }
  }


  for( i in 1:nrow(PFData)){
    if(!is.na( PFData[i, 'Tricker']) & PFData[i, 'Tricker']!="" ){

      addTxn(Portfolio = "PORTF",
             Symbol = str_replace(PFData[i, 'Tricker'], "\\^", ""),
             TxnDate = PFData[i, 'date'],
             TxnQty = PFData[i, 'Qnt'],
             TxnPrice = PFData[i, 'Price'],
             verbose=FALSE)

    }
  }


    for( i in 1:length(tricker)){

      tr <- tricker[i]
      tr <- str_replace( tr, "\\^", "")
      stD <-get(tr)
      cna <- colnames(stD)

      cna <-str_remove(cna, paste(tr, ".", sep=""))
      colnames(stD)<-cna

      divid<-stD[!is.na(stD$div)]
      dividDF <- data.frame(date=index(divid), coredata(divid))
      dividDF <- filter( dividDF, date>firstDate)
      if(nrow(dividDF)>0 ){
        for( ii in 1: nrow(dividDF)){
          div <- dividDF[ii, 'div']
          da <- dividDF[ii, 'date']

          # Check currency
          curr <- filter( stockAndCurr$stocks, stockAndCurr$stocks$Tricker==tr)['OsingonValuutta']
          multiple<-1
          # if( curr[[1]]!="" & !is.na(curr[[1]])){
          #   mult <- filter( stockAndCurr$currency, stockAndCurr$currency$currency==curr[[1]] &
          #             stockAndCurr$currency$date==da)
          #   if( nrow( mult)==1)
          #     multiple<-mult$rate
          # }
          posQ <- getPosQty(Portfolio = "PORTF" , Symbol = tr , da )
          if( div*multiple*posQ != 0){
            addDiv(Portfolio = "PORTF", TxnDate = da, Symbol=tr, DivPerShare=div*multiple )
            log_info(
              "Lisää {tr} osinko päivä={da} DivPerShare= = {div*multiple} osakkeita = {posQ} total={div*multiple*posQ} ")
          }
        }
      }


    }

#firstDate<-firstDate-1
  dte<- paste(firstDate, lastDate, sep="/")
  updatePortf("PORTF", Dates = dte)
  updateAcct("PORTF", Dates = dte)
  updateEndEq("PORTF", Dates = dte)


  ##################################################################
  p = getPortfolio("PORTF")
  a = getAccount("PORTF")
tra<-NULL

for( tr in tricker){
  trades<- getTxns(Portfolio<-"PORTF", Symbol<-tr, Dates<-dte)
  trades$tckr <- " "
  trades$tckr <- tr
  tra<-rbind(tra, trades)
}

tra<- data.frame(date=index(tra), coredata(tra))

tra$date <- as.Date(tra$date)
tra$Txn.Qty<- as.numeric(tra$Txn.Qty)
tra$Txn.Price<- as.numeric(tra$Txn.Price)
tra$Txn.Value<- as.numeric(tra$Txn.Value)
tra$Net.Txn.Realized.PL<- as.numeric(tra$Net.Txn.Realized.PL)
tra$Txn.Avg.Cost <- as.numeric(tra$Txn.Avg.Cost)

trs <- levels(factor(tra$tckr))
da <- "2023-11-1"


tra <- add_row(tra)

for( tr in trs){
  dat <- get(tr)
  ld <- levels(factor(index(dat)))
  if( length(ld) != nrow(dat)){
    print(tr)
    browser()
  }
  #browser()
  cd <- coredata(dat)
  df <- data.frame( Date = index(dat), Close = cd[,4] )

  dff <- df |> filter( Date-as.Date(  da)  == min(abs(Date-as.Date(  da)))      )

  cor_ddd <- coredata(ddd)
  bu <- tra |> filter(tckr == tr)
  cs <- cumsum(bu$Txn.Qty)

  tra <- add_row(tra)
  tra$Txn.Qty[nrow(tra)] <- cs[length(cs)]
  tra$date[nrow(tra)] <- dff$Date[1]
  tra$Txn.Price[nrow(tra)] <- dff$Close
  tra$tckr[nrow(tra)] <- tr


}

ppp<- paste0("Data\\", SalkkuNimi, "_Trades.xlsx")
write.xlsx(tra, ppp)
statistic <- dailyStats("PORTF")

removePortfolioStat(PFname)

writeStateToDB(PFname, statistic)
#browser()
  mydb<-paste0( "Data\\", PFname, ".RData" )
  mydbComp<-paste0( "Data\\", PFname, "_comp", ".RData" )

  pfolio  <- mcTT


  pfolio <- data.frame(date=index(a$summary), coredata(a$summary))



  #pfolio <- pfolio %>% filter( date > firstDate)

  pfolio <- pfolio |> filter( date>=firstDate)
  #filter( pfolio, index(pfolio) > firstDate)
  #browser()
  #date1 <- "2023-10-10"
  cpw <- calcPortfWgt(Account="PORTF", Portfolio="PORTF", denominator = c("Gross.Value", "Net.Value"))
  #write.xlsx(cpw, "f:\\tmp\\ddd.xlsx")

  save(pfolio, file=mydb)
  save(cpw, file=mydbComp)
  }

##########################################################
portfolioValues <- function(PortfolioName, updateStock=FALSE){
  #PortfolioName <- "HOSKL Unelmat"

  log_info("portfolioValues <- function({PortfolioName}, updateStock = {updateStock}) ")
  PortfolioData <- LoadPortfolioData(PortfolioName)
  if(updateStock==TRUE){
    stocks <- levels(factor( PortfolioData$Tricker))
    stocks[!is.na(stocks) & stocks != ""]

    updateStockDatas(dataBaseData=NULL, updateStocks=NULL, updateStocName=stocks)
    # Päivitä osakkeet

  }

  mydb <- getOption("StockDataFile")
  load(mydb) # stockData

    tr <- unique(PortfolioData['Tricker'])
    tricker <- tr[!is.na(tr)]

    tricker<-tricker[tricker!=""]
    if( length(tricker) == 0 ){
      return(  throw("Ei ole määritelty yhtään osakekauppaa! "))

    }
    PortfolioData$date = as.Date(PortfolioData$date, format=getOption("DateFormat"))
    dates<-arrange(PortfolioData['date'])
    firstDate <- dates[1,1]
    if( is.na(firstDate)){
      return( throw('Tarkista että jokaisella salkun tietorivillä on päivätieto!'))
    }



    lastDate <- Sys.Date()
    if( firstDate > lastDate){
      showModal(modalDialog(
        title = "Error!",
        paste0("Päiväykset salkkumäärittelyssä ovat tulevaisuudessa ")

      ))
      return(NULL)
    }



    #stockD <- stockData[-9:-11]
    stockD <- stockData[-13:-ncol(stockData)]

    cn <- c("ticker", "date", "Open","High","Low", "Close", "Volume", "Adjusted", "ret_adj", "ret_closing","cumret_ad", "div" )

    colnames( stockD) <- cn
  for(i in 1:length(tricker)) {


     tr <- tricker[i]
     tt<-subset(stockD, ticker==tr)
     tr <- str_remove(tr,  "\\^")
     tt <- tt[,-1]

     trr <- paste( tr, cn, sep=".")
     trr<-trr[-1]

     colnames(tt)<-trr
     tt<-tk_xts(tt)


      assign( tr, tt, envir = e)

  }

    buildTrades(PortfolioData, PFname=PortfolioName)


    return(1)



}






updatePortfolios<- function(){

}

performance_ui <- function(id) {
  log_info("performance_ui <- function(id)")
  ns <- NS(id)

  selectGroup<-0



  fluidPage(
    titlePanel("Salkut!"),
    sidebarLayout(
      sidebarPanel(
        #log_info("selectInput(....db {db$Groups$Name}"),
        #selectInput(ns("dg"), "Valitse salkkuryhmä ", choices = unique(db$Groups$Name)),
       # selectInput(ns("dg"), "Valitse salkkuryhmä ", choices = c("", "1", "2")),
        textInput(ns("NewGrp"), label="Uusi ryhmä"),
        selectizeInput(ns("dg"), "Valitse salkkuryhmä", selected = "UPM",
                       c("Cylinders" = "cyl",
                         "Transmission" = "am",
                         "Gears" = "gear")),
        selectInput(ns("SelectStocks"),"Valitse osakkeet",
                    c("bas.de", "upm.he"), multiple = TRUE),
        selectInput(ns("SelectPortfolioGroups"),"Valitse salkkuryhmät",
                    c("ryhmä1", "ryhmä2"), multiple = TRUE),
        selectInput(ns("SelectPortfolios"),"Valitse salkut",
                    c("portfolio1", "portfolio2"), multiple = TRUE),
        dateInput(ns("StartingDate"), label="Alkupäivä", format="dd.mm.yyyy", language="fi"),
        numericInput(ns("StartingValue"), "Alkuarvo", 100000),
        actionBttn(ns("Save"), "Tallenna tiedot")
      ),
      mainPanel(
        plotlyOutput(ns("PortfReturn"))
      )
    )
  )
}



returnPortfolios <- function(dbData, GroupName, stocks, StartingDate, StartingVal){
dff <- NULL

  if( GroupName == "" | is.na(GroupName)){ return(dff)}

pfNames <- dbData$Groups$Portfolio[dbData$Groups$Name==GroupName]
for(pfn in pfNames){
  datafile <- paste("Data\\", pfn,".RData", sep="" )
  load(datafile) # return pfolio
  pfolio$Portfolio = pfn
  dff <- rbind( dff, pfolio)
}


stocksD <- subset(dff, FALSE)

mydb <- getOption("StockDataFile")
stDfile<-load(mydb) # stockData
stcData <- get(stDfile)
sss <- NULL
for( st in stocks){
  stcD<- stcData |> filter( ticker == st & ref_date > StartingDate)
  stcD$cumret_adjusted_prices <- stcD$cumret_adjusted_prices/stcD$cumret_adjusted_prices[1]
  stcD$cumret_adjusted_prices <- stcD$cumret_adjusted_prices*StartingVal
  stcD <- stcD |> transmute( date = ref_date, Portfolio = st, End.Eq = cumret_adjusted_prices)
  sss <- rbind( sss, stcD)
}
colnames(dff)


  return(sss)
}

retGroups <- function(dgNames, AddedField){
  if( AddedField!="") dgNames<-append(dgNames, AddedField)

  return(dgNames)
}



performance_server <- function(id, xcat, ycat) {
  log_info("performance_server <- function(id, xcat, ycat)")
  moduleServer(id, function(input, output, session) {
    log_info("performance_server moduleServer(id, function(input, output, session)")
    db <- readDB()


    #performance <- reactiveVal({returnPortfolios(db, input$dg)})
    performance <- reactiveVal({returnPortfolios(db, "")})
#
#     selectInput(ns("SelectPortfolioGroups"),"Valitse salkkuryhmät",
#                 c("ryhmä1", "ryhmä2"), multiple = TRUE),
#     selectInput(ns("SelectPortfolios"),"Valitse salkut",
#                 c("portfolio1", "portfolio2"), multiple = TRUE),
    observeEvent(input$Save, {

      # pp<-input$SelectStocks
      # input$dg
      #Remove old groupdata and replace it the new one

      db$HandlingData <- db$HandlingData %>%
              filter(GroupName != input$dg )
      dfempty <- data.frame( GroupName = "",
                  DataType="",
                  Value="")
      df <- dff <- dfempty
      dff<-NULL


      for( st in input$SelectStocks){
        df$Value = st
        df$GroupName = input$dg
        df$DataType = "Stocks"
        dff <- rbind( dff, df)
      }

      df<- dfempty

      for( st in input$SelectPortfolioGroups) {
        df$Value = st
        df$GroupName = input$dg
        df$DataType = "Group"
        dff <- rbind( dff, df)
      }
      df<- dfempty

      for( st in input$SelectPortfolios) {
        df$Value = st
        df$GroupName = input$dg
        df$DataType = "Portfolios"
        dff <- rbind( dff, df)
      }
      df<- dfempty
      df$DataType = "StartingDay"
      df$Value = toString(input$StartingDate)
      df$GroupName = input$dg

      dff <- rbind( dff, df)
      df<- dfempty

      df$Value = toString(input$StartingValue)
      df$GroupName = input$dg
      df$DataType = "StartingValue"
      dff <- rbind( dff, df)

      # input$SelectPortfolios
      # browser()
      # df$Value <- input$SelectStocks
      # df$DataType <- "Stocks"
      # df$GroupName <- input$dg
      #
      # input$SelectPortfolioGroups
      # input$SelectPortfolios
      # input$StartingDate
      # input$StartingValue
      db$HandlingData <- dff
      saveDB(db)
    })


    observe ({

      updateSelectInput(session, "SelectStocks",
                        choices = db$Trickers$Tricker,
                        selected = filter(db$HandlingData, GroupName==input$dg & db$HandlingData$DataType=="Stocks" )$Value
      )
      updateSelectInput(session, "SelectPortfolioGroups",
                        choices = db$Groups$Name,
                        selected = filter(db$HandlingData, GroupName==input$dg & db$HandlingData$DataType=="Group" )$Value
      )
      updateSelectInput(session, "SelectPortfolios",
                        choices = db$Portfolios$PortfolioName,
                        selected = filter(db$HandlingData, GroupName==input$dg & db$HandlingData$DataType=="Portfolios" )$Value

      )

      updateDateInput(session, "StartingDate",
                      value = filter(db$HandlingData, GroupName==input$dg & db$HandlingData$DataType=="StartingDay" )$Value)

    })


    #performance <- returnPortfolios()
    # RV <- reactiveValues(
    #   db = db,
    #   retProf = performance)

    # portfolioResult <- reactive({returnPortfolios(input$dg)})



 #     rv <- reactiveValues( dataBaseData = db,
 #                           Groups = db$Groups
 # )
#
#     rv <- data_frame(
#             dataBaseData = db,
#             Groups = db$Groups
#           )
    observe ({
    updateSelectInput(session, "dg",

                      choices = retGroups(db$Groups$Name, input$NewGrp)



                      ) #db$Groups$Name
    #)
})


     output$PortfReturn <-
    # ##################################
    # output$PortfReturn2 <- renderEcharts4r({
     output$PortfReturn2 <- renderPlotly(

       plot1 <- plot_ly(
         x = x(),
         y = y(),
         type = 'scatter',
         mode = 'markers')
     )
    #
    #   perf <- returnPortfolios(db, input$dg, input$SelectStocks, input$StartingDate, input$StartingValue)
    #
    #   # Val <- portfolioResult()
    #   # browser()
    #   #if( !is.null(rv$portfolioRet)){
    #
    #
    #     valu <- min(perf$End.Eq)*0.95
    #     perf$date <- as.Date(perf$date)
    #     #perf |> group_by(Portfolio) |> e_charts(date) |> e_line(End.Eq)
    #     perf |> group_by(Portfolio) |> e_charts(date) |> e_line(End.Eq) |> e_datazoom() |>
    #          e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
    #          e_title(text = "HOSKn mallisalkut", left = "left") |>
    #          e_toolbox_feature("dataZoom") |> e_theme("chalk") |> e_animation(duration = 1000) |>
    #          e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
    #          e_grid(gridIndex = "1")|>
    #          e_y_axis(
    #            formatter = e_axis_formatter(style=c("currency"),
    #                                         digits=0,
    #                                         currency="EUR"
    #            ))
    #
    #
    #
    # })
    #

      observeEvent(input$update, {


      shinyjs::disable('update')
      portfolioValues()

      shinyjs::enable('update')
    })

  }
  )
}

x <- reactive({
  mtcars[,input$xcol]
})

y <- reactive({
  mtcars[,input$ycol]
})

