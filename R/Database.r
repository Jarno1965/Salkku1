
library(DBI)
library(RSQLite)
library(dplyr)
library(priceR)
library(lubridate)

writeCurrenciesToDB <- function(currData){
  log_info("writeCurrenciesToDB <- function(currData)")
  tryCatch({
    mydb <- dbConnect(RSQLite::SQLite(), getOption("DatabaseFile"))
    dbWriteTable(conn=mydb, name="Currency", value=currData, overwrite=TRUE)
    dbDisconnect(mydb)
  }
  , error=function(e){
    throw(paste0( "Error: Valuuttakursien tietokantaan kirjoituksessa! ", e))
  })


}


# removePortfolioStat(PFname)

writeStateToDB <- function(PFname, data) {
  tryCatch({
    mydb <- dbConnect(RSQLite::SQLite(), getOption("DatabaseFile"))
  }
  , error=function(e){
    throw(paste0( "Error: Valuuttakursien tietokantaan kirjoituksessa! ", e))
  })

  rn <- row.names(data)
  rn <- str_replace(rn, ".DailyEqPL", "")
  df <- data.frame( Portfolio = c("AP"),
              Trikker =rn,
              Description = "Kuvaus",
              Comment = "Kommentti",
              Value = 125.45)

  Tulos <- pull(data['Total.Net.Profit'])
  PlusPaivia <- pull(data['Winning.Days'])
  MinusPaivia <- pull(data['Losing.Days'])
  Keskimaarin <- pull(data['Avg.Day.PL'])
  Mediaani <- pull(data['Med.Day.PL'])
  SuurinVoitto <- pull(data['Largest.Winner'])
  SuurinTappio <- pull(data['Largest.Loser'])
  Keskihajonta <-  pull(data['Std.Dev.Daily.PL'])
  Sharpe <-  pull(data['Ann.Sharpe'])
  Drawdown <-  pull(data['Max.Drawdown'])

  dfff <- NULL

  for( i in 1: nrow(df)){

     dff <- data.frame(
       Portfolio = PFname,
       Trikker = df[i, 'Trikker'],
       Description = "Tulos",
       Comment = "Paljonko euroissa trikkeri on tuottanut",
       Value = Tulos[i]
     )
    dfff <- rbind( dfff, dff)
     dff <- data.frame(
       Portfolio = PFname,
       Trikker = df[i, 'Trikker'],
       Description = "Pluspäiviä",
       Comment = "Voitollisten päivien määrä",
       Value = PlusPaivia[i]
     )
     dfff <- rbind( dfff, dff)

     dff <- data.frame(
       Portfolio = PFname,
       Trikker = df[i, 'Trikker'],
       Description = "Tappiopäiviä",
       Comment = "Tappiollisten päivien määrä",
       Value = MinusPaivia[i]
     )
     dfff <- rbind( dfff, dff)

     dff <- data.frame(
       Portfolio = PFname,
       Trikker = df[i, 'Trikker'],
       Description = "Keskimääräinen tulos",
       Comment = "Paljonko päivässä on tullut tulosta",
       Value = Keskimaarin[i]
     )
     dfff <- rbind( dfff, dff)

     dff <- data.frame(
       Portfolio = PFname,
       Trikker = df[i, 'Trikker'],
       Description = "Mediaani tulos",
       Comment = "Tuloksen mediaani",
       Value = Mediaani[i]
     )
     dfff <- rbind( dfff, dff)

     dff <- data.frame(
       Portfolio = PFname,
       Trikker = df[i, 'Trikker'],
       Description = "Suurin päivävoitto",
       Comment = "Mitä tuotti paras päivä",
       Value = SuurinVoitto[i])

     dff <- data.frame(
         Portfolio = PFname,
         Trikker = df[i, 'Trikker'],
         Description = "Suurin päivätappio",
         Comment = "Mitä tuotti huonoin päivä",
         Value = SuurinTappio[i]
     )
     dfff <- rbind( dfff, dff)

     dff <- data.frame(
       Portfolio = PFname,
       Trikker = df[i, 'Trikker'],
       Description = "Tuloksen keskihajonta",
       Comment = "Tuloksen keskihajonta",
       Value = Keskihajonta[i]
     )
     dfff <- rbind( dfff, dff)

     dff <- data.frame(
       Portfolio = PFname,
       Trikker = df[i, 'Trikker'],
       Description = "Sharpen luku",
       Comment = "Suhdeluku millä verrataan tuottoa hajontaan",
       Value = Sharpe[i]
     )
     dfff <- rbind( dfff, dff)

     dff <- data.frame(
       Portfolio = PFname,
       Trikker = df[i, 'Trikker'],
       Description = "Maksimi drawdown",
       Comment = "Paljonko on suurin tappio",
       Value = Drawdown[i]
     )
     dfff <- rbind( dfff, dff)
  }
#browser()
  dbWriteTable(
    conn=mydb,
    name = "Stats",
    value = dfff,
    overwrite=FALSE,
    append=TRUE)

  dbDisconnect(mydb)
}



removePortfolioStat <- function(PFname){
#browser()
  tryCatch({
    mydb <- dbConnect(RSQLite::SQLite(), getOption("DatabaseFile"))
  }
  , error=function(e){
    throw(paste0( "Error: removePortfolioStat! ", e))


  })

  sq<- paste0( "DELETE FROM Stats WHERE Portfolio ='", PFname, "'")
  res <- dbSendQuery(mydb, sq)
  dbClearResult(res)
  dbDisconnect(mydb)
}


writeCurrenciesToDB <- function(currData){
 log_info("writeCurrenciesToDB <- function(currData)")
  tryCatch({
    mydb <- dbConnect(RSQLite::SQLite(), getOption("DatabaseFile"))
    dbWriteTable(conn=mydb, name="Currency", value=currData, overwrite=TRUE)
    dbDisconnect(mydb)
  }
  , error=function(e){
    throw(paste0( "Error: Valuuttakursien tietokantaan kirjoituksessa! ", e))
  })


}

readStockAndCurrencies <- function(){
  log_info("readStockAndCurrencies <- function()")
  tryCatch({
  mydb <- dbConnect(RSQLite::SQLite(), getOption("DatabaseFile"))
    stocks <-dbReadTable(mydb, "Trickers")
    currency <-dbReadTable(mydb, "Currency")
    currency['date'] <-
      currency$date <- as.Date(as.numeric(currency$date), origin = "1970-01-01")
  dbDisconnect(mydb)
  }
  , error=function(e){
   throw(paste("Error: Valuuttakurssine haussa! ", e))
  }
  )



 li <- list( stocks = stocks, currency = currency )
 return(li)
}

updataCurrencies <- function(){
  log_info("updataCurrencies <- function()")

  mydb <- dbConnect(RSQLite::SQLite(), getOption("DatabaseFile"))
    stocks <-dbReadTable(mydb, "Trickers")
    currency <-dbReadTable(mydb, "Currency")
    loadCurr <- stocks|>filter(OsingonValuutta  != "" & !is.na(OsingonValuutta) |
                                 KurssienValuutta  != "" & !is.na(KurssienValuutta))

    l1<- loadCurr['OsingonValuutta']
    l2<-loadCurr['KurssienValuutta']
    colnames(l1)<-"Currencies"
    colnames(l2)<-"Currencies"
    currencies <- rbind( l1, l2)
    currencies <- unique(currencies)
    currencies <- na.omit(currencies)
    dff <- NULL
    for( curr in currencies){
      cu <- historical_exchange_rates(from = curr, to="EUR",
                                      start_date ="2000-01-01", end_date = today() )
      cu$Currency = curr
      colnames(cu)<-c("date", "rate", "currency")
      dff<- rbind( dff, cu)

    }

    dbWriteTable(conn=mydb, name = "Currency", value = dff, overwrite=TRUE)
  dbDisconnect(mydb)

Sys.Date()
  SEK <- from_to_dates_rates("SEK", "EUR", dates = list("2000-01-01", today()))
}



saveDB<-function(dataBaseData){
  log_info("saveDB<-function(dataBaseData)")
  mydb <- dbConnect(RSQLite::SQLite(), getOption("DatabaseFile"))
  out <- tryCatch(
    {

  hdd <- dbReadTable(mydb, "HandlingData")

  # as.numeric(   dataBaseData$Portfolios[1, 'Date'])

  pfo <- dataBaseData$Portfolios
  pfo$date<-as.Date(pfo$date, getOption("DateFormat"))


  pfo$date <- as.numeric(pfo$date )

  n1<-nrow( filter( dataBaseData$Trickers, is.na(Tricker)))
  n2<-nrow(filter( dataBaseData$Trickers, Tricker == ""))
  if( n1!=0 | n2 != 0)
  {
    dbDisconnect(mydb)

    throw("Trikkeri kenttä ei voi olla tyhjä")

  }


if( length(dataBaseData$Trickers$Tricker) == length(unique(dataBaseData$Trickers$Tricker))){

  dbWriteTable(conn=mydb, name="Portfolios", value=pfo, overwrite=TRUE)
  dbWriteTable(conn=mydb, name="Portfolio", value=dataBaseData$Portfolio, overwrite=TRUE)
  dbWriteTable(conn=mydb, name="Groups", value=dataBaseData$Groups, overwrite=TRUE)
  dbWriteTable(conn=mydb, name="Trickers", value=dataBaseData$Trickers, overwrite=TRUE)
  dbWriteTable(conn=mydb, name="HandlingData", value=dataBaseData$HandlingData, overwrite=TRUE)
}else{
  for(tr in dataBaseData$Trickers$Tricker)
  {
    nn <- dataBaseData$Trickers|>filter(Tricker==tr)

    if( nrow(nn) != 1)
    {
      showModal(modalDialog(
        title = "Error!",
        paste0("Sinulla on sama trikkeri useamman kerran tai trikkeri puuttuu trikkeri = ", tr)
    ))
    }
  }
}

    },
error=function(cond) {

  dbDisconnect(mydb)
  showModal(modalDialog(
    title = "Error!",
    cond
  ))

  return(NA)
})



  dbDisconnect(mydb)
}


LoadPortfolioData <- function(PfName){
  log_info("LoadPortfolioData <- function(PfName)")
  data<-readDB()
  dataPf <- data$Portfolios
  ret <- dataPf |> filter( dataPf$PortfolioName==PfName)
  return(ret)
}

#ss$date <- format(ss$date, "%d.%m.%Y")

readDB<-function(){
  log_info("readDB<-function()")

  mydb <- dbConnect(RSQLite::SQLite(), getOption("DatabaseFile"))
  Portfolios <- dbReadTable(mydb, "Portfolios")
  Portfolios$CloseVal = 1000
  Portfolios$date<- format(as.Date(Portfolios$date), getOption("DateFormat"))
  Portfolio <- dbReadTable(mydb, "Portfolio")
  Groups <- dbReadTable(mydb, "Groups")
  HandlingData <- dbReadTable(mydb, "HandlingData")
  Trickers <- dbReadTable(mydb, "Trickers")
  if( nrow(Trickers)>0){

   #  Trickers$FirstData <- format( as.Date(as.numeric(Trickers$FirstData), origin = "1970-01-01"), getOption("DateFormat"))
    # Trickers$LastUpdate <- format( as.Date(as.numeric(Trickers$LastUpdate), origin = "1970-01-01"), getOption("DateFormat"))
   # browser()
    #Portfolios$date <- format( as.Date(as.numeric(Portfolios$date), origin = "1970-01-01"), getOption("DateFormat"))
  }else {
    Trickers[1,] <- NA
  }


  dbDisconnect(mydb)

  ret<- list(Portfolios=Portfolios,  Portfolio=Portfolio,Groups=Groups, Trickers=Trickers, HandlingData=HandlingData)
  return(ret)
}

ReadPFData <- function(){
  log_info("ReadPFData <- function()")
  db <-getOption("DatabaseFile")
  con <- dbConnect(drv = RSQLite::SQLite(),
                   dbname = db)

  pfData <-dbReadTable(con, "portfolioData")
  pf <- mutate(pfData, Date = as.Date(pfData$Date, origin = "1970-01-01"))

  dbDisconnect(con)


  return(pf)
}

WriteStockData <- function( data){
  log_info("WriteStockData <- function( data)")
  db <- getOption("DatabaseFile")
#  data <- df_yf

  con <- dbConnect(drv = RSQLite::SQLite(),
                   dbname = db)

  dbWriteTable(con, "StockCourses", data, overwrite = TRUE)
  dbDisconnect(con)
}

# ReadStockData <- function(){
#   log_info("ReadStockData <- function()")
#
#   db <- "Data\\stocData.RData"
#   st <- load(db)
#   db <- "Data\\OmaDB.sqlite"
#   con <- dbConnect(drv = RSQLite::SQLite(),
#                     dbname = db)
#    stData <- dbReadTable( con, "StockCourses")
#    dbDisconnect(con)
#    st <- mutate( stData,
#                  ref_date = as.Date(stData$ref_date, origin="1970-01-01"))
#    save()
#
#   return(st)
# }

ReadPF <- function(removeBest = 0){
  log_info("ReadPF <- function(removeBest = 0)")
  db <- getOption("DatabaseFile")
  con <- dbConnect(drv = RSQLite::SQLite(),
                   dbname = db)

  pf <-dbReadTable(con, "Portfolios")
  pf <- mutate(pf, Date = as.Date(pf$Date, origin = "1970-01-01"))

  dbDisconnect(con)

  if( removeBest != 0){
    wide <- pf |>
      pivot_wider(
        names_from = Salkut,
        values_from = Arvot
      )
    startVals <- wide[1,]
    xts_wide <-as.xts( wide, )
    retMatrix <- CalculateReturns(xts_wide)
    Return.cumulative(ret)


  }


  return(pf)
}


findMinAndMax<- function(pf, howMany=0){
  log_info("findMinAndMax<- function(pf, howMany=0)")
  # howMany<- -3
  wide <- pf |>
    pivot_wider(
      names_from = Salkut,
      values_from = Arvot
    )
  # wide$Unelmat
  startVals <- wide[1,]
  xts_wide <-as.xts( wide, )
  retMatrix <- CalculateReturns(xts_wide)

  #write.xlsx( pf, "F:\\R codes\\df.xlsx", sheetName = "pf")
  #write.xlsx( retMatrix, "F:\\R codes\\df.xlsx", sheetName = "retMatrix", append=TRUE)
  # write.xlsx( retMatrix, "F:\\R codes\\df.xlsx", sheetName = "ret.calc", append=TRUE)

  for( i in 1:ncol(retMatrix)){
    dec<-FALSE
    if( howMany<0) dec <- TRUE

    da <- coredata(retMatrix[,i])
    setNA <- sort(da, decreasing = dec, index.return = TRUE)


    for(ii in 1:abs(howMany) ){


      retMatrix[setNA$ix[ii]+1,i]<- NA
    }
  }

  retPrice<-startVals
  me <- startVals
  preVal<-startVals
  # Calculate all prices
  for(i in 2:nrow(retMatrix)){
    for( ii in 2:ncol(retPrice)){
      vallu <- pull(preVal[1, ii])+pull(preVal[1, ii])*as.numeric(retMatrix[i,ii-1])
      me[1, ii]<-vallu
      if(! is.na(vallu))
        preVal[1,ii] <- vallu
    }
    me[1, 'Date'] <- index(retMatrix[i])

    retPrice<-bind_rows( retPrice, me)
  }
  retDf <-   retPrice |>
    pivot_longer(!Date, names_to = "Salkut", values_to = "Arvot")


  retDf<-as.data.frame(retDf)

  return(retDf)
}
