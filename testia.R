library(shiny)
library(dplyr)
library(yfR)
library(PerformanceAnalytics)
library(tidyverse)
library(tidyquant)
library(echarts4r)

tricker <- C("UPM.HE", "VALMT.HE", "SSABBH.HE")
#mydb <- getOption("StockDataFile")
mydb <- "Data\\stockData.RData"
stDname <- load( file =mydb)
stockData <- get(stDname)




loadStockData <- function( stockData, StockInfo){
  tricker <- c("^OMXHCAPGI", "VALMT.HE", "SSABBH.HE")
  tricker <-StockInfo$Tricker
  #mydb <- getOption("StockDataFile")
  mydb <- "Data\\stockData.RData"
  stDname <- load( file =mydb)
  stockData <- get(stDname)
  stockData=NULL

  #curr <- unique(curr$stocks$OsingonValuutta[!is.na(curr$stocks$OsingonValuutta)])

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
#stockData<-df
mydb <- getOption("StockDataFile")
save( stockData, file =mydb)

st$cumret_adjusted_prices <- st$cumret_adjusted_prices*100
valu <- min(st$cumret_adjusted_prices)*0.95
st |> group_by(ticker) |> e_charts(ref_date) |> e_line(cumret_adjusted_prices) |> e_datazoom() |>
  e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
  e_title(text = "HOSKn mallisalkut", left = "left") |>
  e_toolbox_feature("dataZoom") |> e_theme("chalk") |> e_animation(duration = 1000) |>
  e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
  e_y_axis(
    formatter = e_axis_formatter(style=c("currency"),
                                 digits=0,
                                 currency="EUR"
    ))


ddd %>%
  ggplot(aes(x = ref_date, y = cumret_adjusted_prices)) +
  geom_line() +
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'Portfolio Cumulative Returns') +
  theme_classic() +
  scale_y_continuous(breaks = seq(1,2,0.1)) +
  scale_x_date(date_breaks = 'year',
               date_labels = '%Y')

}
