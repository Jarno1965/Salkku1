#' update Portfilio P&L over a Dates range
#'
#' The \code{updatePortf} function goes through each symbol and calculates the PL for each period prices are available.
#'
#' Note that the portfolio will be marked on every time stamp where prices are available.
#' As such, your \code{Dates} range must reflect timestamps which appear in the price stream.
#' Also note that you probably don't want to mark the portfolio on every tick,
#'
#'
#' @return assigns position information and PL into the environment
#'
#' @param Portfolio string identifying a portfolio
#' @param Symbols character vector identifying symbols to update the portfolio for, default NULL
#' @param Dates optional xts-style ISO-8601 time range to run updatePortf over, default NULL (will use times from Prices)
#' @param Prices optional xts object containing prices and timestamps to mark the book on, default NULL
#' @param Interval optional character string, containing one of "millisecond" (or "ms"), "microsecond" (or "us"),
#' "second", "minute", "hour", "day", "week", "month", "quarter", or "year".  This can optionally be preceded by
#' a positive integer, or followed by "s".
#' @param \dots any other passthrough parameters
#' @export
updatePortfo <- function(Portfolio, Symbols=NULL, Dates=NULL, Prices=NULL, Interval=Interval, ...)
{ #' @author Peter Carl, Brian Peterson
  log_info("inside updatePortfo( {Portfolio}...)")
  pname<-Portfolio
  Portfolio<-.getPortfolio(pname) # TODO add Date handling

  # FUNCTION
  if(is.null(Symbols)){
    Symbols = ls(Portfolio$symbols)
  }
  log_info("inside updatePortfo symbols {Symbols}")
  for(symbol in Symbols){
    log_info("getInstrument {symbol}")
    tmp_instr<-try(getInstrument(symbol), silent=TRUE)
    log_info("getInstrument {symbol} out")
    .updatePosPL(Portfolio=pname, Symbol=as.character(symbol), Dates=Dates, Prices=Prices, Interval=Interval, ...=...)
  }

  # Calculate and store portfolio summary table
  Portfolio<-.getPortfolio(pname) # refresh with an updated object
  if(is.null(Dates)) Dates <- unique(do.call(c,c(lapply(Portfolio$symbols, function(x) index(x[["posPL"]])), use.names=FALSE, recursive=FALSE)))

  #Symbols = ls(Portfolio$symbols)
  Attributes = c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Period.Realized.PL', 'Period.Unrealized.PL', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
  summary = NULL
  tmp.attr=NULL
  for(attribute in Attributes) {
    result=NULL
    switch(attribute,
           Net.Value =,
           Gross.Value =,
           Long.Value =,
           Short.Value =,{
             # all these use Pos.Value
             if(is.null(tmp.attr)){
               table = .getBySymbol(Portfolio = Portfolio, Attribute = "Pos.Value", Dates = Dates, Symbols = Symbols)
               tmp.attr="Pos.Value"
             }
             switch(attribute,
                    Gross.Value = {  result = xts(rowSums(abs(table), na.rm=TRUE), order.by=index(table))},
                    Long.Value  = { tmat = table
                    tmat[tmat < 0] <- 0
                    result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
                    },
                    Short.Value = { tmat = table
                    tmat[tmat > 0] <- 0
                    result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
                    },
                    Net.Value   = {	result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))	}
             )
           },
           Period.Realized.PL =,
           Period.Unrealized.PL =,
           Gross.Trading.PL =,
           Txn.Fees =,
           Net.Trading.PL = {
             table = .getBySymbol(Portfolio = Portfolio, Attribute = attribute, Dates = Dates, Symbols = Symbols)
             tmp.attr = NULL
             result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
           }
    )

    colnames(result) = attribute
    if(is.null(summary)) {summary=result}
    else {summary=cbind(summary,result)}
  }

  # get rid of duplicated indices in the summary data,
  # thanks to Guy Yollin for the bug report and Josh Ulrich for the elegant approach to fixing it
  d <- duplicated(.index(summary)) | duplicated(.index(summary), fromLast=TRUE)
  if(any(d)){
    # extract duplicated rows; get last row for each duplicate
    summary.dups <- summary[d,]
    ds <- duplicated(.index(summary.dups)) & !duplicated(.index(summary.dups), fromLast=TRUE)
    # get the last value
    cLast <- c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value')
    lastCols <- summary.dups[which(ds),cLast]
    # sum values
    cSums <- c('Period.Realized.PL', 'Period.Unrealized.PL', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
    # take cumulative sum; keep last value for each duplicate
    sumCols <- cumsum(summary.dups[,cSums])[which(ds),cSums]
    # subtract previous value from current value, since we used cumsum
    sumColsLag <- lag(sumCols)
    sumColsLag[1,] <- 0
    sumCols <- sumCols - sumColsLag
    slist <- merge(sumCols,lastCols)      # combine aggregated objects
    slist <- slist[,colnames(summary)]    # order columns
    summary <- rbind(summary[!d,], slist) # put it all back together
  }

  # if(!is.timeBased(Dates)) Dates = xts:::time.xts(Portfolio$symbols[[1]][["posPL"]][Dates])
  #xts(,do.call(unlist,c(lapply(symbols,index),use.names=FALSE)))
  if(!is.timeBased(Dates)) Dates <- unique(do.call(c,c(lapply(Portfolio$symbols, function(x) index(x[["posPL"]][Dates]) ), use.names=FALSE, recursive=FALSE)))
  startDate = first(Dates)-.00001
  # trim summary slot to not double count, related to bug 831 on R-Forge, and rbind new summary
  if( as.POSIXct(attr(Portfolio,'initDate'))>=startDate || length(Portfolio$summary)==0 ){
    Portfolio$summary<-summary #changes to subset might not return a empty dimnames set of columns
  }else{
    Portfolio$summary<-rbind(Portfolio$summary[paste('::',startDate,sep='')],summary)
  }

  #portfolio is already an environment, it's been updated in place
  #assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )

  return(pname) #not sure this is a good idea
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/)
# Copyright (c) 2008-2015 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################


.updatePosPL <- function(Portfolio, Symbol, Dates=NULL, Prices=NULL, ConMult=NULL, Interval=NULL, ...)
{ # @author Peter Carl, Brian Peterson

  log_info(".updatePosPL")
  rmfirst=FALSE
  prices=NULL
  pname<-Portfolio
  Portfolio<-.getPortfolio(pname)
  log_info("111")
  p.ccy.str<-attr(Portfolio,'currency')
  if(is.null(p.ccy.str)) p.ccy.str<-'NA'
  tmp_instr<-try(getInstrument(Symbol), silent=TRUE)
  if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
    warning(paste("Instrument",Symbol," not found, things may break"))
    tmp_instr<-list(currency="USD",multiplier=1)
  }
  dargs <- list(...)
  if(!is.null(dargs$env)) {env <- dargs$env} else env=.GlobalEnv
  if(!is.null(dargs$symbol)) {symbol<-dargs$symbol} else symbol=NULL
  if(!is.null(dargs$prefer)) {prefer<-dargs$prefer} else prefer=NULL
  if(is.null(Prices)){
    log_info("prices=getPrice(get(Symbol, pos=env), symbol=symbol, prefer=prefer)[,1]")
    prices=getPrice(get(Symbol, pos=env), symbol=symbol, prefer=prefer)[,1]
  } else {
    prices=Prices
  }
  log_info("222")
  # if no date is specified, get all available dates
  if(is.null(Dates)) {
    Dates = index(prices)
    # Covert to POSIXct w/same TZ as portfolio object
    if(any(tclass(prices) %in% c("Date","yearmon","yearqtr"))) {
      portfTZ <- tzone(Portfolio$symbols[[Symbol]]$txn)
      Dates <- as.POSIXct(as.character(as.Date(Dates)), tz=portfTZ)
    }
  } else if(!is.timeBased(Dates)) {
    # Parse ISO8601 dates and check for NA and bounds
    parsedDates <- .parseISO8601(Dates)
    t1 <- parsedDates$first.time
    t1 <- if (is.na(t1) || t1 < as.POSIXct(start(prices))) "" else format(t1)
    tN <- parsedDates$last.time
    tN <- if (is.na(tN) || tN > as.POSIXct(end(prices))) "" else format(tN)

    # Warn user if bound and/or NA check failed
    dateRange <- paste(t1, tN, sep="/")
    if(t1 == "" || tN == "") {
      priceRange <- paste(range(index(prices)), collapse="/")
      dateRangeStr <- if(dateRange == "/") "all data" else dateRange
      warning("Could not parse ", Dates, " as ISO8601 string, or one/both",
              "ends of the range were outside the available prices: ",
              priceRange, ". Using ", dateRangeStr," instead.")
    }
    # Date subset
    Dates <- index(prices[dateRange])
  }
  log_info("333")
  if(!missing(Interval) && !is.null(Interval)) {
    ep_args <- .parse_interval(Interval)
    log_info("eeee")
    prices <- prices[endpoints(prices, on=ep_args$on, k=ep_args$k)]
  }
  log_info("444")
  log_info("prices = {prices}")
  log_info("typeof prices = {typeof(prices)}")
  log_info("class prices = {class(prices)}")
  log_info("ncol(prices) = {ncol(prices)}")
  if(ncol(prices)>1) prices=getPrice(Prices,Symbol)
  log_info("5555")
  # line up Prices dates with Dates set/index/span passed in.
  startDate = first(Dates)-.00001 #does this need to be a smaller/larger delta for millisecond data?
  endDate   = last(Dates)
  log_info("12125555")
  if(is.na(endDate)) endDate<-NULL
  dateRange = paste(startDate,endDate,sep='::')
  log_info("1616165555")
  #subset Prices by dateRange too...
  Prices<-prices[dateRange]
  log_info("55343455")
  if(nrow(Prices)<1) {
    Prices=xts(cbind(Prices=as.numeric(last(prices[paste('::',endDate,sep='')]))),as.Date(endDate))
    warning('no Prices available for ',Symbol,' in ',dateRange,' : using last available price and marking to ', endDate)
  }

  # Prices <- Prices[dateRange][,1] # only take the first column, if there is more than one
  log_info("#	***** ssss *****#")
  colnames(Prices)<-'Prices' # name it so we can refer to it by name later
  log_info("#	***** Vectorization *****#")
  #	***** Vectorization *****#
  # trim posPL slot to not double count, related to bug 831 on R-Forge
  Portfolio$symbols[[Symbol]]$posPL<-Portfolio$symbols[[Symbol]]$posPL[paste('::',startDate,sep='')]
  log_info("#	1111#")
   Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]<-Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]][paste('::',startDate,sep='')]
   log_info("#	2222#")
  priorPL<-last(Portfolio$symbols[[Symbol]]$posPL)
  log_info("#	33333#")
  if(nrow(priorPL)==0) {
    log_info("#	44444#")
    cn<-colnames(priorPL)
    priorPL = xts(t(rep(0,ncol(priorPL))),order.by=startDate-1)
    colnames(priorPL)<-cn
    log_info("#	555554#")
  }
  log_info("#	66666#")
  Txns <- Portfolio$symbols[[Symbol]]$txn[dateRange]
  # if there are no transactions, get the last one before the current dateRange, we'll discard later
  if(nrow(Txns)==0) {
    Txns <- last(Portfolio$symbols[[Symbol]]$txn[paste('::',startDate,sep='')])
  }
  log_info("#	777777#")
  # Get values frop priorPL into Txns; only keep columns we need from Txns
  # NOTE: There will usually be fewer transactions than price observations,
  # so do as much as possible before merging with potentially large price data
  TxnsCols <- c('Txn.Value','Txn.Fees','Gross.Txn.Realized.PL','Net.Txn.Realized.PL','Pos.Qty','Pos.Avg.Cost','Con.Mult')
  log_info("#	88888777777#")
   tmpPL <- merge(Txns[,TxnsCols], xts(,index(priorPL)))
  if(is.na(tmpPL[1,'Pos.Qty']))
    tmpPL[1,'Pos.Qty'] <- priorPL[1,'Pos.Qty']
   log_info("#	99999777777#")
  if(is.na(tmpPL[1,'Con.Mult']))
    tmpPL[1,'Con.Mult'] <- priorPL[1,'Con.Mult']
   log_info("#	99999777777#")
  if(is.na(tmpPL[1,'Pos.Avg.Cost']))
    tmpPL[1,'Pos.Avg.Cost'] <- priorPL[1,'Pos.Avg.Cost']
   log_info("#	aaaa88888777777#")
  # Now merge with prices
  tmpPL <- merge(tmpPL, Prices)
  log_info("#	bbbbb777777#")
  if(is.na(tmpPL[1,'Prices'])){
    #first price is NA, it would be nice to fill it in with a previous last valid price
    fprice <- last(prices[paste('::',startDate,sep='')])
    if (length(fprice)==1) tmpPL[1,'Prices'] <- fprice
    # if there's no previous valid price, calculate it from the prior position value
    # (can occur if .updatePosPL is called repeatedly with only one date/price)
    if (length(fprice)==0) tmpPL[1,'Prices'] <- priorPL[,'Pos.Value'] / priorPL[,'Pos.Qty']
  }
  log_info("#	ccc777777#")
  # na.locf any missing prices with last observation (this assumption seems the only rational one for vectorization)
  # and na.locf Pos.Qty,Con.Mult,Pos.Avg.Cost to instantiate $posPL new rows
  columns <- c('Prices','Pos.Qty','Con.Mult','Pos.Avg.Cost')
  tmpPL[,columns] <- na.locf(tmpPL[,columns])
  log_info("#	dddd777777#")
  #TODO check for instrument multiplier rather than doing all this messing around, if possible.
  tmpPL[,'Con.Mult'] <- na.locf(tmpPL[,'Con.Mult'], fromLast=TRUE) # carry NA's backwards too, might cause problems with options contracts that change multiplier
  if(any(naConMult <- is.na(tmpPL[,'Con.Mult'])))  # belt + suspenders?
    tmpPL[naConMult,'Con.Mult'] <- 1
  log_info("#	eeeee777777#")
  # zerofill Txn.Value, Txn.Fees
  tmpPL[is.na(tmpPL[,'Txn.Value']),'Txn.Value'] <- 0
  tmpPL[is.na(tmpPL[,'Txn.Fees']),'Txn.Fees']  <- 0
  log_info("#	ffff777777#")
  # matrix calc Pos.Qty * Price * Con.Mult to get Pos.Value
  tmpPL <- merge(tmpPL, Pos.Value=drop(tmpPL[,'Pos.Qty'] * tmpPL[,'Con.Mult'] * tmpPL[,'Prices']))
  log_info("#	gggg777777#")
  log_info("tmpPL {tmpPL}")
  LagValue <- lag(tmpPL[,'Pos.Value'])
  log_info("#	111gggg777777#  LagValue = {LagValue}")
  LagValue[is.na(LagValue)] <- 0  # needed to avoid a possible NA on the first value that would mess up the Gross.Trading.PL calc
  log_info("#	222gggg777777#")
  tmpPL <- merge(tmpPL, Gross.Trading.PL=drop(tmpPL[,'Pos.Value']- LagValue - tmpPL[,'Txn.Value']))
  log_info("#	hhhhh777777#")
  # alternate matrix calc for Realized&Unrealized PL that is only dependent on Txn PL and Gross.Trading.PL
  tmpPL[is.na(tmpPL[,'Net.Txn.Realized.PL']),'Net.Txn.Realized.PL'] <- 0
  tmpPL[is.na(tmpPL[,'Gross.Txn.Realized.PL']),'Gross.Txn.Realized.PL'] <- 0
  log_info("#	jjjjj777777#")
  # matrix calc Period.*.PL, Net.Trading.PL as Gross.Trading.PL + Txn.Fees
  tmpPL <- merge(tmpPL,
                 Period.Realized.PL = drop(tmpPL[,'Gross.Txn.Realized.PL']),  # believe it or not, merging is faster than renaming
                 Period.Unrealized.PL = drop(round(tmpPL[,'Gross.Trading.PL'] - tmpPL[,'Gross.Txn.Realized.PL'], 2)),
                 Net.Trading.PL = drop(tmpPL[,'Gross.Trading.PL'] + tmpPL[,'Txn.Fees']),
                 Ccy.Mult = 1)  # Ccy.Mult for this step is always 1

  # Ccy.Mult for this step is always 1
  tmpPL[,'Ccy.Mult'] <- 1
  log_info("#	kkkkk777777#")
  # reorder,discard  columns for insert into portfolio object
  tmpPL <- tmpPL[,c('Pos.Qty', 'Con.Mult', 'Ccy.Mult', 'Pos.Value', 'Pos.Avg.Cost', 'Txn.Value',  'Period.Realized.PL', 'Period.Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')]
  log_info("#	lll777777#")
  # rbind to $posPL slot
  tmpPL <- tmpPL[dateRange] #subset to get rid of any prior period Txn or PosPL rows we inserted
  Portfolio[['symbols']][[Symbol]][['posPL']]<-rbind(Portfolio[['symbols']][[Symbol]][['posPL']],tmpPL)



  # now do the currency conversions for the whole date range
  TmpPeriods<-Portfolio$symbols[[Symbol]]$posPL[dateRange]

  CcyMult = NA
  FXrate = NA
  invert=FALSE
  if(!is.null(attr(Portfolio,'currency'))) {
    if (tmp_instr$currency==p.ccy.str) {
      CcyMult<-1
    } else {
      port_currency<-try(getInstrument(p.ccy.str), silent=TRUE)
      if(inherits(port_currency,"try-error") | !is.instrument(port_currency)){
        warning("Currency",p.ccy.str," not found, using currency multiplier of 1")
        CcyMult<-1
      } else { #convert from instr ccy to portfolio ccy
        FXrate.str<-paste(tmp_instr$currency, p.ccy.str, sep='') # currency quote convention is EURUSD which reads as "USD per EUR" or "EUR quoted in USD"
        FXrate<-try(get(FXrate.str), silent=TRUE)
        #TODO FIXME: this uses convention to sort out the rate, we should check $currency and $counter_currency and make sure directionality is correct
        if(inherits(FXrate,"try-error")){
          #try to get the inversion
          FXrate_inv.str<-paste(p.ccy.str, tmp_instr$currency, sep='')
          FXrate_inv<-try(get(FXrate_inv.str), silent=TRUE)
          if(inherits(FXrate_inv,"try-error")){
            # inversion not found either
            warning("Exchange Rate",FXrate_inv.str," not found for symbol,',Symbol,' using currency multiplier of 1")
            CcyMult<-1
          }
        }
      }

    }
  } else {
    message("no currency set on portfolio, using currency multiplier of 1")
    CcyMult =1
  }
  if(is.na(CcyMult) && !is.na(FXrate)) {
    if(inherits(FXrate,'xts')){
      if(ncol(FXrate)>1) CcyMult <- getPrice(FXrate[dateRange],...)
      else CcyMult <- FXrate[dateRange]
      CcyMult <- na.locf(merge(CcyMult,index(TmpPeriods)))
      CcyMult <- CcyMult[index(TmpPeriods)]
    } else {
      CcyMult<-as.numeric(FXrate)
    }
  } else {
    CcyMult<-1
  }
  if(isTRUE(invert)){
    # portfolio and instrument have different currencies, and FXrate was in the wrong direction
    CcyMult<-1/CcyMult
  }

  if (length(CcyMult)==1 && CcyMult==1){
    Portfolio[['symbols']][[Symbol]][[paste('posPL',p.ccy.str,sep='.')]] <- Portfolio[['symbols']][[Symbol]][['posPL']]
  } else {
    #multiply the correct columns
    columns<-c('Pos.Value', 'Txn.Value', 'Pos.Avg.Cost', 'Period.Realized.PL', 'Period.Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
    TmpPeriods[,columns] <- TmpPeriods[,columns] * drop(CcyMult)  # drop dims so recycling will occur
    TmpPeriods[,'Ccy.Mult'] <- CcyMult

    #add change in Pos.Value in base currency
    LagValue <- as.numeric(last(Portfolio[['symbols']][[Symbol]][[paste('posPL',p.ccy.str,sep='.')]][,'Pos.Value']))
    if(length(LagValue)==0) LagValue <- 0
    LagPos.Value <- lag(TmpPeriods[,'Pos.Value'],1)
    LagPos.Value[1] <- LagValue
    CcyMove <- TmpPeriods[,'Pos.Value'] - LagPos.Value - TmpPeriods[,'Txn.Value'] - TmpPeriods[,'Period.Unrealized.PL'] - TmpPeriods[,'Period.Realized.PL']
    columns<-c('Gross.Trading.PL','Net.Trading.PL','Period.Unrealized.PL')
    TmpPeriods[,columns] <- TmpPeriods[,columns] + drop(CcyMove)  # drop dims so recycling will occur

    #stick it in posPL.ccy
    Portfolio[['symbols']][[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]<-rbind(Portfolio[['symbols']][[Symbol]][[paste('posPL',p.ccy.str,sep='.')]],TmpPeriods)
  }

  #portfolio is already an environment, it's been updated in place
  #assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
}

.parse_interval <- function(interval) {

  # taken/modified from xts:::last.xts
  ip <- gsub("^([[:digit:]]*)([[:alpha:]]+)", "\\1 \\2", interval)
  ip <- strsplit(ip, " ", fixed = TRUE)[[1]]
  if (length(ip) > 2 || length(ip) < 1)
    stop(paste("incorrectly specified", sQuote("interval")))

  rpu <- ip[length(ip)]
  rpf <- ifelse(length(ip) > 1, as.numeric(ip[1]), 1)

  dt.list <- c("milliseconds", "ms", "microseconds", "us", "secs",
               "mins", "hours", "days", "weeks", "months", "quarters", "years")
  dt.ind <- pmatch(rpu, dt.list)
  if(is.na(dt.ind))
    stop("could not uniquely match '", rpu, "' in '", paste0(dt.list,collapse=",'", "'"))
  dt <- dt.list[dt.ind]

  list(on=dt, k=rpf)
}
