log_setting <<- function(){
  log_threshold(INFO)
  log_layout(layout_simple)
  f <- paste( getwd(), "\\logs\\log.txt", sep="")
  log_appender(appender_file(f, max_lines = 500, max_files = 1L))

}

optionSettings <- function(){
  baseDir <- getOption("baseDir")
  options(DateFormat = "%d.%m.%Y")
  options(DateFormatRH = "DD.MM.YY")
  options(DatabaseFile = paste0(baseDir, "Data\\Datas.db"))
  options( StockDataFile= paste0(baseDir,"Data\\stockData.RData"))
  options( PortfolioResults= paste0(baseDir, "Data\\"))
}

