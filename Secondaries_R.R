# A function that respectively: i) reads in preliminary stock symbols ii) crawls Yahoo Finance and downloads daily stock price data iii) builds data tables and iv) rus stock price correlations
the_whole_enchillada <- function()
  {
  setwd("/Users/dancardella/Desktop/Wellstream/Secondary_Event/Stock_Prices")
  symbols <- read.csv("Secondaries_Sample_List.csv", header = TRUE)
  num_of_stocks<- seq(1, length(symbols[,1]))
  
  update_prices <- function()
    {
    for (row in num_of_stocks){
      stock<- symbols[row,1]
      file_name<- paste("http://chart.finance.yahoo.com/table.csv?s=", stock,"&a=8&b=23&c=2015&d=8&e=23&f=2016&g=d&ignore=.csv", sep="")
      download.file(file_name, paste("./",stock,"prices.csv", sep=""),method ="curl")
  }
  list.files ("./")
}
  build_stock_tables <- function()
    {
      f <- read.csv("AKSprices.csv",header = TRUE)
      f_date_col<- f[,1]
      prices_table <- data.frame(f_date_col)
      volumes_table <- data.frame(f_date_col)
      stock_names =list("Date")
      
    for (row in num_of_stocks)
      {
      stock<- symbols[row,1]
      f <- read.csv(paste(stock,"prices.csv", sep=""),header = TRUE)
      prices_col <- f[,7]
      volumes_col <- f[,6]
      prices_table <- cbind(prices_table,prices_col)
      volumes_table <- cbind(volumes_table,volumes_col)
      stock<- as.vector(stock, mode="character")
      stock_names <- c(stock_names, stock)
      } 
      colnames(prices_table) <- stock_names
      colnames(volumes_table) <- stock_names
      print(prices_table)
      print(volumes_table)
      write.csv(prices_table, "Prices_table.csv", 
                 row.names = FALSE, fileEncoding = "utf8")
      write.csv(volumes_table, "Volumes_table.csv", 
                row.names = FALSE, fileEncoding = "utf8")
    }
  update_prices()
  build_stock_tables()
}

stock_correlation_table_build <- function(){
  table_names <- list(symbols[,1])
  correlation_table <- matrix(dimnames<- table_names)
  #rownames(correlation_table) <- table_names
  i<- 0
  seq_along(length(num_of_stocks))
    i<-i+1
    k<-0
    seq_along(length(num_of_stocks-i))
      k<-k+1
      correlation <- cor(prices_table[,i+1], prices_table[,i+1+k])
      correlation_table <- c(correlation_table, correlation)
}

the_whole_enchillada()






