library('quantmod')
library('zoo')
library(Quandl)
library(calibrate)
library(ggplot2)
library(ggrepel)
library(lubridate)

varresult <- 0
meanresult <- 0
record <- 0
j <- 1
setwd("C:\\Users\\win8\\project\\RProject\\MeanVarRes")
start <- c()
PriceGot <- c()
#data1 <<- readRDS("data1.rds")
#data1 <- read.csv("C:\\Users\\win8\\Downloads\\Major Project Data\\TC1\\TC1_20161223.csv")
names(data1) <- c("Name" , "Date" , "Open", "High" , "Low" , "Last Traded Price" , "Close", "Traded Quantity", "Volume")
data1$Date <- as.Date(data1$Date , format = "%Y-%m-%d")      #to convert the date dates into Date class
stockList <- unique(data1$Name)
#stockList <- head(stockList,30)
noofshares <- c()
priceGot <- c()
startDate <<- as.POSIXlt(as.Date("2011-10-1"))
#startDate <<- as.POSIXlt(as.Date("2016-01-18"))
x <- c(1:1000)
record <<- 0
invest <<- 131744.3
#invest <<- 124010
p <<- 0
toPlot <<- c()



dailyPortfolioValue <- function(tickerselected , noofshares)
{
  c <- 0 
  endDate2 <<- as.POSIXlt(endDate)
  sellDate <- as.Date(endDate) %m+% months(3)
  
  for(i in 1:90){
    
    print(paste0("Investment done is " , invest))
    
    for( p in tickerselected)
    {
      
      tickerData <- data1[data1$Name == p,]
      tickerData$Date <- as.Date(tickerData$Date , format = "%Y-%m-%d") 
      
      tickerDataBuy <- with(tickerData, tickerData[(Date >= as.Date(endDate2)  & Date <= as.Date(sellDate)), ])
      
      #tickerDataBuy <- tail(tickerDataBuy, 1)
      tickerDataForDailyPortfolio <- head(tickerDataBuy, 1)
      
      if((tickerDataForDailyPortfolio == 0) || tickerDataForDailyPortfolio$Date != endDate2)
        break;
      if(!is.null(tickerDataForDailyPortfolio))
      {
        if(c == 0)
          print(paste0("Portfolio value on date" , tickerDataForDailyPortfolio$Date)) 
        #  noofshares[p] <- perSharePrice/tickerDataForDailyPortfolio$Open
        #noofshares[p] <- floor(noofshares[p])
        
        
        #tickerDataSell <- tickereDataBuy   #with(tickerData, tickerData[(Date >= as.Date(endDate) & Date <= as.Date(sellDate)), ])
        #tickerDataSell <- tail(tickerDataSell , 1)
        #print(paste0("The sell price of share " , p , "is " , tickerDataSell$Close))
        
        PriceGot[p] <- noofshares[p] * tickerDataForDailyPortfolio$Close
      }
      else
        break
    }
    c <- c+1
    returnvalue <- sum(PriceGot)
    if(returnvalue != 0)
    {
      toPlot1 <<- data.frame(tickerDataForDailyPortfolio$Date , returnvalue)
      toPlot <<- rbind(toPlot, toPlot1)
      print(paste0("The total portfolio value is on date ", endDate2 ," is " , returnvalue))
      }
    #else
     # returnvalue <- invest
    
    endDate2 <- as.Date(endDate2) + 1
    print(paste0("check if date increases here" , endDate2))
    if(endDate2 >= sellDate)
      break
  }

  
  return()
}
  
 


investmentReturns <- function(result)
{
  invest <<- floor(invest)
  print(paste0("Investment done is " , invest))
  tickerselected <<- row.names(result)
  print(row.names(result))
  NoofTicker <- nrow(result)
  perSharePrice <<- invest/NoofTicker
  print(paste0("per share price is" , perSharePrice))
  c = 0
  for( p in tickerselected)
  {
    tickerData <- data1[data1$Name == p,]
    tickerData$Date <- as.Date(tickerData$Date , format = "%Y-%m-%d") 
    dateRange <- as.Date(endDate) - 5
    tickerDataBuy <- with(tickerData, tickerData[(Date >= as.Date(dateRange) & Date <= as.Date(endDate)), ])
    tickerDataBuy <- tail(tickerDataBuy, 1)
    if(c ==0)
      print(paste0("Buy shares on " , tickerDataBuy$Date))
    #print(paste0("Buy share " , p , " at Rs.", tickerDataBuy$Open ))
    noofshares[p] <- perSharePrice/tickerDataBuy$Open
    noofshares[p] <- floor(noofshares[p])
    
    sellDate <- as.Date(endDate) %m+% months(3)
    tickerDataSell <- with(tickerData, tickerData[(Date >= as.Date(endDate) & Date <= as.Date(sellDate)), ])
    tickerDataSell <- tail(tickerDataSell , 1)
    if(c == 0)
      print(paste0("Sell them on ",tickerDataSell$Date))
    #print(paste0("The sell price of share " , p , "is " , tickerDataSell$Close))
    
    PriceGot[p] <- noofshares[p] * tickerDataSell$Close
    c <- c+1
  }
  
  dailyPortfolioValue(tickerselected , noofshares)
  
  print(noofshares)
  returnvalue1 <- sum(PriceGot)
  print(paste0("The total portfolio value on ", sellDate , "is", returnvalue1))
  invest <<- returnvalue1
  
  return()
  
}



selectPortfolio2 <- function(Com , record){
  
  ComSortMean <<- Com[order(Com$meanresult , decreasing = TRUE),]        #sorting data frame with mean
  ComSortVar <<- Com[order(Com$varresult , decreasing = FALSE , na.last = TRUE),] 
  duplicateMeanInCom <<- ComSortMean[duplicated(ComSortMean$meanresult),]
  duplicateVarInCom <<- ComSortVar[duplicated(ComSortVar$varresult),]
  if(nrow(duplicateMeanInCom) >=1 && nrow(duplicateVarInCom) == 0)
  {
    resultedCom <<- ComSortVar[!duplicated(ComSortVar[ ,2]),]
  }
  
  if(nrow(duplicateVarInCom) >=1 && nrow(duplicateMeanInCom) == 0)
  {
    resultedCom <<- ComSortMean[!duplicated(ComSortMean[ ,2]),]
  }
  
  if(nrow(duplicateMeanInCom) >=1 && nrow(duplicateVarInCom) >=1 )
  {
    resultedCom <<- ComSortVar[!duplicated(ComSortVar[ ,2]),]
    #print(resultedCom)
    resultedCom <<- resultedCom[order(resultedCom$meanresult , decreasing = TRUE),]
    resultedCom <<- resultedCom[!duplicated(resultedCom[ ,2]),]
  }
  if(endDate == as.Date("2013-04-01"))
  {
    resultedCom <<- resultedCom[!row.names(resultedCom) == "MARKSANS" , ]
  }
  resultedCom <<- head(resultedCom , 5)
  
  return(resultedCom)
}


selectPortfolio <- function(Com , record)
{
  sortedCom <<- Com[order(Com$meanresult , decreasing = TRUE),]        #sorting data frame with mean
  positiveCom <<- subset( sortedCom, sortedCom$meanresult > 0)         #getting stocks with postive mean
  positiveCom <<- round(positiveCom , digits = 5)                      #changing elements precision to 5
  
  for(k in 1 : length(positiveCom)){
    #getRow <- positiveCom$meanresult[k]
    uniqueCom <<- unique(positiveCom)
    duplicateMeanInCom <<- positiveCom[duplicated(positiveCom$meanresult),]
    duplicateVarInCom <<- positiveCom[duplicated(positiveCom$varresult),]
    
    if(nrow(positiveCom) <= 5)
      result <<- positiveCom
    
    else
    {
      if(nrow( duplicateMeanInCom) >=1 )
        result <<- selectPortfolio2(positiveCom ,record )
      
      if(nrow( duplicateVarInCom) >=1)
        result <<- selectPortfolio2(positiveCom , record)
    }
    
    
    
    old.par <- par(mfrow=c(1, 3))
    plot(Com , na.rm = TRUE)
    text(Com$varresult, Com$meanresult , labels = row.names(Com), cex = 0.75,pos = 4)
    abline( h=0)
    
    if(nrow(positiveCom) >=1){
      mtext(paste("Stocks Return's Mean v/s Variance for " , startDate , " - ", endDate),cex = 1 , cex.main = 0.8)
      plot(positiveCom , xlim = c (0, 1.2*max(positiveCom$varresult , na.rm = TRUE)) , ylim = c( 0, 1.2*max(positiveCom$meanresult , na.rm = TRUE)), na.rm = TRUE)
      text(positiveCom$varresult, positiveCom$meanresult , labels = row.names(positiveCom), cex = 0.75,pos = 4)
      plot(result, xlim = c (0, 1.2*max(positiveCom$varresult, na.rm = TRUE)) , ylim = c( 0, 1.2*max(positiveCom$meanresult, na.rm = TRUE)), na.rm = TRUE)
      text(result$varresult, result$meanresult , labels = row.names(result), cex = 0.75,pos = 4)
    }
    par(old.par)
  }
  
  return(result)
}


for( j in x)
{
  
  startDate1 <<- startDate
  endDate <<- as.POSIXlt(startDate)
  endDate$year <- as.Date(endDate$year) +1
  #if(startDate1 >= as.Date("2016-01-01"))
  #{
  # endDate <<- as.Date("2016-11-29")
  # }
  for (i in stockList) {
    
    data <- subset(data1 , Name == i)
    data$Date <- as.Date(data$Date , format = "%Y-%m-%d")      #to convert the date dates into Date class
    data2 <- data
    data <- with(data, data[(Date >= as.Date(startDate1) & Date <= as.Date(endDate)), ])
    
    data.Returns <- diff(data$Close)/data$Close[-length(data$Close)]
    Returns.mean <- mean(data.Returns,na.rm = TRUE)
    variance<- var(data.Returns, na.rm = TRUE)
    
    varresult[i]<- variance
    meanresult[i] <- Returns.mean
    
    #final <<- rbind(final , tail(data,1))
    #date2 <<- as.Date(endDate) + 4  #to get the date on which we will buy the stock
    #data2 <- with(data2, data2[(Date > as.Date(endDate) & Date <= as.Date(date2)), ])
    #df <<- data.frame(tail(data2, 1))
    #df <<- rbind(df, df1)
    
    
    
  }
  
  
  
  Com <<- data.frame( varresult , meanresult )
  saveRds(Com , file = paste0(startDate1,"-" , endDate))
  result <- selectPortfolio(Com , record)
  #plot(Com$varresult, Com$meanresult  , xlim = range(varresult , na.rm = TRUE), ylim = c(-0.003 , 0.005),xlab = "Return's Variance",ylab = "Return's Mean", main = paste("Stocks Return's Mean v/s Variance for " , startDate , " - ", endDate),cex = 1 , cex.main = 0.8)
  #text(Com$varresult, Com$meanresult , labels = row.names(Com), cex = 0.5,pos = 4)
  
  investmentReturns(result)
  startDate <<- as.Date(startDate) %m+% months(3)
  #rm(df)
  #if(endDate >= as.Date("2016-11-29"))
  #{
  #break
  #}
  
}
