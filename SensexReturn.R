library('quantmod')


load("sensex.rda")
sensex$Date <- as.Date(sensex$Date , format = "%Y-%m-%d") 
sensex <- with(sensex, sensex[Date >= as.Date("2010-12-28"), ])


invest <- 100000



start <- c()
PriceGot <- c()

noofshares <- c()
priceGot <- c()
endDate <<- as.POSIXlt(as.Date("2011-1-1"))
#startDate <<- as.POSIXlt(as.Date("2016-01-18"))
x <- c(1:1000)
record <<- 0

p <<- 0
toPlot <<- c()

sensexForDailyPortfolio <<- c()

dailyPortfolioValue <- function(noofshares)
{
  
  
  c <- 0 
  endDate2 <<- as.POSIXlt(endDate)
  sellDate <- as.Date(endDate) %m+% months(3)
  
  
  print(paste0("endDate for daily portfolio " , endDate2))
  for(i in 1:90){
    
    print(paste0("Investment done is " , invest))
    

      sensex$Date <- as.Date(sensex$Date , format = "%Y-%m-%d") 
      
      sensexBuy <- with(sensex, sensex[(Date >= as.Date(endDate2)  & Date <= as.Date(sellDate)), ])
      
      #sensexBuy <- tail(sensexBuy, 1)
      sensexForDailyPortfolio <<- head(sensexBuy, 1)
      
      if((sensexForDailyPortfolio == 0) || sensexForDailyPortfolio$Date != endDate2)

      if (!is.null(sensexForDailyPortfolio))
      {
        if(c == 0)
          print(paste0("Portfolio value on date" , sensexForDailyPortfolio$Date)) 
    
        
        PriceGot <<- noofshares * sensexForDailyPortfolio$Close
      }

      print(paste0("price got " , PriceGot))
    c <- c+1
 
    if(PriceGot != 0)
    {
      toPlot1 <<- data.frame(sensexForDailyPortfolio$Date , priceGot)
      toPlot <<- rbind(toPlot, toPlot1)
      print(paste0("The total portfolio value is on date ", endDate2 ," is " , PriceGot))
    }
    #else
    # returnvalue <- invest
    print(paste("check here" , endDate2))
    endDate2 <- as.Date(endDate2) + 1
    print(paste0("check if date increases here" , endDate2))
    if(endDate2 >= sellDate)
      break
  }
  
  
  return()
}




investmentReturns <- function()
{
  invest <<- floor(invest)
  print(paste0("Investment done is " , invest))
 
  c = 0
    
   
    dateRange <- as.Date(endDate) - 5
      sensexBuy <- with(sensex, sensex[(Date >= as.Date(dateRange) & Date <= as.Date(endDate)), ])
    sensexBuy <- tail(sensexBuy, 1)
    if(c ==0)
      print(paste0("Buy shares on " , sensexBuy$Date))
    #print(paste0("Buy share " , p , " at Rs.", sensexBuy$Open ))
    noofshares <- invest/sensexBuy$Open
    noofshares <- floor(noofshares)
    
    sellDate <- as.Date(endDate) %m+% months(3)
    sensexSell <- with(sensex, sensex[(Date >= as.Date(endDate) & Date <= as.Date(sellDate)), ])
    sensexSell <- tail(sensexSell , 1)
    if(c == 0)
      print(paste0("Sell them on ",sensexSell$Date))
    #print(paste0("The sell price of share " , p , "is " , sensexSell$Close))
    
    PriceGot<- noofshares * sensexSell$Close
    c <- c+1
  print(paste0("endDate befor call" , endDate))
  dailyPortfolioValue( noofshares)
  
  print(noofshares)
  returnvalue1 <- sum(PriceGot)
  print(paste0("The total portfolio value on ", sellDate , "is", returnvalue1))
  invest <<- returnvalue1
  
  return()
  
}


debug(investmentReturns)
for(i in 1:100){
investmentReturns()
  sellDate <- as.Date(endDate) %m+% months(3)
endDate <- as.Date(sellDate) +1
}

