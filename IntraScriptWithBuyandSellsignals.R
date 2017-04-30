setwd("C:\\Users\\win8\\project\\RProject\\INTRADAY Data")
library(egcm)



# X <- readRDS("PNBupdated.rds")
# load("BARODA.rda")
# Y <- head(BARODA , 374)

# 
# load("ICICIBANK.rda")
# load("HDFCB.rda")
# X<- head(ICICIB , 374)
# Y <- head(HDFCB , 374)


# X <- readRDS("UNIONBupdated.rds")
# Y <- readRDS("PNBupdated.rds")


X <- readRDS("SBIN.rds")
Y <- readRDS("PNBforSBi.rds")


overall <- 0

# signal1 = 1 if cointegrated with range of positive spread
# signal2 = 100*reg$coefficients
# signal3 = constant 100
# signal4 = constant -100
# signal5 = 1 if cointegrated with range of negative spread


finalReturn <<- 0
j<- 181
bet <- 0
a <- 1
b <- 180 
binSignal <- 0
signal1 <- 0
signal2 <- 0
signal3 <- 100
signal4 <- -100
signal5 <- 0
lsigmaPOsi <- 0.5
usigmaPosi <- 1.5
lsigmaNeg <- -1.5
usigmaNeg <- -0.5

itterator <- 0
tmp <- 0
FinalReturnPlot <- 0
analyzeStock <- function(){
  
  if(signal1[i] == 1 ){
    Buy <- X
    Sell <- Y
  }
  if(signal5[i] == 1){
    Buy <- Y
    Sell <-X
  }
  
  
}


getReturn <- function(){
  # for(i in 1:374) {
  #   if((signal1[i] & signal2[i]) == 1)
  #   {
  #     bet <<- i
  #     XaxisCost <<- X[i, ]$OPEN  #PNB
  #     YaxisCost <<- Y[i, ]$CLOSE  #UNION
  #     noofYaxisShares <<- signal2[i]*100
  #     CostYaxisShares <<- noofYaxisShares * YaxisCost
  #     CostXaxisSahres <<- c[i] * XaxisCost
  #     
  #   }
  
  while( j < 374){
    
    while(signal1[j] == 0 | signal5[j] == 0)
    {
      if(signal1[j] == 1) break
      if(signal5[j] == 1) break
      j <- j+1
      if(j == 375)
        break
    }
    if(j == 375)
      break
    # print(paste0("signal1 is ", signal1[j], "and signal 5 is " , signal5[j]))
    XaxisCost <<- X[j, ]$OPEN  #PNB
    YaxisCost <<- Y[j, ]$CLOSE  #UNION
    
    if(signal1[j] == 1 ){
      Buy <- X
      Sell <- Y
      bet <<- j

      noofYaxisShares <<- signal2[j]
      CostYaxisShares <<- -signal3 * YaxisCost
      CostXaxisSahres <<- noofYaxisShares * XaxisCost
      tmp <- j
      while(signal1[tmp] == 1)
      {
        tmp <- tmp+1
      }
      #print(paste0("cointegration is", signal1[j], "and j =" , j))
      out <<- tmp
      XaxisCostNow <<- X[tmp, ]$OPEN  #PNB
      YaxisCostNow <<- Y[tmp, ]$CLOSE  #UNION
      CostYaxisSharesNow <<- signal3*YaxisCostNow
      CostXaxisSahresNow <<- noofYaxisShares * XaxisCostNow
      
      ReturnX <<- CostXaxisSahresNow - CostXaxisSahres
      #print(paste0("xaxis return is " , ReturnX))
      ReturnY <<- CostYaxisShares + CostYaxisSharesNow
      #print(paste0("YAxis Return is ", ReturnY))
      finalReturn <<- ReturnX + ReturnY
      FinalReturnPlot[j] <<- finalReturn
      print(paste0("final Return is " , finalReturn))
    
    }
    
    
    
    if(signal5[j] == 1){
      Buy <- Y
      Sell <- X
      noofYaxisShares <<- signal2[j]
      CostYaxisShares <<- signal3 * YaxisCost
      CostXaxisSahres <<- -noofYaxisShares * XaxisCost
      tmp <- j
      while(signal5[tmp] == 1)
      {
        tmp <- tmp+1
      }
      #print(paste0("cointegration is", signal1[j], "and j =" , j))
      out <<- tmp
      XaxisCostNow <<- X[tmp, ]$OPEN  #PNB
      YaxisCostNow <<- Y[tmp, ]$CLOSE  #UNION
  
      CostYaxisSharesNow <<- signal3*YaxisCostNow
      CostXaxisSahresNow <<- noofYaxisShares* XaxisCostNow
      
      ReturnX <<- CostXaxisSahresNow + CostXaxisSahres
      #print(paste0("xaxis return is " , ReturnX))
      ReturnY <<- CostYaxisSharesNow - CostYaxisShares
      #print(paste0("YAxis Return is ", ReturnY))
      finalReturn <<- ReturnX + ReturnY
      FinalReturnPlot[j] <<- finalReturn
      print(paste0("final Return is " , finalReturn))
      overall[j] <<- overall + finalReturn
  
    }
    
     j<- tmp
    if(j > 375)
      
      break
    #print(paste0("cointegration is ", signal1[j], "and j =" , j))
    
    
    
    
    
    
    
    
  }
  if(j > 375)
  {print("done!")
    break}
  return
}

for(i in 1:1000)
{
  
  x <- X[a:b ,]$CLOSE
  y <- Y[a:b,]$CLOSE
  result <- allpairs.egcm(data.frame(x ,y))
  # print(result$is.cointegrated)
  if(result$is.cointegrated == TRUE)
  {
    binSignal[(b+1)] <- 1
    
  }
  else
    binSignal[(b+1)] <- 0
  #print(binSignal)
  reg <- lm(y~x)
  #print(paste0("the reg coeffcients are " , reg$coefficients) )
  sigma <- sd(reg$residuals)
  lsigma <- lsigmaPOsi*sigma
  usigma <- usigmaPosi*sigma
  if(result$is.cointegrated == TRUE & tail(reg$residuals, 1) < usigma & tail(reg$residuals , 1) > lsigma)
  {
    signal1[b+1] <- 1
    #print(result$beta)
    # print(usigma)
    # print(lsigma)
    # print(paste0("The residual value is " ,tail(reg$residuals , 1)))
  }
  else
    signal1[b+1] <- 0
  
  if(result$is.cointegrated == TRUE & tail(reg$residuals, 1) < usigmaNeg*sigma & tail(reg$residuals , 1) > lsigmaNeg*sigma)
  {
    #print("HERE")
    signal5[b+1] <- 1
    #print(result$beta)
    # print(usigma)
    # print(lsigma)
    # print(paste0("The residual value is " ,tail(reg$residuals , 1)))
  }
  else
    signal5[b+1] <- 0
  
  
  # print(paste0("signal1 here is " , signal1[b+1]))
  
  signal2[b+1] <- 100*reg$coefficients[2]
  
  
  
  
  if(b == 374)
    break
  a <- a+1
  b <- b+1
  
  
}
#debug(getReturn) 
getReturn()

#par(mfrow = c(1,2))
plot(X$DATE, head(binSignal , 374) , type = "s" , col= "green" , main = "BInary signal of INTRADAY TRADING Cointegration test" , ylab = "Binary value" , xlab = "Time")
plot(X$DATE, tail(signal1, 374) , type = "s" , col = "blue" , main = "COintegrated with spread in sigma range" , xlab = "TIME" , ylab = "Binary SIgnal")

 miss <- is.na(FinalReturnPlot)
FinalReturnPlot[miss] <- 0
a <- cumsum(FinalReturnPlot)
len <- length(a)
a[len:374] <- a[len]
  plot( X$DATE , a, type = "l" , col = "blue" , xlab = "TIME" , ylab = "CUMULATIVE RETURN" , main  = "CUMULATIVE RETURN OF INTRADAY PAIR TRADING ")