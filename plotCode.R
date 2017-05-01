
load(file = "sensex.rda")
load(file = "portfolioReturn.rda")
tmp <- portfolioReturn
sensex <- with(sensex, sensex[Date >= as.Date("2010-01-03") & Date <= as.Date("2017-01-19"), ])
tmp$returnvalue <- log(portfolioReturn$returnvalue)
sensex$`Adjusted Close` <- log(sensex$`Adjusted Close`)
plot(tmp$tickerDataForDailyPortfolio.Date , tmp$returnvalue , type = "l" , col = "green" , xlab = "Date" , ylab = "Return/Market(log scale)" , main = "Portfolio V/s Maket" , ylim = range(min(sensex$`Adjusted Close` ), (max(tmp$returnvalue)+0.25)) )
par(new = TRUE)
plot(sensex$Date , sensex$`Adjusted Close` , type = "l" , col = "blue" , xlab = NA , ylab = NA , ylim = range(min(sensex$`Adjusted Close` ), (max(tmp$returnvalue)+.25)) )

legend("topleft" ,legend = c("Market","Portfolio") ,  col = c("blue" , "green")   , lty = 1:2 , cex = 0.8)