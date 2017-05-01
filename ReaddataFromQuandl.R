stockList <- c("PNB" , "PSB", "ALBK" , "ANDHRABANK" , "MAHABANK", "ORIENTBANK", "DENABANK" , "CENTRALBK","UNITEDBNK","CUB" , "SYNDIBANK" , "CENTRALBK" , "VIJAYABANK" , "CORPBANK" , "UCOBANK" , "BANKBEES" , "INGVYSYABK", "DCB" , "RELBANK" , "EBANK" , "INDUSINDBK" )
for( i in stockList) {
 abc <- Quandl(paste0("NSE/" , i))
 write.csv(abc , paste0("C:\\Users\\win8\\project\\RProject\\", i , ".csv"))
}