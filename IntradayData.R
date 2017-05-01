setwd("C:\\Users\\win8\\project\\RProject\\INTRADAY Data")
library('RCurl')
tmp <- getURL('https://www.google.com/finance/getprices?q=ICICIBANK&i=60&p=15d&f=d,o,h,l,c,v')
tmp <- strsplit(tmp,'\n')
tmp <- tmp[[1]]
tmp <- tmp[-c(1:8)]
tmp <- strsplit(tmp,',')
tmp <- do.call('rbind',tmp)
tmp <- apply(tmp,2,as.numeric)
tmp <- tmp[-apply(tmp,1,function(x) any(is.na(x))),]
ICICIB <- data.frame(tmp)
names(ICICIB) <- c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME")
as.POSIXct(1484711100+300*60 , origin = "1970-01-01" , tz = 'EST')
save(ICICIB , file = "ICICIBANK.rda")



tmp <- getURL('https://www.google.com/finance/getprices?q=HDFCBANK&i=60&p=15d&f=d,o,h,l,c,v')
tmp <- strsplit(tmp,'\n')
tmp <- tmp[[1]]
tmp <- tmp[-c(1:8)]
tmp <- strsplit(tmp,',')
tmp <- do.call('rbind',tmp)
tmp <- apply(tmp,2,as.numeric)
tmp <- tmp[-apply(tmp,1,function(x) any(is.na(x))),]
HDFCB <- data.frame(tmp)
names(HDFCB) <- c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME")
save(HDFCB , file = "HDFCB.rda")



tmp <- getURL('https://www.google.com/finance/getprices?q=UNIONBANK&i=60&p=15d&f=d,o,h,l,c,v')
tmp <- strsplit(tmp,'\n')
tmp <- tmp[[1]]
tmp <- tmp[-c(1:8)]
tmp <- strsplit(tmp,',')
tmp <- do.call('rbind',tmp)
tmp <- apply(tmp,2,as.numeric)
tmp <- tmp[-apply(tmp,1,function(x) any(is.na(x))),]
UNIONB <- data.frame(tmp)
names(UNIONB) <- c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME")
save(UNIONB , file = "UNIONB.rda")


tmp <- getURL('https://www.google.com/finance/getprices?q=DENABANK&i=60&p=15d&f=d,o,h,l,c,v')
tmp <- strsplit(tmp,'\n')
tmp <- tmp[[1]]
tmp <- tmp[-c(1:8)]
tmp <- strsplit(tmp,',')
tmp <- do.call('rbind',tmp)
tmp <- apply(tmp,2,as.numeric)
tmp <- tmp[-apply(tmp,1,function(x) any(is.na(x))),]
DENAB <- data.frame(tmp)
names(DENAB) <- c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME")
save(DENAB , file = "DENAB.rda")


tmp <- getURL('https://www.google.com/finance/getprices?q=AXISANK&i=60&p=15d&f=d,o,h,l,c,v')
tmp <- strsplit(tmp,'\n')
tmp <- tmp[[1]]
tmp <- tmp[-c(1:8)]
tmp <- strsplit(tmp,',')
tmp <- do.call('rbind',tmp)
tmp <- apply(tmp,2,as.numeric)
tmp <- tmp[-apply(tmp,1,function(x) any(is.na(x))),]
AXISBANK<- data.frame(tmp)
names(AXISBANK) <- c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME")

save(AXISBANK , file = "HDFCB.rda")



tmp <- getURL('https://www.google.com/finance/getprices?q=SBIN&i=60&p=15d&f=d,o,h,l,c,v')
tmp <- strsplit(tmp,'\n')
tmp <- tmp[[1]]
tmp <- tmp[-c(1:8)]
tmp <- strsplit(tmp,',')
tmp <- do.call('rbind',tmp)
tmp <- apply(tmp,2,as.numeric)
tmp <- tmp[-apply(tmp,1,function(x) any(is.na(x))),]
SBIN <- data.frame(tmp)
names(SBIN) <- c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME")
save(SBIN , file = "SBIN.rda")



tmp <- getURL('https://www.google.com/finance/getprices?q=BANKBARODA&i=60&p=15d&f=d,o,h,l,c,v')
tmp <- strsplit(tmp,'\n')
tmp <- tmp[[1]]
tmp <- tmp[-c(1:8)]
tmp <- strsplit(tmp,',')
tmp <- do.call('rbind',tmp)
tmp <- apply(tmp,2,as.numeric)
tmp <- tmp[-apply(tmp,1,function(x) any(is.na(x))),]
BARODA <- data.frame(tmp)
names(BARODA) <- c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME")
save(BARODA , file = "BARODA.rda")


tmp <- getURL('https://www.google.com/finance/getprices?q=PNB&i=60&p=15d&f=d,o,h,l,c,v')
tmp <- strsplit(tmp,'\n')
tmp <- tmp[[1]]
tmp <- tmp[-c(1:8)]
tmp <- strsplit(tmp,',')
tmp <- do.call('rbind',tmp)
tmp <- apply(tmp,2,as.numeric)
tmp <- tmp[-apply(tmp,1,function(x) any(is.na(x))),]
PNB <- data.frame(tmp)
names(PNB) <- c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME")



save(PNB , file = "PNB.rda")