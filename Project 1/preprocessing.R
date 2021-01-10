bankData <- read.csv(file='D:/Data Minning/bank-additional-full.csv',sep=';',header=TRUE)
summary(bankData)
unique(bankData)
x <- bankData[complete.cases(bankData), ]
str(x)
y<-x[x$job != "unknown"& x$marital != "unknown" & x$education != "unknown" & x$default != "unknown" & x$housing != "unknown" & x$loan != "unknown", ]
z=subset(y, select = -c(marital,default,housing,contact) )
print(z)









unique(y)