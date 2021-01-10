setwd("D:\\DATA MINNING FINAL PROJECT")
#packages to import

install.packages("readxl")
install.packages("tidyverse")
install.packages("hash")
install.packages("arules")
install.packages("arulesViz")
install.packages("plyr")
library(plyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(datasets)
library("readxl")
library("hash")
library(dplyr)
library(stringr)
#Data Preprocessing
#1.Reading the Data set from the xlsx and store the columns which are required

retail_data <- read_excel("online-retail.xlsx")
required_columns_retail_data<-subset(retail_data,select= -c(StockCode,Quantity,InvoiceDate,UnitPrice,CustomerID,Country))

#2. Now discarding the Invoice Number starting with a C 

str(required_columns_retail_data)
#answer <-required_columns_retail_data[grep('^[C-c]', required_columns_retail_data$InvoiceNo),]
required_columns_retail_data<-required_columns_retail_data[!grepl('^[C-c]', required_columns_retail_data$InvoiceNo),]
#toMatch <- c("WRONG","LOST", "CRUSHED", "SMASHED", "DAMAGED", "FOUND", "THROWN", "MISSING", "AWAY", "\\?", "CHECK", "POSTAGE", "MANUAL", "CHARGES", "AMAZON", "FEE", "FAULT", "SALES", "ADJUST", "COUNTED", "LABEL", "INCORRECT", "SOLD", "BROKEN", "BARCODE", "CRACKED", "RETURNED", "MAILOUT", "DELIVERY", "MIX UP", "MOULDY", "PUT ASIDE", "ERROR", "DESTROYED", "RUSTY")
required_columns_retail_data <-required_columns_retail_data[!grepl("WRONG", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("wrong", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("LOST", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("lost", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("CRUSHED", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("crushed", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("SMASHED", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("smashed", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("DAMAGED", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("damaged", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("FOUND", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("found", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("THROWN", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("thrown", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("MISSING", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("missing", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("AWAY", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("away", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("\\?", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("CHECK", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("check", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("POSTGATE", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("postgate", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("MANUAL", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("manual", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("CHARGES", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("charges", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("AMAZON", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("amazon", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("FEE", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("fee", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("FAULT", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("fault", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("SALES", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("sales", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("ADJUST", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("adjust", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("COUNTED", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("counted", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("LABEL", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("label", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("INCORRECT", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("incorrect", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("SOLD", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("sold", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("BROKEN", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("broken", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("BARCODE", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("barcode", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("CRACKED", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("cracked", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("RETURNED", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("returned", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("MAILOUT", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("mailout", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("DELIVERY", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("delivery", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("MIX UP", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("mix up", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("MOULDY", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("mouldy", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("PUT ASIDE", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("put aside", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("ERROR", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("error", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("DESTROYED", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("destroyed", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("RUSTY", required_columns_retail_data$Description),]
required_columns_retail_data <-required_columns_retail_data[!grepl("rusty", required_columns_retail_data$Description),]
write.csv(required_columns_retail_data,'required_columns_retail_data.csv')
#trial <- as.factor(required_columns_retail_data$Description)
#str(trial)
#vector1 <- c()
#vector2<- c("demo","shah")
#vector2 <- append(vector2,"abc")
varx <- 1
h<-hash()
demo <-function(a1)
{
  a1<-tolower(a1)
  a1 <- str_squish(a1)
  length_of_h <- length(h)
  if(length_of_h == 0)
  {
 
    .set(h,keys=varx,values=a1)
     varx<<-varx+1
    return (varx-1)
  }
  else
  {
    for (i in 1:length_of_h)
    { 
      j <- toString(i)
      if(values(h,keys=j) == a1)
      {
        return(j)
      }
    }
      .set(h,keys=varx,values=a1)
      varx<<-varx+1
      
      return(varx-1)
    }
}
trial_dataset<-required_columns_retail_data
trial_dataset <- na.omit(trial_dataset)
len<-dim(trial_dataset)
finallen<-as.numeric(len[1])
for (i in 1:finallen)
{
  trial_dataset$Description[i]<-demo(trial_dataset$Description[i])   
}
write.csv(trial_dataset,'after_integer_required_columns_retail_data.csv', row.names = FALSE)

x<-data.frame("NO" = 1,"Description"="abc")
lengthof_h <- length(h)
counter<-1
for (i in 1:lengthof_h)
{
  if(counter==1)
  {
    
  }
  counter<<- counter+1
  print(i)
  if(i==1)
  {
    x[1,"Description"] <- values(h,keys="1")
  }
  else
  {
    print("fsdf")
    j <- toString(i)
    print(j)
    x<<- rbind(x,list(i,values(h,keys=j)))
  }
}
print(x)
write.csv(x,'integer_with_description_required_columns_retail_data.csv',row.names = FALSE)
storing<-required_columns_retail_data
storing <- na.omit(storing)
write.csv(storing,'fully_processed_with_whole_data_required_columns_retail_data.csv',row.names = FALSE)
tmt <-read.transactions("after_integer_required_columns_retail_data.csv",format="single",sep = "\n", cols = c(1,2))
inspect(tmt)
final_transaction_data <- ""
#==============================================TRYING THE TRANSACTION FUNCTIONALITY===============================================
transaction_data<-read.csv("after_integer_required_columns_retail_data.csv", sep = ",", header = TRUE) 
retail<-read.csv("after_integer_required_columns_retail_data.csv",sep=",")
transactionData <- ddply(retail,c("InvoiceNo"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
transactionData$InvoiceNo <- NULL
colnames(transactionData) <- c("items")
write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')


inspect(association.rules[1:10])


#=============================================NEW OPERATIONS====================================
checking<-apriori(tr, parameter = list(support= 0.007,target="frequent itemsets",minlen=1, maxlen=1))
checking1<-apriori(tr, parameter =list(target = "closed frequent itemsets", support = 0.004))
print(checking)

frequent_itemset_004_1<-eclat(data = tr, parameter = list(support =0.004, maxlen = 1,minlen = 1))
frequent_itemset_004_2<-eclat(data = tr, parameter = list(support =0.004, maxlen = 2,minlen = 2))
frequent_itemset_004_3<-eclat(data = tr, parameter = list(support =0.004, maxlen = 3,minlen = 3))

frequent_itemset_007_1<-eclat(data = tr, parameter = list(support =0.007, maxlen = 1,minlen = 1))
frequent_itemset_007_2<-eclat(data = tr, parameter = list(support =0.007, maxlen = 2,minlen = 2))
frequent_itemset_007_3<-eclat(data = tr, parameter = list(support =0.007, maxlen = 3,minlen = 3))

frequent_itemset_0011_1<-eclat(data = tr, parameter = list(support =0.011, maxlen = 1,minlen = 1))
frequent_itemset_0011_2<-eclat(data = tr, parameter = list(support =0.011, maxlen = 2,minlen = 2))
frequent_itemset_0011_3<-eclat(data = tr, parameter = list(support =0.011, maxlen = 3,minlen = 3))

frequent_itemset_004<-eclat(data = tr, parameter = list(support =0.004))
frequent_itemset_007<-eclat(data = tr, parameter = list(support =0.007))
frequent_itemset_011<-eclat(data = tr, parameter = list(support =0.011))
#======================================================TRYING NEW THING=========================================================
#1. C1

#2. L1
frequent_itemset_004_1<-eclat(data=tr, parameter = list(support =0.0001, maxlen = 1,minlen = 1))
#3. C2
df1 <-  select(inspect(frequent_itemset_004_1), items)
for (i in 1:nrow(df1))
{
  df1[i,]<-str_remove(df1[i,], "[{ }]")
  df1[i,]<-str_remove(df1[i,], "[ }]")
}
write.csv(df1,"c1.csv", quote = FALSE, row.names = FALSE)






#============================================================FINAL STEPS===================================
#1. L1
frequent_itemset_004_l1<-apriori(tr , parameter = list(support =0.004, maxlen = 1,minlen = 1, target="frequent itemset"))
#2. C2
retrive<- as.data.frame(frequent_itemset_004_l1@items@data@i)
names(retrive)[1]<-"items"
retrive1<- as.data.frame(frequent_itemset_004_l1@items@data@i)
names(retrive)[1]<-"items1"
pubsperauthor <- crossing(retrive$items1,retrive1$`frequent_itemset_004_l1@items@data@i`)
names(pubsperauthor)[2]<-"items1"
names(pubsperauthor)[1]<-"items"
calculate_candidate_2_2007 <- filter(pubsperauthor,  items< items1 &items!=0 & items1!=0 )
data_combine<-paste(pubsperauthor$items,',',pubsperauthor$items1)
write.csv(data_combine,"checker.csv", quote = FALSE, row.names = FALSE)

#3. L2
frequent_itemset_004_2<-eclat(data = tr, parameter = list(support =0.004, maxlen = 2,minlen = 2))
#4. C3
dfft <-  select(inspect(frequent_itemset_004_2), items)
for (i in 1:nrow(dfft))
{
  dfft[i,]<-str_remove(dfft[i,], "[{ }]")
  dfft[i,]<-str_remove(dfft[i,], "[ }]")
}
write.csv(dfft,"c2.csv", quote = FALSE, row.names = FALSE)
fdfft<-read.csv("c2.csv",sep=",")
fdfft1<-fdfft
names(fdfft1)[2]<-"items4"
names(fdfft1)[1]<-"items3"
names(fdfft)[2]<-"items2"
names(fdfft)[1]<-"items1"
demo_trial<- crossing(fdfft,fdfft1)
calculate_candidate_itemsets_3 <- filter(demo_trial,  items1==items3 &items2<items4)
calculate_candidate_itemsets_3<- subset(calculate_candidate_itemsets_3, select = -c(items3))
data_combine1<-paste(calculate_candidate_itemsets_3$items1,',',calculate_candidate_itemsets_3$items2,',',calculate_candidate_itemsets_3$items4)
write.csv(data_combine1,"checker1.csv", quote = FALSE, row.names = FALSE)

#5 L3
frequent_itemset_004_2<-eclat(data = tr, parameter = list(support =0.011, maxlen = 3,minlen = 3))
#4 C4
dfftt <-  select(inspect(frequent_itemset_004_2), items)
for (i in 1:nrow(dfftt))
{
  dfftt[i,]<-str_remove(dfftt[i,], "[{ }]")
  dfftt[i,]<-str_remove(dfftt[i,], "[ }]")
}
write.csv(dfftt,"c3.csv", quote = FALSE, row.names = FALSE)
fdfftt<-read.csv("c3.csv",sep=",")
fdfft1t<-fdfftt
names(fdfft1t)[3]<-"items6"
names(fdfft1t)[2]<-"items5"
names(fdfft1t)[1]<-"items4"
demo_trialt<- crossing(fdfftt,fdfft1t)
calculate_candidate_itemsets_4 <- filter(demo_trialt,  items1==items4 & items2==items5  & items3<items6)
calculate_candidate_itemsets_4<- subset(calculate_candidate_itemsets_4, select = -c(items4,items5))
data_combine1<-paste(calculate_candidate_itemsets_4$items1,',',calculate_candidate_itemsets_4$items2,',',calculate_candidate_itemsets_4$items3,calculate_candidate_itemsets_4$items6)
write.csv(data_combine1,"checker1.csv", quote = FALSE, row.names = FALSE)
#5 L4
frequent_itemset_004_2<-eclat(data = tr, parameter = list(support =0.011, maxlen = 4,minlen = 4))
#6 C5
dffttx <-  select(inspect(frequent_itemset_004_2), items)
for (i in 1:nrow(dffttx))
{
  dffttx[i,]<-str_remove(dffttx[i,], "[{ }]")
  dffttx[i,]<-str_remove(dffttx[i,], "[ }]")
}
write.csv(dffttx,"c4.csv", quote = FALSE, row.names = FALSE)
fdffttx<-read.csv("c4.csv",sep=",")
fdfft1tx<-fdffttx
names(fdfft1tx)[4]<-"items8"
names(fdfft1tx)[3]<-"items7"
names(fdfft1tx)[2]<-"items6"
names(fdfft1tx)[1]<-"items5"
demo_trialtx<-crossing(fdffttx,fdfft1tx)
calculate_candidate_itemsets_4x <- filter(demo_trialtx,  items1==items5 & items2==items6 &items3==items7  & items4<items8)
calculate_candidate_itemsets_4x<- subset(calculate_candidate_itemsets_4x, select = -c(items6,items7))
data_combine1x<-paste(calculate_candidate_itemsets_4x$items1,',',calculate_candidate_itemsets_4x$items2,',',calculate_candidate_itemsets_4x$items3,calculate_candidate_itemsets_4x$items4,calculate_candidate_itemsets_4x$items8)
write.csv(data_combine1x,"checker1.csv", quote = FALSE, row.names = FALSE)


frequent_itemset_004_2<-eclat(data = tr, parameter = list(support =0.011, maxlen = 5,minlen = 5))


#============================================================FINAL STEPS 2===================================
#1. L1
frequent_itemset_004_l1<-apriori(tr , parameter = list(support =0.011, maxlen = 1,minlen = 1, target="frequent itemset"))
#2. C2
retrive<- as.data.frame(frequent_itemset_004_l1@items@data@i)
names(retrive)[1]<-"items"
retrive1<- as.data.frame(frequent_itemset_004_l1@items@data@i)
names(retrive)[1]<-"items1"
pubsperauthor <- crossing(retrive$items1,retrive1$`frequent_itemset_004_l1@items@data@i`)
names(pubsperauthor)[2]<-"items1"
names(pubsperauthor)[1]<-"items"
calculate_candidate_2_2007 <- filter(pubsperauthor,  items< items1 &items!=0 & items1!=0 )
data_combine<-paste(pubsperauthor$items,',',pubsperauthor$items1)
write.csv(data_combine,"checker.csv", quote = FALSE, row.names = FALSE)

#3. L2
frequent_itemset_004_2<-eclat(data = tr, parameter = list(support =0.011, maxlen = 2,minlen = 2))
#4. C3
dfft <-  select(inspect(frequent_itemset_004_2), items)
for (i in 1:nrow(dfft))
{
  dfft[i,]<-str_remove(dfft[i,], "[{ }]")
  dfft[i,]<-str_remove(dfft[i,], "[ }]")
}
print(dfft[1,1])
write.csv(dfft,"c2.csv", quote = FALSE, row.names = FALSE)
fdfft<-read.csv("c2.csv",sep=",")
fdfft1<-fdfft
names(fdfft1)[2]<-"items4"
names(fdfft1)[1]<-"items3"
demo_trial<- crossing(fdfft,fdfft1)
calculate_candidate_itemsets_3 <- filter(demo_trial,  items1==items3 &items2<items4)
calculate_candidate_itemsets_3<- subset(calculate_candidate_itemsets_3, select = -c(items3))
data_combine1<-paste(calculate_candidate_itemsets_3$items1,',',calculate_candidate_itemsets_3$items2,',',calculate_candidate_itemsets_3$items4)
write.csv(data_combine1,"checker1.csv", quote = FALSE, row.names = FALSE)

#5 L3
frequent_itemset_004_2<-eclat(data = tr, parameter = list(support =0.011, maxlen = 3,minlen = 3))













frequent_itemset_004_l1<-apriori(tr , parameter = list(support =0.004, maxlen = 1,minlen = 1, target="frequent itemset"))
print(frequent_itemset_004_l1)

trans<-as(retrive, "transactions")
calculate_processed_hourly_2007 <- filter(pubsperauthor,  items< items1 &items!=0 & items1!=0 )
data_combine<-paste(pubsperauthor$items,',',pubsperauthor$items1)
write.csv(data_combine,"checker.csv", quote = FALSE, row.names = FALSE)
data2<-0
pubsperauthor<-as.data.frame(pubsperauthor)
for (i in 1:nrow(pubsperauthor))
{
  if(pubsperauthor$items[i]>=pubsperauthor$items1[i])
  {
    print(pubsperauthor$items[i])
    pubsperauthor<-pubsperauthor[-c(i),]
  }
}


for (i in 1:nrow(dfft))
{
  dfft[i,]<-str_remove(dfft[i,], "[{ }]")
  dfft[i,]<-str_remove(dfft[i,], "[ }]")
}






frequent_itemset_004_l2<-apriori(tr , parameter = list(support =0.004, maxlen = 2,minlen = 2, target="frequent itemset"))
print(frequent_itemset_004_l2)
write.csv(df1,"c2.csv", quote = FALSE, row.names = FALSE)

frequent_itemset_004_l3<-apriori(tr , parameter = list(support =0.004, maxlen = 3,minlen = 3, target="frequent itemset"))
print(frequent_itemset_004_l3)

frequent_itemset_004_1_f<-eclat(data = df1, parameter = list(support =0.004, maxlen = 1,minlen = 1))

df <-  select(inspect(frequent_itemset_004_2), items)
df1 <-  select(inspect(frequent_itemset_004_1), items)
df2<-0

  for(j in 1:nrow(df1))
  {
    trial2<-str_remove(df1[j,], "[{ }]")
    trial2<-str_remove(trial2, "[ }]")
    if(as.integer(trial1)<as.integer(trial2))
    {
      demo<-","
      string5<-paste(trial1,demo)
      string5<-paste(string5,trial2)
      df2<<- rbind(df2,string5)
    }
    
  }
  
}

#==============================================================================Rules==============================================================================
association_rules_1 <- apriori(tr, parameter = list(supp=0.004, conf=0.5,maxlen=10))
association_rules_2 <- apriori(tr, parameter = list(supp=0.004, conf=0.7,maxlen=10))
association_rules_3 <- apriori(tr, parameter = list(supp=0.004, conf=0.8,maxlen=10))
association_rules_4 <- apriori(tr, parameter = list(supp=0.007, conf=0.5,maxlen=10))
association_rules_5 <- apriori(tr, parameter = list(supp=0.007, conf=0.7,maxlen=10))
association_rules_6 <- apriori(tr, parameter = list(supp=0.007, conf=0.8,maxlen=10))
association_rules_7 <- apriori(tr, parameter = list(supp=0.011, conf=0.5,maxlen=10))
association_rules_8 <- apriori(tr, parameter = list(supp=0.011, conf=0.7,maxlen=10))
association_rules_9 <- apriori(tr, parameter = list(supp=0.011, conf=0.8,maxlen=10))
plotly_arules(association_rules_9)
print(association_rules_1)
print(association_rules_2)
print(association_rules_3)
print(association_rules_4)
print(association_rules_5)
print(association_rules_6)
print(association_rules_7)
print(association_rules_8)
print(association_rules_9)


#===========================================================================LIFT < 10======================================================================================
association_rules_lift_less_10_1 <- subset(association_rules_1, subset = lift < 10)
filtered_association_rules_lift_less_10_1<-head(sort(association_rules_lift_less_10_1, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_less_10_1)
association_rules_lift_less_10_2<- subset(association_rules_2,subset = lift < 10)
filtered_association_rules_lift_less_10_2<-head(sort(association_rules_lift_less_10_2, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_less_10_2)
association_rules_lift_less_10_3<- subset(association_rules_3, subset = lift < 10)
filtered_association_rules_lift_less_10_3<-head(sort(association_rules_lift_less_10_3, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_less_10_3)
association_rules_lift_less_10_4<- subset(association_rules_4, subset = lift < 10)
filtered_association_rules_lift_less_10_4<-head(sort(association_rules_lift_less_10_4, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_less_10_4)
association_rules_lift_less_10_5<- subset(association_rules_5, subset = lift < 10)
filtered_association_rules_lift_less_10_5<-head(sort(association_rules_lift_less_10_5, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_less_10_5)
association_rules_lift_less_10_6<- subset(association_rules_6, subset = lift < 10)
filtered_association_rules_lift_less_10_6<-head(sort(association_rules_lift_less_10_6, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_less_10_6)
association_rules_lift_less_10_7<- subset(association_rules_7, subset = lift < 10)
filtered_association_rules_lift_less_10_7<-head(sort(association_rules_lift_less_10_7, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_less_10_7)
association_rules_lift_less_10_8<- subset(association_rules_8, subset = lift < 10)
filtered_association_rules_lift_less_10_8<-head(sort(association_rules_lift_less_10_8, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_less_10_8)
association_rules_lift_less_10_9<- subset(association_rules_9, subset = lift < 10)
filtered_association_rules_lift_less_10_9<-head(sort(association_rules_lift_less_10_9, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_less_10_9)
print(association_rules_lift_less_10_1)
print(association_rules_lift_less_10_2)
print(association_rules_lift_less_10_3)
print(association_rules_lift_less_10_4)
print(association_rules_lift_less_10_5)
print(association_rules_lift_less_10_6)
print(association_rules_lift_less_10_7)
print(association_rules_lift_less_10_8)
print(association_rules_lift_less_10_9)


#===========================================================================LIFT < 10======================================================================================
association_rules_lift_more_10_1 <- subset(association_rules_1, subset = lift > 10)
filtered_association_rules_lift_more_10_1<-head(sort(association_rules_lift_more_10_1, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_more_10_1)
association_rules_lift_more_10_2<- subset(association_rules_2,subset = lift > 10)
filtered_association_rules_lift_more_10_2<-head(sort(association_rules_lift_more_10_2, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_more_10_2)
association_rules_lift_more_10_3<- subset(association_rules_3, subset = lift > 10)
filtered_association_rules_lift_more_10_3<-head(sort(association_rules_lift_more_10_3, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_more_10_3)

association_rules_lift_more_10_4<- subset(association_rules_4, subset = lift > 10)
filtered_association_rules_lift_more_10_4<-head(sort(association_rules_lift_more_10_4, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_more_10_4)

association_rules_lift_more_10_5<- subset(association_rules_5, subset = lift > 10)
filtered_association_rules_lift_more_10_5<-head(sort(association_rules_lift_more_10_5, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_more_10_5)

association_rules_lift_more_10_6<- subset(association_rules_6, subset = lift > 10)
filtered_association_rules_lift_more_10_6<-head(sort(association_rules_lift_more_10_6, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_more_10_6)

association_rules_lift_more_10_7<- subset(association_rules_7, subset = lift > 10)
filtered_association_rules_lift_more_10_7<-head(sort(association_rules_lift_more_10_7, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_more_10_7)

association_rules_lift_more_10_8<- subset(association_rules_8, subset = lift > 10)
filtered_association_rules_lift_more_10_8<-head(sort(association_rules_lift_more_10_8, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_more_10_8)

association_rules_lift_more_10_9<- subset(association_rules_9, subset = lift > 10)
filtered_association_rules_lift_more_10_9<-head(sort(association_rules_lift_more_10_9, by="lift", decreasing=TRUE),10)
inspect(filtered_association_rules_lift_more_10_9)

print(association_rules_lift_more_10_1)
print(association_rules_lift_more_10_2)
print(association_rules_lift_more_10_3)
print(association_rules_lift_more_10_4)
print(association_rules_lift_more_10_5)
print(association_rules_lift_more_10_6)
print(association_rules_lift_more_10_7)
print(association_rules_lift_more_10_8)
print(association_rules_lift_more_10_9)

#==================================================Descending order of top 100 items===================================================================

top100_association_rule_1<-head(sort(association_rules_1, by="confidence", decreasing=TRUE),100)

top100_association_rule_2<-head(sort(association_rules_2, by="confidence", decreasing=TRUE),100)
top100_association_rule_3<-head(sort(association_rules_3, by="confidence", decreasing=TRUE),100)
top100_association_rule_4<-head(sort(association_rules_4, by="confidence", decreasing=TRUE),100)
top100_association_rule_5<-head(sort(association_rules_5, by="confidence", decreasing=TRUE),100)
top100_association_rule_6<-head(sort(association_rules_6, by="confidence", decreasing=TRUE),100)
top100_association_rule_7<-head(sort(association_rules_7, by="confidence", decreasing=TRUE),100)
top100_association_rule_8<-head(sort(association_rules_8, by="confidence", decreasing=TRUE),100)
top100_association_rule_9<-head(sort(association_rules_9, by="confidence", decreasing=TRUE),100)








inspect(top100_association_rule_1)
plot(top100, method = "graph",  engine = "htmlwidget")
plotly_arules(top100)

plot(top100, method = "grouped matrix", engine = "interactive")


association_rules_lift_more_10_1 <- subset(association_rules_1, subset = lift > 10)
top10<-head(sort(association_rules_lift_more_10_1),10)
plotly_arules(top10)


