install.packages("lubridate")
install.packages("proxy")
install.packages("clusteval")
install.packages("factoextra")
install.packages("NbClust")
install.packages("cluster.datasets")
install.packages("MASS")
install.packages("cluster")
install.packages("ggplot2")
install.packages("lattice")
install.packages("rpart")
install.packages("caret")
install.packages("e1071")
install.packages("rpart.plot")
install.packages("clusteval")
install.packages("vegan")

library(lubridate)
library(proxy)
library(clusteval)
library(factoextra)
library(NbClust)
library(cluster.datasets)
library(MASS)
library(cluster)
library(ggplot2)
library(lattice)
library(rpart)
library(caret)
library(e1071)
library(rpart.plot)
library(dplyr)
library(vegan)

hourly_2007 <- read.csv(file='hourly_2007.g',sep='',header=TRUE)
hourly_2008 <- read.csv(file='hourly_2008.g',sep='',header=TRUE)
hourly_2009 <- read.csv(file='hourly_2009.g',sep='',header=TRUE)
write.csv(hourly_2007,'hourly_2007.csv')
write.csv(hourly_2008,'hourly_2008.csv')
write.csv(hourly_2009,'hourly_2009.csv')
hourly_2007$YEARMODA <- as.POSIXct(hourly_2007$YEARMODA,format="%Y%m%d_%H") 
hourly_2008$YEARMODA <- as.POSIXct(hourly_2008$YEARMODA,format="%Y%m%d_%H") 
hourly_2009$YEARMODA <- as.POSIXct(hourly_2009$YEARMODA,format="%Y%m%d_%H")
format(mydates,"%m")

edited_hourly_2007 <- tibble::as_tibble(hourly_2007)
print(edited_hourly_2007,n=20)
basic_processed_hourly_2007 <- filter(edited_hourly_2007, format(YEARMODA,"%m") == "09")
print(basic_processed_hourly_2007)

edited_hourly_2008 <- tibble::as_tibble(hourly_2008)
print(edited_hourly_2008,n=20)
basic_processed_hourly_2008 <- filter(edited_hourly_2008, format(YEARMODA,"%m") == "09")
print(basic_processed_hourly_2008)

edited_hourly_2009 <- tibble::as_tibble(hourly_2009)
print(edited_hourly_2009,n=20)
basic_processed_hourly_2009 <- filter(edited_hourly_2009, format(YEARMODA,"%m") == "09")
print(basic_processed_hourly_2009)

calculate_processed_hourly_2007 <- filter(basic_processed_hourly_2007, TEMP !=9999.9 & DEWP != 9999.9 & STP != 9999.9 & WDSP != 999.9 )
calculate_processed_hourly_2008 <- filter(basic_processed_hourly_2008, TEMP !=9999.9 & DEWP != 9999.9 & STP != 9999.9 & WDSP != 999.9 )
calculate_processed_hourly_2009 <- filter(basic_processed_hourly_2009, TEMP !=9999.9 & DEWP != 9999.9 & STP != 9999.9 & WDSP != 999.9 )

only_calculated_hourly_2007 <- calculate_processed_hourly_2007
only_calculated_hourly_2008 <- calculate_processed_hourly_2008
only_calculated_hourly_2009 <- calculate_processed_hourly_2009

only_calculated_hourly_month_2007 <- calculate_processed_hourly_2007
only_calculated_hourly_month_2008 <- calculate_processed_hourly_2008
only_calculated_hourly_month_2009 <- calculate_processed_hourly_2009


only_calculated_hourly_2007$YEARMODA <- day(only_calculated_hourly_2007$YEARMODA)
only_calculated_hourly_2008$YEARMODA <- day(only_calculated_hourly_2008$YEARMODA)
only_calculated_hourly_2009$YEARMODA <- day(only_calculated_hourly_2009$YEARMODA)

only_calculated_hourly_month_2007$YEARMODA <- month(only_calculated_hourly_month_2007$YEARMODA)
only_calculated_hourly_month_2008$YEARMODA <- month(only_calculated_hourly_month_2008$YEARMODA)
only_calculated_hourly_month_2009$YEARMODA <- month(only_calculated_hourly_month_2009$YEARMODA)

final_only_calculated_hourly_2007<-subset(only_calculated_hourly_2007,select=-c(WBAN,Count1,SLP,Count2,Count3,VISIB,Count4,Count5,MXSPD,GUST,PRCP,SNDP,FRSHTT))
final_only_calculated_hourly_2008<-subset(only_calculated_hourly_2008,select=-c(WBAN,Count1,SLP,Count2,Count3,VISIB,Count4,Count5,MXSPD,GUST,PRCP,SNDP,FRSHTT))
final_only_calculated_hourly_2009<-subset(only_calculated_hourly_2009,select=-c(WBAN,Count1,SLP,Count2,Count3,VISIB,Count4,Count5,MXSPD,GUST,PRCP,SNDP,FRSHTT))

final_only_calculated_hourly_month_2007<-subset(only_calculated_hourly_month_2007,select=-c(WBAN,Count1,SLP,Count2,Count3,VISIB,Count4,Count5,MXSPD,GUST,PRCP,SNDP,FRSHTT))
final_only_calculated_hourly_month_2008<-subset(only_calculated_hourly_month_2008,select=-c(WBAN,Count1,SLP,Count2,Count3,VISIB,Count4,Count5,MXSPD,GUST,PRCP,SNDP,FRSHTT))
final_only_calculated_hourly_month_2009<-subset(only_calculated_hourly_month_2009,select=-c(WBAN,Count1,SLP,Count2,Count3,VISIB,Count4,Count5,MXSPD,GUST,PRCP,SNDP,FRSHTT))

computing_hourly_daily_2007 =aggregate(final_only_calculated_hourly_2007[,3:6],by=list(final_only_calculated_hourly_2007$STN...,final_only_calculated_hourly_2007$YEARMODA), FUN=mean)
computing_hourly_daily_2008 =aggregate(final_only_calculated_hourly_2008[,3:6],by=list(final_only_calculated_hourly_2008$STN...,final_only_calculated_hourly_2008$YEARMODA), FUN=mean)
computing_hourly_daily_2009 =aggregate(final_only_calculated_hourly_2009[,3:6],by=list(final_only_calculated_hourly_2009$STN...,final_only_calculated_hourly_2009$YEARMODA), FUN=mean)

computing_hourly_month_2007 =aggregate(final_only_calculated_hourly_month_2007[,3:6],by=list(final_only_calculated_hourly_month_2007$STN...,final_only_calculated_hourly_month_2007$YEARMODA), FUN=mean)
computing_hourly_month_2008 =aggregate(final_only_calculated_hourly_month_2008[,3:6],by=list(final_only_calculated_hourly_month_2008$STN...,final_only_calculated_hourly_month_2008$YEARMODA), FUN=mean)
computing_hourly_month_2009 =aggregate(final_only_calculated_hourly_month_2009[,3:6],by=list(final_only_calculated_hourly_month_2009$STN...,final_only_calculated_hourly_month_2009$YEARMODA), FUN=mean)

names(computing_hourly_month_2007)[names(computing_hourly_month_2007)== "Group.1"] <- "STN..."
names(computing_hourly_month_2007)[names(computing_hourly_month_2007)== "Group.2"] <- "Date"
names(computing_hourly_month_2008)[names(computing_hourly_month_2008)== "Group.1"] <- "STN..."
names(computing_hourly_month_2008)[names(computing_hourly_month_2008)== "Group.2"] <- "Date"
names(computing_hourly_month_2009)[names(computing_hourly_month_2009)== "Group.1"] <- "STN..."
names(computing_hourly_month_2009)[names(computing_hourly_month_2009)== "Group.2"] <- "Date"

names(computing_hourly_daily_2007)[names(computing_hourly_daily_2007)== "Group.1"] <- "STN..."
names(computing_hourly_daily_2007)[names(computing_hourly_daily_2007)== "Group.2"] <- "Date"
names(computing_hourly_daily_2008)[names(computing_hourly_daily_2008)== "Group.1"] <- "STN..."
names(computing_hourly_daily_2008)[names(computing_hourly_daily_2008)== "Group.2"] <- "Date"
names(computing_hourly_daily_2009)[names(computing_hourly_daily_2009)== "Group.1"] <- "STN..."
names(computing_hourly_daily_2009)[names(computing_hourly_daily_2009)== "Group.2"] <- "Date"


write.csv(computing_hourly_daily_2007,'computing_hourly_daily_2007.csv')
write.csv(computing_hourly_daily_2008,'computing_hourly_daily_2008.csv')
write.csv(computing_hourly_daily_2009,'computing_hourly_daily_2009.csv')

write.csv(computing_hourly_month_2007,'computing_hourly_daily_month_2007.csv')
write.csv(computing_hourly_month_2008,'computing_hourly_daily_month_2008.csv')
write.csv(computing_hourly_month_2009,'computing_hourly_daily_month_2009.csv')

data_used_for_kmeam_hourly_daily_2007<-subset(computing_hourly_daily_2007,select= -c(Date))
data_used_for_kmeam_hourly_daily_2008<-subset(computing_hourly_daily_2008,select= -c(Date))
data_used_for_kmeam_hourly_daily_2009<-subset(computing_hourly_daily_2009,select= -c(Date))

write.csv(data_used_for_kmeam_hourly_daily_2007,'data_used_for_kmeam_hourly_daily_2007.csv')
write.csv(data_used_for_kmeam_hourly_daily_2008,'data_used_for_kmeam_hourly_daily_2008.csv')
write.csv(data_used_for_kmeam_hourly_daily_2009,'data_used_for_kmeam_hourly_daily_2009.csv')

data_used_for_kmeam_hourly_month_2007<-subset(computing_hourly_month_2007,select= -c(Date))
data_used_for_kmeam_hourly_month_2008<-subset(computing_hourly_month_2008,select= -c(Date))
data_used_for_kmeam_hourly_month_2009<-subset(computing_hourly_month_2009,select= -c(Date))

write.csv(data_used_for_kmeam_hourly_month_2007,'data_used_for_kmeam_hourly_month_2007.csv')
write.csv(data_used_for_kmeam_hourly_month_2008,'data_used_for_kmeam_hourly_month_2008.csv')
write.csv(data_used_for_kmeam_hourly_month_2009,'data_used_for_kmeam_hourly_month_2009.csv')


# 
# monthly_coverage_mean_2007_TEMP<- calculate_processed_hourly_2007 %>%
#        mutate(date = month(YEARMODA),STN...) %>%
#        group_by(date) %>%
#        summarize(mean_montly_average = mean(TEMP))
# daily_average_coverage_mean_2007_TEMP <- calculate_processed_hourly_2007 %>%
#        mutate(date = day(floor_date(YEARMODA)),STN...) %>%
#        group_by(date) %>%
#        summarize(mean_montly_average = mean(TEMP))
# 
# monthly_coverage_mean_2008_TEMP<- calculate_processed_hourly_2008 %>%
#        mutate(date = month(YEARMODA)) %>%
#        group_by(date) %>%
#        summarize(mean_montly_average = mean(TEMP))
# daily_average_coverage_mean_2008_TEMP <- calculate_processed_hourly_2008 %>%
#        mutate(date = day(floor_date(YEARMODA))) %>%
#        group_by(date) %>%
#       summarize(mean_montly_average = mean(TEMP))
# 
# monthly_coverage_mean_2009_TEMP<- calculate_processed_hourly_2009 %>%
#        mutate(date = month(YEARMODA)) %>%
#        group_by(date) %>%
#       summarize(mean_montly_average = mean(TEMP))
# daily_average_coverage_mean_2009_TEMP <- calculate_processed_hourly_2009 %>%
#       mutate(date = day(floor_date(YEARMODA))) %>%
#        group_by(date) %>%
#        summarize(mean_montly_average = mean(TEMP))
# 
# 
# 
# monthly_coverage_mean_2007_STP<- calculate_processed_hourly_2007 %>%
#   mutate(date = month(YEARMODA)) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(STP))
# daily_average_coverage_mean_2007_STP <- calculate_processed_hourly_2007 %>%
#   mutate(date = day(floor_date(YEARMODA))) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(STP))
# 
# monthly_coverage_mean_2008_STP<- calculate_processed_hourly_2008 %>%
#   mutate(date = month(YEARMODA)) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(STP))
# daily_average_coverage_mean_2008_STP <- calculate_processed_hourly_2008 %>%
#   mutate(date = day(floor_date(YEARMODA))) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(STP))
# 
# monthly_coverage_mean_2009_STP<- calculate_processed_hourly_2009 %>%
#   mutate(date = month(YEARMODA)) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(STP))
# daily_average_coverage_mean_2009_STP <- calculate_processed_hourly_2009 %>%
#   mutate(date = day(floor_date(YEARMODA))) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(STP))
# 
# 
# monthly_coverage_mean_2007_DEWP<- calculate_processed_hourly_2007 %>%
#   mutate(date = month(YEARMODA)) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(DEWP))
# daily_average_coverage_mean_2007_DEWP <- calculate_processed_hourly_2007 %>%
#   mutate(date = day(floor_date(YEARMODA))) %>%
#   group_by(date) %>%
#   summarize(STN...,mean_montly_average = mean(DEWP))
# 
# monthly_coverage_mean_2008_DEWP<- calculate_processed_hourly_2008 %>%
#   mutate(date = month(YEARMODA)) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(DEWP))
# daily_average_coverage_mean_2008_DEWP <- calculate_processed_hourly_2008 %>%
#   mutate(date = day(floor_date(YEARMODA))) %>%
#   group_by(STN...) %>%
#   summarize(STN..,mean_montly_average = mean(DEWP))
# 
# monthly_coverage_mean_2009_DEWP<- calculate_processed_hourly_2009 %>%
#   mutate(date = month(YEARMODA)) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(DEWP))
# daily_average_coverage_mean_2009_DEWP <- calculate_processed_hourly_2009 %>%
#   mutate(mean(DEWP)) %>%
#   summarize(mean_montly_average = mean(DEWP))
# 
#   
# 
# monthly_coverage_mean_2007_WDSP<- calculate_processed_hourly_2007 %>%
#   mutate(date = month(YEARMODA)) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(WDSP))
# daily_average_coverage_mean_2007_WDSP <- calculate_processed_hourly_2007 %>%
#   mutate(date = day(floor_date(YEARMODA))) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(WDSP))
# 
# monthly_coverage_mean_2008_WDSP<- calculate_processed_hourly_2008 %>%
#   mutate(date = month(YEARMODA)) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(WDSP))
# daily_average_coverage_mean_2008_WDSP <- calculate_processed_hourly_2008 %>%
#   mutate(date = day(floor_date(YEARMODA))) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(WDSP))
# 
# monthly_coverage_mean_2009_WDSP<- calculate_processed_hourly_2009 %>%
#   mutate(date = month(YEARMODA)) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(WDSP))
# daily_average_coverage_mean_2009_WDSP <- calculate_processed_hourly_2009 %>%
#   mutate(date = day(floor_date(YEARMODA))) %>%
#   group_by(date) %>%
#   summarize(mean_montly_average = mean(WDSP))
# summary(calculate_processed_hourly_2007)
# summary(calculate_processed_hourly_2008)
# summary(calculate_processed_hourly_2009)
# print(monthly_coverage_mean_2007_TEMP)
# print(monthly_coverage_mean_2008_TEMP)
# print(monthly_coverage_mean_2009_TEMP)
# 
# print(daily_average_coverage_mean_2007_TEMP)
# print(daily_average_coverage_mean_2008_TEMP)
# print(daily_average_coverage_mean_2009_TEMP)
# 
# print(monthly_coverage_mean_2007_WDSP)
# print(monthly_coverage_mean_2008_WDSP)
# print(monthly_coverage_mean_2009_WDSP)
# 
# print(daily_average_coverage_mean_2007_WDSP)
# print(daily_average_coverage_mean_2008_WDSP)
# print(daily_average_coverage_mean_2009_WDSP)
# 
# print(monthly_coverage_mean_2007_STP)
# print(monthly_coverage_mean_2008_STP)
# print(monthly_coverage_mean_2009_STP)
# 
# print(daily_average_coverage_mean_2007_STP)
# print(daily_average_coverage_mean_2008_STP)
# print(daily_average_coverage_mean_2009_STP)
# 
# print(monthly_coverage_mean_2007_DEWP)
# print(monthly_coverage_mean_2008_DEWP)
# print(monthly_coverage_mean_2009_DEWP)
# 
# print(daily_average_coverage_mean_2007_DEWP)
# print(daily_average_coverage_mean_2008_DEWP)
# print(daily_average_coverage_mean_2009_DEWP)
# 
# 

#set.seed(1000)

#=================================K-MEAN====================================
data_used_for_kmean_hourly_month_2007_scaled <- scale(data_used_for_kmeam_hourly_month_2007[,-1])
data_used_for_kmean_hourly_month_2008_scaled <- scale(data_used_for_kmeam_hourly_month_2008[,-1])
data_used_for_kmean_hourly_month_2009_scaled <- scale(data_used_for_kmeam_hourly_month_2009[,-1])

data_used_for_kmean_hourly_daily_2007_scaled <- scale(data_used_for_kmeam_hourly_daily_2007[,-1])
data_used_for_kmean_hourly_daily_2008_scaled <- scale(data_used_for_kmeam_hourly_daily_2008[,-1])
data_used_for_kmean_hourly_daily_2009_scaled <- scale(data_used_for_kmeam_hourly_daily_2009[,-1])

fviz_nbclust(data_used_for_kmean_hourly_month_2007_scaled , kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

fviz_nbclust(data_used_for_kmean_hourly_month_2008_scaled , kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

fviz_nbclust(data_used_for_kmean_hourly_month_2009_scaled , kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

fviz_nbclust(data_used_for_kmean_hourly_daily_2007_scaled , kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

fviz_nbclust(data_used_for_kmean_hourly_daily_2008_scaled , kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

fviz_nbclust(data_used_for_kmean_hourly_daily_2009_scaled , kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

kmeansvalue_ks.res_month_2007 <- kmeans(data_used_for_kmean_hourly_month_2007_scaled , 3, nstart = 25)
kmeansvalue_ks.res_month_2008 <- kmeans(data_used_for_kmean_hourly_month_2008_scaled , 3, nstart = 25)
kmeansvalue_ks.res_month_2009 <- kmeans(data_used_for_kmean_hourly_month_2009_scaled , 3, nstart = 25)

kmeansvalue_ks.res_daily_2007 <- kmeans(data_used_for_kmean_hourly_daily_2007_scaled , 3, nstart = 25)
kmeansvalue_ks.res_daily_2008 <- kmeans(data_used_for_kmean_hourly_daily_2008_scaled , 3, nstart = 25)
kmeansvalue_ks.res_daily_2009 <- kmeans(data_used_for_kmean_hourly_daily_2009_scaled , 3, nstart = 25)

euclidean_distance_of_month_2007 <- dist(data_used_for_kmean_hourly_month_2007_scaled, method = "euclidean") 
euclidean_distance_of_month_2008 <- dist(data_used_for_kmean_hourly_month_2008_scaled, method = "euclidean") 
euclidean_distance_of_month_2009 <- dist(data_used_for_kmean_hourly_month_2009_scaled, method = "euclidean") 

euclidean_distance_of_daily_2007 <- dist(data_used_for_kmean_hourly_daily_2007_scaled, method = "euclidean") 
euclidean_distance_of_daily_2008 <- dist(data_used_for_kmean_hourly_daily_2008_scaled, method = "euclidean") 
euclidean_distance_of_daily_2009 <- dist(data_used_for_kmean_hourly_daily_2009_scaled, method = "euclidean")


pearson_distance_of_month_2007 <-get_dist(data_used_for_kmean_hourly_month_2007_scaled, method = "pearson") 
pearson_distance_of_month_2008 <-get_dist(data_used_for_kmean_hourly_month_2008_scaled, method = "pearson") 
pearson_distance_of_month_2009 <-get_dist(data_used_for_kmean_hourly_month_2009_scaled, method = "pearson") 

pearson_distance_of_daily_2007 <-get_dist(data_used_for_kmean_hourly_daily_2007_scaled, method = "pearson") 
pearson_distance_of_daily_2008 <-get_dist(data_used_for_kmean_hourly_daily_2008_scaled, method = "pearson") 
pearson_distance_of_daily_2009 <-get_dist(data_used_for_kmean_hourly_daily_2009_scaled, method = "pearson")

fviz_dist(euclidean_distance_of_month_2007)
fviz_dist(euclidean_distance_of_month_2008)
fviz_dist(euclidean_distance_of_month_2009)
fviz_dist(euclidean_distance_of_daily_2007)
fviz_dist(euclidean_distance_of_daily_2008)
fviz_dist(euclidean_distance_of_daily_2009)

fviz_dist(pearson_distance_of_month_2007)
fviz_dist(pearson_distance_of_month_2008)
fviz_dist(pearson_distance_of_month_2009)
fviz_dist(pearson_distance_of_daily_2007)
fviz_dist(pearson_distance_of_daily_2008)
fviz_dist(pearson_distance_of_daily_2009)

matrix_of_euclidean_distance_of_month_2007<- as.matrix(euclidean_distance_of_month_2007)
matrix_of_euclidean_distance_of_month_2008<- as.matrix(euclidean_distance_of_month_2008)
matrix_of_euclidean_distance_of_month_2009<- as.matrix(euclidean_distance_of_month_2009)

matrix_of_euclidean_distance_of_daily_2007<- as.matrix(euclidean_distance_of_daily_2007)
matrix_of_euclidean_distance_of_daily_2008<- as.matrix(euclidean_distance_of_daily_2008)
matrix_of_euclidean_distance_of_daily_2009<- as.matrix(euclidean_distance_of_daily_2009)


matrix_of_pearson_distance_of_daily_2007<- as.matrix(pearson_distance_of_daily_2007)
matrix_of_pearson_distance_of_daily_2008<- as.matrix(pearson_distance_of_daily_2008)
matrix_of_pearson_distance_of_daily_2009<- as.matrix(pearson_distance_of_daily_2009)

matrix_of_pearson_distance_of_month_2007<- as.matrix(pearson_distance_of_month_2007)
matrix_of_pearson_distance_of_month_2008<- as.matrix(pearson_distance_of_month_2008)
matrix_of_pearson_distance_of_month_2009<- as.matrix(pearson_distance_of_month_2009)

# #y1: ecludiean month , y2: pearson month, y3: euclidian_vs_pearson_2007_month
# 
# #y1 and y2
# euclidian_vs_pearson_2007_month <- proxy::dist(matrix_of_euclidean_distance_of_month_2007,matrix_of_pearson_distance_of_month_2007, by_rows = TRUE, method ="Jaccard")
# #y2 and y3 
# pearson_vs_pearson_2007__daily_month <- proxy::dist(matrix_of_pearson_distance_of_month_2007,euclidian_vs_pearson_2007_month, by_rows = TRUE, method ="Jaccard")
#---------------------------Jaccard-----------------------------------------
kmeansvalue_ks.res_month_2007_TEMP <- kmeans(data_used_for_kmean_hourly_month_2007_scaled[,1] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2007_TEMP)
kmeansvalue_ks.res_month_2007_TEMP_cluster <- kmeansvalue_ks.res_month_2007_TEMP$cluster
kmeansvalue_ks.res_month_2007_TEMP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2007_TEMP_cluster)
print(kmeansvalue_ks.res_month_2007_TEMP_cluster_matrix)

kmeansvalue_ks.res_month_2007_DEWP <- kmeans(data_used_for_kmean_hourly_month_2007_scaled[,2] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2007_DEWP)
kmeansvalue_ks.res_month_2007_DEWP_cluster <- kmeansvalue_ks.res_month_2007_DEWP$cluster
kmeansvalue_ks.res_month_2007_DEWP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2007_DEWP_cluster)
print(kmeansvalue_ks.res_month_2007_DEWP_cluster_matrix)

kmeansvalue_ks.res_month_2007_STP <- kmeans(data_used_for_kmean_hourly_month_2007_scaled[,3] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2007_STP)
kmeansvalue_ks.res_month_2007_STP_cluster <- kmeansvalue_ks.res_month_2007_STP$cluster
kmeansvalue_ks.res_month_2007_STP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2007_STP_cluster)
print(kmeansvalue_ks.res_month_2007_STP_cluster_matrix)


kmeansvalue_ks.res_month_2007_WDSP <- kmeans(data_used_for_kmean_hourly_month_2007_scaled[,4] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2007_WDSP)
kmeansvalue_ks.res_month_2007_WDSP_cluster <- kmeansvalue_ks.res_month_2007_WDSP$cluster
kmeansvalue_ks.res_month_2007_WDSP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2007_WDSP_cluster)
print(kmeansvalue_ks.res_month_2007_WDSP_cluster_matrix)

kmeansvalue_ks.res_daily_2007_TEMP <- kmeans(data_used_for_kmean_hourly_daily_2007_scaled[,1] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2007_TEMP)
kmeansvalue_ks.res_daily_2007_TEMP_cluster <- kmeansvalue_ks.res_daily_2007_TEMP$cluster
kmeansvalue_ks.res_daily_2007_TEMP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2007_TEMP_cluster)
print(kmeansvalue_ks.res_daily_2007_TEMP_cluster_matrix)

kmeansvalue_ks.res_daily_2007_DEWP <- kmeans(data_used_for_kmean_hourly_daily_2007_scaled[,2] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2007_DEWP)
kmeansvalue_ks.res_daily_2007_DEWP_cluster <- kmeansvalue_ks.res_daily_2007_DEWP$cluster
kmeansvalue_ks.res_daily_2007_DEWP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2007_DEWP_cluster)
print(kmeansvalue_ks.res_daily_2007_DEWP_cluster_matrix)

kmeansvalue_ks.res_daily_2007_STP <- kmeans(data_used_for_kmean_hourly_daily_2007_scaled[,3] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2007_STP)
kmeansvalue_ks.res_daily_2007_STP_cluster <- kmeansvalue_ks.res_daily_2007_STP$cluster
kmeansvalue_ks.res_daily_2007_STP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2007_STP_cluster)
print(kmeansvalue_ks.res_daily_2007_STP_cluster_matrix)

kmeansvalue_ks.res_daily_2007_WDSP <- kmeans(data_used_for_kmean_hourly_daily_2007_scaled[,4] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2007_WDSP)
kmeansvalue_ks.res_daily_2007_WDSP_cluster <- kmeansvalue_ks.res_daily_2007_WDSP$cluster
kmeansvalue_ks.res_daily_2007_WDSP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2007_WDSP_cluster)
print(kmeansvalue_ks.res_daily_2007_WDSP_cluster_matrix)

kmeansvalue_ks.res_month_2008_TEMP <- kmeans(data_used_for_kmean_hourly_month_2008_scaled[,1] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2008_TEMP)
kmeansvalue_ks.res_month_2008_TEMP_cluster <- kmeansvalue_ks.res_month_2008_TEMP$cluster
kmeansvalue_ks.res_month_2008_TEMP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2008_TEMP_cluster)
print(kmeansvalue_ks.res_month_2008_TEMP_cluster_matrix)

kmeansvalue_ks.res_month_2008_DEWP <- kmeans(data_used_for_kmean_hourly_month_2008_scaled[,2] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2008_DEWP)
kmeansvalue_ks.res_month_2008_DEWP_cluster <- kmeansvalue_ks.res_month_2008_DEWP$cluster
kmeansvalue_ks.res_month_2008_DEWP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2008_DEWP_cluster)
print(kmeansvalue_ks.res_month_2008_DEWP_cluster_matrix)

kmeansvalue_ks.res_month_2008_STP <- kmeans(data_used_for_kmean_hourly_month_2008_scaled[,3] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2008_STP)
kmeansvalue_ks.res_month_2008_STP_cluster <- kmeansvalue_ks.res_month_2008_STP$cluster
kmeansvalue_ks.res_month_2008_STP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2008_STP_cluster)
print(kmeansvalue_ks.res_month_2008_STP_cluster_matrix)

kmeansvalue_ks.res_month_2008_WDSP <- kmeans(data_used_for_kmean_hourly_month_2008_scaled[,4] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2008_WDSP)
kmeansvalue_ks.res_month_2008_WDSP_cluster <- kmeansvalue_ks.res_month_2008_WDSP$cluster
kmeansvalue_ks.res_month_2008_WDSP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2008_WDSP_cluster)
print(kmeansvalue_ks.res_month_2008_WDSP_cluster_matrix)

kmeansvalue_ks.res_daily_2008_TEMP <- kmeans(data_used_for_kmean_hourly_daily_2008_scaled[,1] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2008_TEMP)
kmeansvalue_ks.res_daily_2008_TEMP_cluster <- kmeansvalue_ks.res_daily_2008_TEMP$cluster
kmeansvalue_ks.res_daily_2008_TEMP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2008_TEMP_cluster)
print(kmeansvalue_ks.res_daily_2008_TEMP_cluster_matrix)

kmeansvalue_ks.res_daily_2008_DEWP <- kmeans(data_used_for_kmean_hourly_daily_2008_scaled[,2] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2008_DEWP)
kmeansvalue_ks.res_daily_2008_DEWP_cluster <- kmeansvalue_ks.res_daily_2008_DEWP$cluster
kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2008_DEWP_cluster)
print(kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix)

kmeansvalue_ks.res_daily_2008_STP <- kmeans(data_used_for_kmean_hourly_daily_2008_scaled[,3] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2008_STP)
kmeansvalue_ks.res_daily_2008_STP_cluster <- kmeansvalue_ks.res_daily_2008_STP$cluster
kmeansvalue_ks.res_daily_2008_STP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2008_STP_cluster)
print(kmeansvalue_ks.res_daily_2008_STP_cluster_matrix)

kmeansvalue_ks.res_daily_2008_WDSP <- kmeans(data_used_for_kmean_hourly_daily_2008_scaled[,4] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2008_WDSP)
kmeansvalue_ks.res_daily_2008_WDSP_cluster <- kmeansvalue_ks.res_daily_2008_WDSP$cluster
kmeansvalue_ks.res_daily_2008_WDSP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2008_WDSP_cluster)
print(kmeansvalue_ks.res_daily_2008_WDSP_cluster_matrix)

kmeansvalue_ks.res_month_2009_TEMP <- kmeans(data_used_for_kmean_hourly_month_2009_scaled[,1] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2009_TEMP)
kmeansvalue_ks.res_month_2009_TEMP_cluster <- kmeansvalue_ks.res_month_2009_TEMP$cluster
kmeansvalue_ks.res_month_2009_TEMP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2009_TEMP_cluster)
print(kmeansvalue_ks.res_month_2009_TEMP_cluster_matrix)

kmeansvalue_ks.res_month_2009_DEWP <- kmeans(data_used_for_kmean_hourly_month_2009_scaled[,2] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2009_DEWP)
kmeansvalue_ks.res_month_2009_DEWP_cluster <- kmeansvalue_ks.res_month_2009_DEWP$cluster
kmeansvalue_ks.res_month_2009_DEWP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2009_DEWP_cluster)
print(kmeansvalue_ks.res_month_2009_DEWP_cluster_matrix)

kmeansvalue_ks.res_month_2009_STP <- kmeans(data_used_for_kmean_hourly_month_2009_scaled[,3] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2009_STP)
kmeansvalue_ks.res_month_2009_STP_cluster <- kmeansvalue_ks.res_month_2009_STP$cluster
kmeansvalue_ks.res_month_2009_STP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2009_STP_cluster)
print(kmeansvalue_ks.res_month_2009_STP_cluster_matrix)

kmeansvalue_ks.res_month_2009_WDSP <- kmeans(data_used_for_kmean_hourly_month_2009_scaled[,4] , 3, nstart = 25)
print(kmeansvalue_ks.res_month_2009_WDSP)
kmeansvalue_ks.res_month_2009_WDSP_cluster <- kmeansvalue_ks.res_month_2009_WDSP$cluster
kmeansvalue_ks.res_month_2009_WDSP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2009_WDSP_cluster)
print(kmeansvalue_ks.res_month_2009_WDSP_cluster_matrix)

kmeansvalue_ks.res_daily_2009_TEMP <- kmeans(data_used_for_kmean_hourly_daily_2009_scaled[,1] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2009_TEMP)
kmeansvalue_ks.res_daily_2009_TEMP_cluster <- kmeansvalue_ks.res_daily_2009_TEMP$cluster
kmeansvalue_ks.res_daily_2009_TEMP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2009_TEMP_cluster)
print(kmeansvalue_ks.res_daily_2009_TEMP_cluster_matrix)

kmeansvalue_ks.res_daily_2009_DEWP <- kmeans(data_used_for_kmean_hourly_daily_2009_scaled[,2] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2009_DEWP)
kmeansvalue_ks.res_daily_2009_DEWP_cluster <- kmeansvalue_ks.res_daily_2009_DEWP$cluster
kmeansvalue_ks.res_daily_2009_DEWP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2009_DEWP_cluster)
print(kmeansvalue_ks.res_daily_2009_DEWP_cluster_matrix)

kmeansvalue_ks.res_daily_2009_STP <- kmeans(data_used_for_kmean_hourly_daily_2009_scaled[,3] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2009_STP)
kmeansvalue_ks.res_daily_2009_STP_cluster <- kmeansvalue_ks.res_daily_2009_STP$cluster
kmeansvalue_ks.res_daily_2009_STP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2009_STP_cluster)
print(kmeansvalue_ks.res_daily_2009_STP_cluster_matrix)

kmeansvalue_ks.res_daily_2009_WDSP <- kmeans(data_used_for_kmean_hourly_daily_2009_scaled[,4] , 3, nstart = 25)
print(kmeansvalue_ks.res_daily_2009_WDSP)
kmeansvalue_ks.res_daily_2009_WDSP_cluster <- kmeansvalue_ks.res_daily_2009_WDSP$cluster
kmeansvalue_ks.res_daily_2009_WDSP_cluster_matrix <- as.matrix(kmeansvalue_ks.res_daily_2009_WDSP_cluster)
print(kmeansvalue_ks.res_daily_2009_WDSP_cluster_matrix)

#----------- Making new variables from k-means to use it in Jaccard calculation ------------

refined_kmeansvalue_ks.res_daily_2007_DEWP_cluster_matrix<-kmeansvalue_ks.res_daily_2007_DEWP_cluster_matrix
refined_kmeansvalue_ks.res_daily_2007_TEMP_cluster_matrix<-kmeansvalue_ks.res_daily_2007_TEMP_cluster_matrix
refined_kmeansvalue_ks.res_daily_2007_STP_cluster_matrix<-kmeansvalue_ks.res_daily_2007_STP_cluster_matrix
refined_kmeansvalue_ks.res_daily_2007_WDSP_cluster_matrix<-kmeansvalue_ks.res_daily_2007_WDSP_cluster_matrix

refined_kmeansvalue_ks.res_month_2007_DEWP_cluster_matrix<-kmeansvalue_ks.res_month_2007_DEWP_cluster_matrix
refined_kmeansvalue_ks.res_month_2007_TEMP_cluster_matrix<-kmeansvalue_ks.res_month_2007_TEMP_cluster_matrix
refined_kmeansvalue_ks.res_month_2007_STP_cluster_matrix<-kmeansvalue_ks.res_month_2007_STP_cluster_matrix
refined_kmeansvalue_ks.res_month_2007_WDSP_cluster_matrix<-kmeansvalue_ks.res_month_2007_WDSP_cluster_matrix


refined_kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix[-(3759:3876),])
refined_kmeansvalue_ks.res_daily_2008_TEMP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_daily_2008_TEMP_cluster_matrix[-(3759:3876),])
refined_kmeansvalue_ks.res_daily_2008_STP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_daily_2008_STP_cluster_matrix[-(3759:3876),])
refined_kmeansvalue_ks.res_daily_2008_WDSP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_daily_2008_WDSP_cluster_matrix[-(3759:3876),])

refined_kmeansvalue_ks.res_daily_2009_DEWP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_daily_2009_DEWP_cluster_matrix[-(3759:4020),])
refined_kmeansvalue_ks.res_daily_2009_TEMP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_daily_2009_TEMP_cluster_matrix[-(3759:4020),])
refined_kmeansvalue_ks.res_daily_2009_STP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_daily_2009_STP_cluster_matrix[-(3759:4020),])
refined_kmeansvalue_ks.res_daily_2009_WDSP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_daily_2009_WDSP_cluster_matrix[-(3759:4020),])

refined_kmeansvalue_ks.res_month_2008_DEWP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_month_2008_DEWP_cluster_matrix[-(129:135),])
refined_kmeansvalue_ks.res_month_2008_TEMP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_month_2008_TEMP_cluster_matrix[-(129:135),])
refined_kmeansvalue_ks.res_month_2008_STP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_month_2008_STP_cluster_matrix[-(129:135),])
refined_kmeansvalue_ks.res_month_2008_WDSP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_month_2008_WDSP_cluster_matrix[-(129:135),])

refined_kmeansvalue_ks.res_month_2009_DEWP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_month_2009_DEWP_cluster_matrix[-(129:137),])
refined_kmeansvalue_ks.res_month_2009_TEMP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_month_2009_TEMP_cluster_matrix[-(129:137),])
refined_kmeansvalue_ks.res_month_2009_STP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_month_2009_STP_cluster_matrix[-(129:137),])
refined_kmeansvalue_ks.res_month_2009_WDSP_cluster_matrix<-as.matrix( kmeansvalue_ks.res_month_2009_WDSP_cluster_matrix[-(129:137),])

#-------------------JACCARD QUESTION 5 -------------------------
jaccard_indep <- function(labels1, labels2) {
  com_table <- comembership_table(labels1, labels2)
  jaccard_out <- with(com_table, n_11 / (n_11 + n_10 + n_01))
  
  # In the case where 'labels1' and 'labels2' contain all singletons, the Jaccard
  # coefficient results in the expression 0 / 0, which yields a NaN value in R.
  # We define such cases as 0.
  if (is.nan(jaccard_out)) {
    warning("The two clusterings contain all singletons -- returning 0.")
    jaccard_out <- 0
  }
  jaccard_out
}
#2007
jaccard_TEMP_DEWP_daily_2007<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2007_DEWP_cluster_matrix)
jaccard_TEMP_STP_daily_2007<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2007_STP_cluster_matrix)
jaccard_TEMP_WDSP_daily_2007<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2007_WDSP_cluster_matrix)
jaccard_DEWP_STP_daily_2007<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2007_STP_cluster_matrix)
jaccard_DEWP_WDSP_daily_2007<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2007_WDSP_cluster_matrix)
jaccard_STP_WDSP_daily_2007<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_STP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2007_WDSP_cluster_matrix)
#2008
jaccard_TEMP_DEWP_daily_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix)
jaccard_TEMP_STP_daily_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_STP_cluster_matrix)
jaccard_TEMP_WDSP_daily_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_WDSP_cluster_matrix)
jaccard_DEWP_STP_daily_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_STP_cluster_matrix)
jaccard_DEWP_WDSP_daily_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_WDSP_cluster_matrix)
jaccard_STP_WDSP_daily_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_STP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_WDSP_cluster_matrix)
#2009
jaccard_TEMP_DEWP_daily_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2009_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_DEWP_cluster_matrix)
jaccard_TEMP_STP_daily_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2009_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_STP_cluster_matrix)
jaccard_TEMP_WDSP_daily_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2009_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_WDSP_cluster_matrix)
jaccard_DEWP_STP_daily_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2009_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_STP_cluster_matrix)
jaccard_DEWP_WDSP_daily_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2009_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_WDSP_cluster_matrix)
jaccard_STP_WDSP_daily_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2009_STP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_WDSP_cluster_matrix)

#------------------JACCARD QUESTION 6-------------------------
jaccard_TEMP_TEMP_daily_2007_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_TEMP_cluster_matrix)
jaccard_STP_STP_daily_2007_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_STP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_STP_cluster_matrix)
jaccard_DEWP_DEWP_daily_2007_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix)
jaccard_WDSP_WDSP_daily_2007_2008<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_WDSP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2008_WDSP_cluster_matrix)


jaccard_TEMP_TEMP_month_2007_2008<- jaccard_indep(refined_kmeansvalue_ks.res_month_2007_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_month_2008_TEMP_cluster_matrix)
jaccard_STP_STP_month_2007_2008<- jaccard_indep(refined_kmeansvalue_ks.res_month_2007_STP_cluster_matrix,refined_kmeansvalue_ks.res_month_2008_STP_cluster_matrix)
jaccard_DEWP_DEWP_month_2007_2008<- jaccard_indep(refined_kmeansvalue_ks.res_month_2007_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_month_2008_DEWP_cluster_matrix)
jaccard_WDSP_WDSP_month_2007_2008<- jaccard_indep(refined_kmeansvalue_ks.res_month_2007_WDSP_cluster_matrix,refined_kmeansvalue_ks.res_month_2008_WDSP_cluster_matrix)
#---------------FOR THREE Years y1 and y3-----------------------
jaccard_TEMP_TEMP_daily_2007_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_TEMP_cluster_matrix)
jaccard_STP_STP_daily_2007_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_STP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_STP_cluster_matrix)
jaccard_DEWP_DEWP_daily_2007_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_DEWP_cluster_matrix)
jaccard_WDSP_WDSP_daily_2007_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2007_WDSP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_WDSP_cluster_matrix)


jaccard_TEMP_TEMP_month_2007_2009<- jaccard_indep(refined_kmeansvalue_ks.res_month_2007_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_month_2009_TEMP_cluster_matrix)
jaccard_STP_STP_month_2007_2009<- jaccard_indep(refined_kmeansvalue_ks.res_month_2007_STP_cluster_matrix,refined_kmeansvalue_ks.res_month_2009_STP_cluster_matrix)
jaccard_DEWP_DEWP_month_2007_2009<- jaccard_indep(refined_kmeansvalue_ks.res_month_2007_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_month_2009_DEWP_cluster_matrix)
jaccard_WDSP_WDSP_month_2007_2009<- jaccard_indep(refined_kmeansvalue_ks.res_month_2007_WDSP_cluster_matrix,refined_kmeansvalue_ks.res_month_2009_WDSP_cluster_matrix)
#------------FOR THREE Years y2 and y3---------------
jaccard_TEMP_TEMP_daily_2008_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_TEMP_cluster_matrix)
jaccard_STP_STP_daily_2008_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_STP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_STP_cluster_matrix)
jaccard_DEWP_DEWP_daily_2008_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_DEWP_cluster_matrix)
jaccard_WDSP_WDSP_daily_2008_2009<- jaccard_indep(refined_kmeansvalue_ks.res_daily_2008_WDSP_cluster_matrix,refined_kmeansvalue_ks.res_daily_2009_WDSP_cluster_matrix)


jaccard_TEMP_TEMP_month_2008_2009<- jaccard_indep(refined_kmeansvalue_ks.res_month_2008_TEMP_cluster_matrix,refined_kmeansvalue_ks.res_month_2009_TEMP_cluster_matrix)
jaccard_STP_STP_month_2008_2009<- jaccard_indep(refined_kmeansvalue_ks.res_month_2008_STP_cluster_matrix,refined_kmeansvalue_ks.res_month_2009_STP_cluster_matrix)
jaccard_DEWP_DEWP_month_2008_2009<- jaccard_indep(refined_kmeansvalue_ks.res_month_2008_DEWP_cluster_matrix,refined_kmeansvalue_ks.res_month_2009_DEWP_cluster_matrix)
jaccard_WDSP_WDSP_month_2008_2009<- jaccard_indep(refined_kmeansvalue_ks.res_month_2008_WDSP_cluster_matrix,refined_kmeansvalue_ks.res_month_2009_WDSP_cluster_matrix)
#--------------------- calculations --------------
TEMP_daily_7_8_9 <- c(jaccard_TEMP_TEMP_daily_2007_2008,jaccard_TEMP_TEMP_daily_2008_2009,jaccard_TEMP_TEMP_daily_2007_2009)
DEWP_daily_7_8_9 <- c(jaccard_DEWP_DEWP_daily_2007_2008,jaccard_DEWP_DEWP_daily_2008_2009,jaccard_DEWP_DEWP_daily_2007_2009)
STP_daily_7_8_9 <- c(jaccard_STP_STP_daily_2007_2008,jaccard_STP_STP_daily_2008_2009,jaccard_STP_STP_daily_2007_2009)
WDSP_daily_7_8_9 <- c(jaccard_WDSP_WDSP_daily_2007_2008,jaccard_WDSP_WDSP_daily_2008_2009,jaccard_WDSP_WDSP_daily_2007_2009)


TEMP_month_7_8_9 <- c(jaccard_TEMP_TEMP_month_2007_2008,jaccard_TEMP_TEMP_month_2008_2009,jaccard_TEMP_TEMP_month_2007_2009)
DEWP_month_7_8_9 <- c(jaccard_DEWP_DEWP_month_2007_2008,jaccard_DEWP_DEWP_month_2008_2009,jaccard_DEWP_DEWP_month_2007_2009)
STP_month_7_8_9 <- c(jaccard_STP_STP_month_2007_2008,jaccard_STP_STP_month_2008_2009,jaccard_STP_STP_month_2007_2009)
WDSP_month_7_8_9 <- c(jaccard_WDSP_WDSP_month_2007_2008,jaccard_WDSP_WDSP_month_2008_2009,jaccard_WDSP_WDSP_month_2007_2009)

mean_TEMP_daily_7_8_9<- mean(TEMP_daily_7_8_9)
mean_DEWP_daily_7_8_9<- mean(DEWP_daily_7_8_9)
mean_STP_daily_7_8_9<- mean(STP_daily_7_8_9)
mean_WDSP_daily_7_8_9<- mean(WDSP_daily_7_8_9)

mean_TEMP_month_7_8_9<- mean(TEMP_month_7_8_9)
mean_DEWP_month_7_8_9<- mean(DEWP_month_7_8_9)
mean_STP_month_7_8_9<- mean(STP_month_7_8_9)
mean_WDSP_month_7_8_9<- mean(WDSP_month_7_8_9)

print(mean_TEMP_daily_7_8_9)
print(mean_DEWP_daily_7_8_9)
print(mean_STP_daily_7_8_9)
print(mean_WDSP_daily_7_8_9)

print(mean_TEMP_month_7_8_9)
print(mean_DEWP_month_7_8_9)
print(mean_STP_month_7_8_9)
print(mean_WDSP_month_7_8_9)

#------------------------Plotting clusters for all the years-------------------------------
fviz_cluster(kmeansvalue_ks.res_month_2007,data_used_for_kmeam_hourly_month_2007[,-1], ellipse.type = "norm")
fviz_cluster(kmeansvalue_ks.res_month_2008,data_used_for_kmeam_hourly_month_2008[,-1], ellipse.type = "norm")
fviz_cluster(kmeansvalue_ks.res_month_2009,data_used_for_kmeam_hourly_month_2009[,-1], ellipse.type = "norm")

#------------------------Plotting clusters for Euclidean for all the years-------------------------------
kmeansvalue_ks.reseuclidiean <- kmeans(matrix_of_euclidean_distance_of_month_2007 , 3, nstart = 25)
kmeansvalue_ks.res_month_2007reseuclidiean_cluster <- kmeansvalue_ks.reseuclidiean$cluster
kmeansvalue_ks.res_month_2007reseuclidiean_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2007reseuclidiean_cluster)
fviz_cluster(kmeansvalue_ks.reseuclidiean,data_used_for_kmeam_hourly_month_2007[,-1], ellipse.type = "norm")

kmeansvalue_ks.reseuclidiean <- kmeans(matrix_of_euclidean_distance_of_month_2008 , 3, nstart = 25)
kmeansvalue_ks.res_month_2008reseuclidiean_cluster <- kmeansvalue_ks.reseuclidiean$cluster
kmeansvalue_ks.res_month_2008reseuclidiean_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2008reseuclidiean_cluster)
fviz_cluster(kmeansvalue_ks.reseuclidiean,data_used_for_kmeam_hourly_month_2008[,-1], ellipse.type = "norm")

kmeansvalue_ks.reseuclidiean <- kmeans(matrix_of_euclidean_distance_of_month_2009 , 3, nstart = 25)
kmeansvalue_ks.res_month_2009reseuclidiean_cluster <- kmeansvalue_ks.reseuclidiean$cluster
kmeansvalue_ks.res_month_2009reseuclidiean_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2009reseuclidiean_cluster)
fviz_cluster(kmeansvalue_ks.reseuclidiean,data_used_for_kmeam_hourly_month_2009[,-1], ellipse.type = "norm")

#------------------------Plotting clusters for Pearson for all the years-------------------------------
kmeansvalue_ks.respearson <- kmeans(matrix_of_pearson_distance_of_month_2007 , 3, nstart = 25)
kmeansvalue_ks.res_month_2007respearson_cluster <- kmeansvalue_ks.respearson$cluster
kmeansvalue_ks.res_month_2007respearson_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2007respearson_cluster)
fviz_cluster(kmeansvalue_ks.respearson,data_used_for_kmeam_hourly_month_2007[,-1], ellipse.type = "norm")

kmeansvalue_ks.respearson <- kmeans(matrix_of_pearson_distance_of_month_2008 , 3, nstart = 25)
kmeansvalue_ks.res_month_2008respearson_cluster <- kmeansvalue_ks.respearson$cluster
kmeansvalue_ks.res_month_2008respearson_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2008respearson_cluster)
fviz_cluster(kmeansvalue_ks.respearson,data_used_for_kmeam_hourly_month_2008[,-1], ellipse.type = "norm")

kmeansvalue_ks.respearson <- kmeans(matrix_of_pearson_distance_of_month_2009 , 3, nstart = 25)
kmeansvalue_ks.res_month_2009respearson_cluster <- kmeansvalue_ks.respearson$cluster
kmeansvalue_ks.res_month_2009respearson_cluster_matrix <- as.matrix(kmeansvalue_ks.res_month_2009respearson_cluster)
fviz_cluster(kmeansvalue_ks.respearson,data_used_for_kmeam_hourly_month_2009[,-1], ellipse.type = "norm")

#----------------Visualization of stations belonging to the same cluster on the TX map for the year 2008--------------
install.packages("ggmap")
library(ggmap)
library(ggplot2)
library(tibble)
stations <- read.csv(file='stations.csv',sep=',',header=TRUE)
register_google("AIzaSyC8hddQcNZATeITv9VvPZoLTIrYSm5alsQ")
location_df <- paste(stations$City,",",stations$State)
location_df <- tibble(location_df)
location_geo_df <- geocode(location_df$location_df)
df.map_location <- cbind(location_df,location_geo_df)
stations_number <- paste(stations$StationNumber)
clusters<-as.matrix( kmeansvalue_ks.res_daily_2008_DEWP_cluster_matrix[-(478:3876),])
cl1 <- paste(clusters)
cl1 <- tibble(cl1)
df.map_location <- cbind(location_df,location_geo_df,stations_number,cl1)
KYMAP <- get_map("texas", zoom=6)
ggmap(KYMAP)+geom_point(aes(x= lon[], y= lat[], colour= as.factor(cl1)),data = df.map_location)+ggtitle("Texas")