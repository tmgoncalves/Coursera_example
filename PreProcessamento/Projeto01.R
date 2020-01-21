setwd('/Volumes/Seagate Exp/DSAcademy/MiniProjetos/AnaliseFraudes/')
getwd()

#carregar arquivo
train <- read.csv('train_sample.csv')

#informações do dataset

str(train)
View(train)

#ip: ip address of click.
#app: app id for marketing.
#device: device type id of user mobile phone (e.g., iphone 6 plus, iphone 7, huawei mate 7, etc.)
#os: os version id of user mobile phone
#channel: channel id of mobile ad publisher
#click_time: timestamp of click (UTC)
#attributed_time: if user download the app for after clicking an ad, this is the time of the app download
#is_attributed: the target that is to be predicted, indicating the app was downloaded


dim(train)

sapply(train, function(x) sum(is.na(x)))


#transformação das datas
train$click_time <- as.POSIXct(train$click_time)

train$attributed_time <- ifelse(train$attributed_time == '', 0, train$attributed_time)

train$attributed_time <- as.POSIXct(train$attributed_time)
table(train$attributed_time)

library(lubridate)

#análise da variável attributed_time

train_2 <- subset(train, train$is_attributed == 1)

#criação das variáveis hora

train$day <- day(train$click_time)
train$hr <- hour(train$click_time)
train$minutes <- minute(train$click_time)
train$seconds <- second(train$click_time)


table(train$day)
table(train$hr)


table(train$is_attributed)

head(max(table(train$ip)))

library(dplyr)
library(plyr)

as.data.frame(head(sort(table(train$ip), decreasing = TRUE),10))
as.data.frame(head(sort(table(train$app), decreasing = TRUE),10))
as.data.frame(head(sort(table(train$device), decreasing = TRUE),10))
as.data.frame(head(sort(table(train$os), decreasing = TRUE),10))


#graficamente

barplot(as.vector(head(sort(table(train$ip), decreasing = TRUE),10)),
        beside = T,
        col = palette())

View(subset(train, train$ip == 5348))

hist(train$ip)


View(subset(train, train$day == 6 & train$is_attributed ==1))

###################################################################################


train_2$attributed_time <- as.POSIXct(train_2$attributed_time)
str(train_2)

View(train_2)

train_2$day1 <- day(train_2$attributed_time)
train_2$hr1 <- hour(train_2$attributed_time)
train_2$minutes1 <- minute(train_2$attributed_time)
train_2$seconds1 <- second(train_2$attributed_time)


vec1 <- as.vector(table(train_2$day1))
names(vec1) <- c('6','7','8','9')
vec1

barplot(vec1,
        beside = T,
        xlab = 'Dias da semana',
        ylab = 'Número de Downloads',
        col = palette())

View(subset(train_2, train_2$day1==6))
day_7 <- subset(train_2, train_2$day1==7)
day_8 <- subset(train_2, train_2$day1==8)
day_9 <- subset(train_2, train_2$day1==9)

vec_day7 <- as.vector(table(day_7$hr1))
names(vec_day7) <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,23)
vec_day7

points(vec_day7,
        xlab = 'Dias da semana',
        ylab = 'Número de Downloads',
        col = palette())

train$attributed_time <- ifelse(train$attributed_time == '', 0, train$attributed_time)























































