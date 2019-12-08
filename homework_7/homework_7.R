library(tidyverse)
library(caret)
library(neuralnet)
library(gains)
ToyotaCorolla <- read.csv(file = "D:/IE 7275 data mining/homework_7/ToyotaCorolla.csv")

summary(data) 

data <- ToyotaCorolla[,c(4,7,8,9,12,14,17,19,21,25,26,28,30,34,39)]



train.norm.df <- train.df

train.norm.df[,c(1,2,4,6,7,9)] <- predict(norm.values,train.df[,c(1,2,4,6,7,9)])

norm.values <- preProcess(train.df[,c(1,2,4,6,7,9)],method= "range")

norm.values <- preProcess(train.df,method= "range")

train.norm.df <- predict(norm.values,train.df)

summary(norm.values)

summary(train.norm.df)

data[,c("CNG")] <- ifelse(data$Fuel_Type=="CNG",1,0)
data[,c("Diesel")] <- ifelse(data$Fuel_Type=="Diesel",1,0)
data[,c("Petrol")] <- ifelse(data$Fuel_Type=="Petrol",1,0)

data <- data[,-3]

data[,c("Price")] <- ToyotaCorolla$Price

norm.values <- preProcess(data,method= "range")

data.norm <- data

data.norm <- predict(norm.values,data)

summary(data)

summary(data.norm)

train.index <- sample(row.names(data.norm), 0.6*dim(data.norm)[1])
valid.index <- setdiff(row.names(data.norm), train.index)
train.df <- data.norm[train.index, ]
valid.df <- data.norm[valid.index, ]

nn <- neuralnet(Price ~., data = train.df, linear.output = F, hidden = 2)
nn$weights
prediction(nn)
plot(nn, rep="best")

pred <- predict(nn, train.df)

RMSE <- sqrt(sum((train.df[,18] - as.array(pred))^2)/nrow(as.array(pred)))
RMSE

pred <- predict(nn, valid.df)

RMSE <- sqrt(sum((valid.df[,18] - as.array(pred))^2)/nrow(as.array(pred)))
RMSE

nn <- neuralnet(Price ~., data = train.df, linear.output = F, hidden = c(5,5))
nn$weights
prediction(nn)
plot(nn, rep="best")

pred <- predict(nn, train.df)

RMSE <- sqrt(sum((train.df[,18] - as.array(pred))^2)/nrow(as.array(pred)))
RMSE

pred <- predict(nn, valid.df)

RMSE <- sqrt(sum((valid.df[,18] - as.array(pred))^2)/nrow(as.array(pred)))
RMSE


#11.2
EastWestAirlinesNN <- read.csv(file = "D:/IE 7275 data mining/homework_7/EastWestAirlinesNN.csv")

summary(EastWestAirlinesNN)

data2 <- na.omit(EastWestAirlinesNN[,-1])

norm.values <- preProcess(data2,method= "range")

data2.norm <- data2

data2.norm <- predict(norm.values,data2)

summary(data2.norm)

train.index <- sample(row.names(data2.norm), 0.6*dim(data2.norm)[1])
valid.index <- setdiff(row.names(data2.norm), train.index)
train.df <- data2.norm[train.index, ]
valid.df <- data2.norm[valid.index, ]

nn <- neuralnet(Phone_sale ~., data = train.df, linear.output = F, hidden = 5)
nn$weights
prediction(nn)
plot(nn, rep="best")

pred <- predict(nn, train.df)

(t3 <- table(pred, train.df$Phone_sale))

predict <- ifelse(pred>=0.5,1,0)

confusionMatrix(predict, train.df$Phone_sale)

(t3 <- table(predict, train.df$Phone_sale))


gain <- gains(train.df$Phone_sale, pred)
barplot(gain$mean.resp / mean(train.df$Phone_sale), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart")

pred <- predict(nn, valid.df[,-15])

predict <- ifelse(pred>=0.5,1,0)

gain <- gains(valid.df$Phone_sale, pred)
barplot(gain$mean.resp / mean(valid.df$Phone_sale), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart")
