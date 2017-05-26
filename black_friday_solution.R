## loading libraries
library(dummies)
library(plyr)
library(xgboost)

#load directory

path <- "D:/mohi/analytics/av/black friday"

setwd(path)

#load data

train <- read.csv("Train.csv",stringsAsFactors = F)
test <- read.csv("Test.csv",stringsAsFactors = F)
## cleaning data

# removing categories 19 and 20
train <- subset(train, !Product_Category_1 %in% c(19,20))
test <- test

# onehot-encoding city variable
train <- dummy.data.frame(train, names=c("City_Category"), sep="_")
test <- dummy.data.frame(test, names=c("City_Category"), sep="_")
View(train)
# converting age variable to numeric
train$Age[train$Age == "0-17"] <- "15"
train$Age[train$Age == "18-25"] <- "21"
train$Age[train$Age == "26-35"] <- "30"
train$Age[train$Age == "36-45"] <- "40"
train$Age[train$Age == "46-50"] <- "48"
train$Age[train$Age == "51-55"] <- "53"
train$Age[train$Age == "55+"] <- "60"

test$Age[test$Age == "0-17"] <- "15"
test$Age[test$Age == "18-25"] <- "21"
test$Age[test$Age == "26-35"] <- "30"
test$Age[test$Age == "36-45"] <- "40"
test$Age[test$Age == "46-50"] <- "48"
test$Age[test$Age == "51-55"] <- "53"
test$Age[test$Age == "55+"] <- "60"

train$Age <- as.integer(train$Age)
test$Age <- as.integer(test$Age)
View(train)
# converting stay in current city to numeric
train$Stay_In_Current_City_Years[train$Stay_In_Current_City_Years == "4+"] <- "4"
test$Stay_In_Current_City_Years[test$Stay_In_Current_City_Years == "4+"] <- "4"

train$Stay_In_Current_City_Years <- as.integer(train$Stay_In_Current_City_Years)
test$Stay_In_Current_City_Years <- as.integer(test$Stay_In_Current_City_Years)

# converting gender to binary
train$Gender <- ifelse(train$Gender == "F", 1, 0)
test$Gender <- ifelse(test$Gender == "F", 1, 0)

# feature representing the count of each user
user_count <- ddply(train, .(User_ID), nrow)
names(user_count)[2] <- "User_Count"
train <- merge(train, user_count, by="User_ID")
test <- merge(test, user_count, all.x=T, by="User_ID")

# feature representing the count of each product
product_count <- ddply(train, .(Product_ID), nrow)
names(product_count)[2] <- "Product_Count"
train <- merge(train, product_count, by="Product_ID")
test <- merge(test, product_count, all.x=T, by="Product_ID")
test$Product_Count[is.na(test$Product_Count)] <- 0

# feature representing the average Purchase of each product
product_mean <- ddply(train, .(Product_ID), summarize, Product_Mean=mean(Purchase))
train <- merge(train, product_mean, by="Product_ID")
test <- merge(test, product_mean, all.x=T, by="Product_ID")
test$Product_Mean[is.na(test$Product_Mean)] <- mean(train$Purchase)

# subsetting columns for submission
submit <- test[,c("User_ID","Product_ID")]

# target variable
y <- train$Purchase

# removing irrelevant columns
train <- subset(train, select=-c(Purchase,Product_ID))
test <- subset(test, select=c(colnames(train)))
str(train)
## xgboost with cross validation
model_xgb_1 <- xgboost(data=as.matrix(train),label=as.matrix(y),cv=5,objective="reg:linear",nrounds=500,max.depth=10,eta=0.1,colsample_bytree=0.5,seed=235,metric="rmse",importance=1)

pred <- predict(model_xgb_1, as.matrix(test))
head(pred)
submit <- data.frame(submit, "Purchase" = pred)
write.csv(submit, "submit.csv", row.names=F)
