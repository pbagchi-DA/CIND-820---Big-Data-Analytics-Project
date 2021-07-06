library(readr)
library(tidyr)
library(tidyverse)
library(readxl)
library(reshape2)
library(caret)
library(dplyr)
library(ggcorrplot)
library(randomForest)
library(e1071)
library(gplots)
library(ROCR)
library(xgboost)
library(rminer)

#FilePath = "C:\\Users\\rhusein\\Documents\\Capstone CIND820 Projects\\Priyanka Bagchi\\E Commerce Dataset.xlsx"
Data <- read_xlsx("D:/Data Analytics, Big Data, and Predictive Analytics Certificate/CIND 820 DA0 - Big Data Analytics Project - P2021/Data/E Commerce Dataset.xlsx", sheet = "E Comm", col_names = TRUE)
#Data <- read_xlsx(FilePath, sheet = "E Comm", col_names = TRUE)

Data_Backup = Data
str(Data)

Data$CustomerID <- as.integer(Data$CustomerID)

str(Data) #Used to visualize the data types of all attributes

summary(Data) 


#sum(is.na(df$col))

#Cleaned_Data <- na.omit(Data) #Removing Rows with NAs Using na.omit() Function


Numeric_Data1 <- select(Data, Churn, CityTier, HourSpendOnApp, Complain) 
Numeric_Data2 <- select(Data, Tenure, WarehouseToHome)
Numeric_Data3 <- select(Data, CouponUsed, OrderCount, SatisfactionScore)
Numeric_Data4 <- select(Data, NumberOfDeviceRegistered, NumberOfAddress, DaySinceLastOrder)

boxplot(Numeric_Data1, horizontal = TRUE)
boxplot(Numeric_Data2, horizontal = TRUE)
boxplot(Numeric_Data3, horizontal = TRUE)
boxplot(Numeric_Data4, horizontal = TRUE)

melt.O_data <- melt(Data)
str(melt.O_data)
table(melt.O_data$PreferredLoginDevice, useNA = 'ifany')
table(melt.O_data$PreferredPaymentMode, useNA = 'ifany')
table(melt.O_data$Gender, useNA = 'ifany')
table(melt.O_data$PreferedOrderCat, useNA = 'ifany')
table(melt.O_data$MaritalStatus, useNA = 'ifany')
table(melt.O_data$variable, useNA = 'ifany')
summary(melt.O_data$value)
head(melt.O_data)

ggplot(data = melt.O_data, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

Character_Data <- select(Data, PreferredLoginDevice, PreferredPaymentMode, Gender, PreferedOrderCat, MaritalStatus)

Character_Data %>% count(PreferredLoginDevice)
Character_Data %>% count(PreferredPaymentMode)
Character_Data %>% count(Gender)
Character_Data %>% count(PreferedOrderCat)
Character_Data %>% count(MaritalStatus)
str(Character_Data)
Data$PreferredLoginDevice = str_replace_all(Data$PreferredLoginDevice,"Phone", "Mobile Phone")
Data$PreferredLoginDevice = str_replace_all(Data$PreferredLoginDevice,"Mobile Mobile Phone", "Mobile Phone")

Data$PreferedOrderCat = str_replace_all(Data$PreferedOrderCat,"Mobile", "Mobile Phone")
Data$PreferedOrderCat = str_replace_all(Data$PreferedOrderCat,"Mobile Phone Phone", "Mobile Phone")

Data$PreferredPaymentMode = str_replace_all(Data$PreferredPaymentMode,"CC", "Credit Card")
Data$PreferredPaymentMode = str_replace_all(Data$PreferredPaymentMode,"COD", "Cash on Delivery")


colSums(is.na(Data))

Tenure_Table <- (as.data.frame(Data %>% count(Tenure)))
str(Tenure_Table)
summary(Tenure_Table)
table(Tenure_Table$Tenure, useNA = 'ifany')
table(is.na(Tenure_Table))
barplot(height = Tenure_Table$n,names=Tenure_Table$Tenure)
Tenure_Table <- as.numeric(Tenure_Table)
plot(Tenure_Table)
Tenure_Table

Data$Tenure[is.na(Data$Tenure)] <- 0 #Used to replace NA's with 0's
colSums(is.na(Data))


Cleaned_Data <- Data %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))

Cleaned_Data$Churn <- as.character(Cleaned_Data$Churn) 

colSums(is.na(Cleaned_Data))

summary(Cleaned_Data)

Combined_Numeric_data <- select(Cleaned_Data, Churn, CityTier, HourSpendOnApp, Complain, Tenure, WarehouseToHome, CouponUsed, OrderCount, SatisfactionScore, NumberOfDeviceRegistered, NumberOfAddress, DaySinceLastOrder)

melt.CM_data <- melt(Cleaned_Data)

head(melt.CM_data)

ggplot(data = melt.CM_data, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

Cleaned_Data$Churn[Cleaned_Data$Churn > 0 & Cleaned_Data$Churn < 0.5] <- 0
Cleaned_Data$Churn[Cleaned_Data$Churn < 1 & Cleaned_Data$Churn >= 0.5] <- 1
summary(Cleaned_Data)

Churn_Table <- (as.data.frame(Cleaned_Data %>% count(Churn)))
str(Churn_Table)

Churn_Table <- as.numeric(Churn_Table)

plot(Churn_Table)
barplot(height = Churn_Table$n, names= Churn_Table$Churn)
Churn_Table

#write.csv(Cleaned_Data, "D:/Data Analytics, Big Data, and Predictive Analytics Certificate/CIND 820 DA0 - Big Data Analytics Project - P2021/Data/Cleaned E Commerce Dataset.csv")


str(Combined_Numeric_data)
corr <- round(cor(Combined_Numeric_data[,-1]), 2)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

#removing specific outliers

#HourSpendOnApp
Cleaned_Data <- subset(Cleaned_Data, HourSpendOnApp < 5) # Removing the 3 customers who spent 5 hours & didn't churn

#WarehouseToHome
Cleaned_Data <- subset(Cleaned_Data, WarehouseToHome < 120) # Removing the 2 customers who live the furthest from the warehouse & didn't churn 

#Tenure
Cleaned_Data <- subset(Cleaned_Data, Tenure < 50) # Removing the 4 customers who have been with the company for over 50 months & didn't churn

#OrderCount
Order_Count_MT7 <- subset(Cleaned_Data, OrderCount >= 7) # Creating a subset with all Order Countes 7 and more

Cleaned_Data <- subset(Cleaned_Data, OrderCount < 7) # Keeping all the customers whose Order Counts were < 7

Order_Count_MT7 <- subset(Order_Count_MT7, Churn == 1) # Removing all the Order count outliers that didn't churn

Cleaned_Data <- rbind(Cleaned_Data, Order_Count_MT7) # Adding all the Order count outliers that did churn back to Dataset

#CouponUsed
Coupon_Used_OT <- subset(Cleaned_Data, OrderCount >= 4) # Creating a subset with all CouponUsed Outliers

Cleaned_Data <- subset(Cleaned_Data, OrderCount < 4) # Keeping all CouponUsed non-outliers

Coupon_Used_OT <- subset(Coupon_Used_OT, Churn == 1) # Removing all CouponUsed outliers that didn't churn

Cleaned_Data <- rbind(Cleaned_Data, Coupon_Used_OT) # Adding all the CouponUsed outliers that did churn back to Dataset

#NumberOfAddress
No_Address_OT <- subset(Cleaned_Data, NumberOfAddress >= 19) # Creating a subset with all NumberOfAddress Outliers

Cleaned_Data <- subset(Cleaned_Data, NumberOfAddress < 19) # Keeping all NumberOfAddress non-outliers

No_Address_OT <- subset(No_Address_OT, Churn == 1) # Removing all NumberOfAddress outliers that didn't churn

Cleaned_Data <- rbind(Cleaned_Data, No_Address_OT) # Adding all the NumberOfAddress outliers that did churn back to Dataset

#DaySinceLastOrder
Cleaned_Data <- subset(Cleaned_Data, DaySinceLastOrder < 15)

#NumberOfDeviceRegistered
count(Cleaned_Data, NumberOfDeviceRegistered >= 6)

No_Devices_OT <- subset(Cleaned_Data, NumberOfDeviceRegistered >= 6) # Creating a subset with all NumberOfDeviceRegistered Outliers

Cleaned_Data <- subset(Cleaned_Data, NumberOfDeviceRegistered < 6) # Keeping all NumberOfDeviceRegistered non-outliers

No_Devices_OT <- subset(No_Devices_OT, Churn == 1) # Removing all NumberOfDeviceRegistered outliers that didn't churn

Cleaned_Data <- rbind(Cleaned_Data, No_Devices_OT) # Adding all the NumberOfDeviceRegistered outliers that did churn back to Dataset
summary(Cleaned_Data)
#Removing CustomerID
Cleaned_Data <- subset(Cleaned_Data, select = -CustomerID)
Cleaned_BKUP <- Cleaned_Data  ## <-  Create backup data-frame
sum(is.na(Cleaned_Data))
Cleaned_Data$Churn <- as.factor(Cleaned_Data$Churn)
Cleaned_Data$PreferredLoginDevice <- as.factor(Cleaned_Data$PreferredLoginDevice)
Cleaned_Data$PreferredPaymentMode <- as.factor(Cleaned_Data$PreferredPaymentMode)
Cleaned_Data$Gender <- as.factor(Cleaned_Data$Gender)
Cleaned_Data$PreferedOrderCat <- as.factor(Cleaned_Data$PreferedOrderCat)
Cleaned_Data$MaritalStatus <- as.factor(Cleaned_Data$MaritalStatus)

str(Cleaned_Data)
summary(Cleaned_Data)

#Splitting Test and Train set

set.seed(2930) #randomly chosen to be able to to produce reproducible results for this assignment but, best not to use in real world scenarios

train.index <- createDataPartition(Cleaned_Data$Churn, p = .7, list = FALSE)

UNB.train_set <- Cleaned_Data[ train.index,]
UNB.test_set  <- Cleaned_Data[-train.index,]

summary(UNB.train_set)
sum(is.na(UNB.train_set))

##Unbalanced Dataset Before Feature Selection

#Logistic Regression

# Fit the model
UNB.model <- glm(Churn ~., data = UNB.train_set, family = binomial)

# Summarize the model

varImp(UNB.model, decreasing = T)

imp <- as.data.frame(varImp(UNB.model))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

#UNB.modelcoef <- summary(UNB.model)[["coefficients"]]

#UNB.modelcoef[order(UNB.modelcoef[ , 4]), ]  

# Make predictions
probabilities <- UNB.model %>% predict(UNB.test_set, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Model accuracy
mean(predicted.classes == UNB.test_set$Churn)
table(is.na(UNB.test_set))
table(is.na(UNB.train_set))
fitted.results <- predict(UNB.model,newdata = UNB.test_set,type='response')
fitted.results <- as.factor(ifelse(fitted.results > 0.5,1,0))
misClasificError <- mean(fitted.results != UNB.test_set$Churn)
misClasificError
print(paste('Accuracy',1-misClasificError))

confusionMatrix(data=fitted.results, reference = UNB.test_set$Churn)

# fitting the model
UNB.model <- glm(Churn~., data = UNB.train_set, family = binomial)

# making predictions
churn.probs <- predict(UNB.model, UNB.test_set, type="response")

contrasts(Cleaned_Data$Churn)  # Yes = 1, No = 0

glm.pred = rep("No", length(churn.probs))

table(glm.pred)
glm.pred[churn.probs > 0.5] = "Yes"

glm.pred <- as.factor(glm.pred)
table(glm.pred)
length(glm.pred)
test_churn <- UNB.test_set$Churn
levels(test_churn)
levels(test_churn) = c('0' = "No",'1' = "Yes")
levels(test_churn)
table(test_churn)

table(test_churn,glm.pred)

confusionMatrix(glm.pred, test_churn, positive = "Yes")


# need to create prediction object from ROCR

pr <- prediction(churn.probs, UNB.test_set$Churn)

# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main = "Logistic Regression ROC curve", colorize = T)

# AUC value
LR.auc <- performance(pr, measure = "auc")
LR.auc <- LR.auc@y.values[[1]]
LR.auc

# With Cross-Validation

# 3 Fold
# define training control
train_control <- trainControl(method = "cv", 
                              number = 3,
                              savePredictions = T)

# train the model on training set
LR.3CV.model <- train(Churn ~ .,
               data = UNB.train_set,
               method = "glm",
               family = binomial,
               trControl = train_control)

# print cv scores
summary(LR.3CV.model)
print(LR.3CV.model)

#Important Variables
varImp(LR.3CV.model, decreasing = T)


# making predictions
LR.3CV.probs <- predict(LR.3CV.model, UNB.test_set)

caret::confusionMatrix(LR.3CV.probs, UNB.test_set$Churn,
                       positive = "1")

table(LR.3CV.probs, UNB.test_set$Churn)


# need to create prediction object from ROCR
LR.3CV.predictions <- as.numeric(predict(LR.3CV.model, UNB.test_set))
LR.3CV.pr <- prediction(as.numeric(LR.3CV.probs), as.numeric(UNB.test_set$Churn))

# plotting ROC curve
LR.3CV.prf <- performance(LR.3CV.pr, measure = "tpr", x.measure = "fpr")
plot(LR.3CV.prf, main = "Logistic Regression with 3 CV ROC curve", colorize = T)

# AUC value
LR.3CV.auc <- performance(LR.3CV.pr, measure = "auc")
LR.3CV.auc <- LR.3CV.auc@y.values[[1]]
LR.3CV.auc

# 10 Fold
# define training control
train_control <- trainControl(method = "cv", 
                              number = 10,
                              savePredictions = T)

# train the model on training set
LR.10CV.model <- train(Churn ~ .,
                      data = UNB.train_set,
                      method = "glm",
                      family = binomial,
                      trControl = train_control)

# print cv scores
summary(LR.10CV.model)
print(LR.10CV.model)

#Important Variables
varImp(LR.10CV.model, decreasing = T)

# making predictions
LR.10CV.probs <- predict(LR.10CV.model, UNB.test_set)

caret::confusionMatrix(LR.10CV.probs, UNB.test_set$Churn,
                       positive = "1")

table(LR.10CV.probs, UNB.test_set$Churn)


# need to create prediction object from ROCR
LR.10CV.predictions <- as.numeric(predict(LR.10CV.model, UNB.test_set))
LR.10CV.pr <- prediction(as.numeric(LR.10CV.probs), as.numeric(UNB.test_set$Churn))

# plotting ROC curve
LR.10CV.prf <- performance(LR.10CV.pr, measure = "tpr", x.measure = "fpr")
plot(LR.10CV.prf, main = "Logistic Regression with 10 CV ROC curve", colorize = T)

# AUC value
LR.10CV.auc <- performance(LR.10CV.pr, measure = "auc")
LR.10CV.auc <- LR.10CV.auc@y.values[[1]]
LR.10CV.auc

#Random Forest

rf_classifier <- randomForest(Churn ~ ., data = UNB.train_set)
print(rf_classifier)

pred_rf <- predict(rf_classifier, UNB.test_set)
caret::confusionMatrix(pred_rf, UNB.test_set$Churn,
                       positive = "1")

table(pred_rf, UNB.test_set$Churn)

# need to create prediction object from ROCR
predictions <- as.numeric(predict(rf_classifier, UNB.test_set, type="response"))
pred <- prediction(predictions, UNB.test_set$Churn)

# plotting ROC curve
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, main = "Random Forest ROC curve", colorize = T)

# AUC value
RF.auc <- performance(pred, measure = "auc")
RF.auc <- RF.auc@y.values[[1]]
RF.auc

varImpPlot(rf_classifier, main = "Important Variables")

#SVM

svm.model <- svm(Churn ~ ., data = UNB.train_set,
                 type = "C-classification",
                 kernel = "linear",
                 scale = FALSE)
svm.model

svm.Test <- predict(svm.model, UNB.test_set,
                     type = "class")
table(svm.Test, UNB.test_set$Churn)

caret::confusionMatrix(svm.Test, UNB.test_set$Churn,
                       positive = "1")

# need to create prediction object from ROCR
svm.predictions <- as.numeric(predict(svm.model, UNB.test_set, type="response"))
svm.pred <- prediction(svm.predictions, UNB.test_set$Churn)

# plotting ROC curve
svm.perf <- performance(svm.pred, measure = "tpr", x.measure = "fpr") 
plot(svm.perf, main = "SVM ROC curve", colorize = T)

# AUC value
svm.auc <- performance(svm.pred, measure = "auc")
svm.auc <- svm.auc@y.values[[1]]
svm.auc

roc_imp2 <- varImp(svmFit, scale = FALSE)
roc_imp2

varImp(svm.predictions, scale = FALSE)

plot(svm.model, data = UNB.train_set)

#XGBoost

#using one hot encoding 
train_labels <- UNB.train_set$Churn 
tests_label <- UNB.test_set$Churn
new_tr <- model.matrix(~.+0, data = UNB.train_set[,-c("Churn"), with = F]) #Getting: Error in -c("Churn") : invalid argument to unary operator
new_ts <- model.matrix(~.+0, data = UNB.test_set[,-c("Churn"), with = F]) #Getting: Error in -c("Churn") : invalid argument to unary operator

#convert factor to numeric 
train_labels <- as.numeric(train_labels)-1
tests_label <- as.numeric(tests_label)-1

#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = train_labels) 
dtest <- xgb.DMatrix(data = new_ts,label = tests_label)

#default parameters
D.parameters <- list(booster = "gbtree", 
                 objective = "binary:logistic", 
                 eta = 0.3, gamma = 0, max_depth = 6, 
                 min_child_weight = 1, 
                 subsample = 1, colsample_bytree = 1)

xgbcv <- xgb.cv( params = D.parameters, data = dtrain, nrounds = 100, nfold = 5, 
                 showsd = T, stratified = T, print_every_n = 10, 
                 early_stop_round = 20, maximize = F)
#best iteration = 79

#first default - model training
xgb1 <- xgb.train (params = D.parameters, data = dtrain, 
                     nrounds = 79, watchlist = list(val = dtest, train = dtrain), 
                     print.every.n = 10, early.stop.round = 10, 
                     maximize = F , eval_metric = "error")

#model prediction
xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

#confusion matrix
confusionMatrix (xgbpred, ts_label)

#Accuracy - 86.54%` 

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 






# calculating the number of negative & positive cases in our data
#negative_cases <- sum(UNB.train_set == FALSE)
#positive_cases <- sum(UNB.train_set == TRUE)

#dtrain <- xgb.DMatrix(as.matrix(sapply(UNB.train_set, as.numeric)))
#dtest <- xgb.DMatrix(as.matrix(sapply(UNB.test_set, as.numeric)))


# train a model using our training data
#model_tuned <- xgboost(data = dtrain, # the data           
                       #max.depth = 3, # the maximum depth of each decision tree
                       #nround = 10, # number of boosting rounds
                       #early_stopping_rounds = 3, # if we dont see an improvement in this many rounds, stop
                       #objective = "binary:logistic", # the objective function
                       #scale_pos_weight = negative_cases/postive_cases) # control for imbalanced classes

# generate predictions for our held-out testing data
#pred <- predict(model_tuned, dtest)


##Unbalanced Dataset After Feature Selection


##Balanced Dataset Before Feature Selection


##Balanced Dataset After Feature Selection



