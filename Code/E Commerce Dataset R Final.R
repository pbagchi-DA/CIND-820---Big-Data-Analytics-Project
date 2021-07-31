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
library(rpart)
library(pROC)
library(MASS)
library(leaps)
library(mlbench)
library(modeltools)
library(ROSE)

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

#F1 Score
LR.F1 <- F1_Score(y_pred = fitted.results, y_true = UNB.test_set$Churn)
LR.F1


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
train_control.3 <- trainControl(method = "cv", 
                              number = 3,
                              savePredictions = T)

# train the model on training set
LR.3CV.model <- train(Churn ~ .,
               data = UNB.train_set,
               method = "glm",
               family = binomial,
               trControl = train_control.3)

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

#F1 Score
LR.3CVF1 <- F1_Score(y_pred = LR.3CV.probs, y_true = UNB.test_set$Churn)
LR.3CVF1

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
train_control.10 <- trainControl(method = "cv", 
                              number = 10,
                              savePredictions = T)

# train the model on training set
LR.10CV.model <- train(Churn ~ .,
                      data = UNB.train_set,
                      method = "glm",
                      family = binomial,
                      trControl = train_control.10)

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

#F1 Score
LR.10CVF1 <- F1_Score(y_pred = LR.10CV.probs, y_true = UNB.test_set$Churn)
LR.10CVF1

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


#Decision Tree

#Fit the model
DT.Model <- rpart(Churn ~. , data = UNB.train_set, method = "class", control = rpart.control((cp = 0.05)))
summary(DT.Model)

#Important variables
pred_DT <- predict(DT.Model, UNB.test_set, type = "class")
DT.table <- table(pred_DT, UNB.test_set$Churn)
DT.table

#Plot Tree
print(DT.Model)
plot(DT.Model, uniform=TRUE,
     main="Important variables")
text(DT.Model, use.n=TRUE, all=TRUE, cex=.8)

#Confusion Matrix
caret::confusionMatrix(pred_DT, UNB.test_set$Churn,
                       positive = "1")

table(pred_DT, UNB.test_set$Churn)

#F1 Score
DT.F1 <- F1_Score(y_pred = pred_DT, y_true = UNB.test_set$Churn)
DT.F1

# need to create prediction object from ROCR
DT.predictions <- as.numeric(predict(DT.Model, UNB.test_set, type = "class"))
DT.pred <- prediction(DT.predictions, UNB.test_set$Churn)

# plotting ROC curve
DT.perf <- performance(DT.pred, measure = "tpr", x.measure = "fpr") 
plot(DT.perf, main = "Random Forest ROC curve", colorize = T)

# AUC value
DT.auc <- performance(DT.pred, measure = "auc")
DT.auc <- DT.auc@y.values[[1]]
DT.auc


#Random Forest

#Fit the model
rf_classifier <- randomForest(Churn ~ ., data = UNB.train_set)
print(rf_classifier)

#Important variables
varImpPlot(rf_classifier, main = "Important Variables")

pred_rf <- predict(rf_classifier, UNB.test_set)
caret::confusionMatrix(pred_rf, UNB.test_set$Churn,
                       positive = "1")

table(pred_rf, UNB.test_set$Churn)

#F1 Score
RF.F1 <- F1_Score(y_pred = pred_rf, y_true = UNB.test_set$Churn)
RF.F1

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


#AUC for all models

plot(prf, col = "blue", main = "ROC")
plot(LR.3CV.prf, col = "black", lty = 2, lwd = 2, add = TRUE)
plot(LR.10CV.prf, col = "red", lwd = 1, add = TRUE)
plot(DT.perf, col = "chocolate4", add = TRUE)
plot(perf, col = "dark green", add = TRUE)
legend("bottom", c("Logistic Regression", "Logistic Regression 3 CV", "Logistic Regression 10 CV", 
                   "Decision Tree", "Random Forest"),
       lty = c(1,1), lwd = c(2, 2), col = c("blue", "black", "red", "chocolate4", "dark green"), cex = 0.75)
rect(0, 1.05, 2, 2.5, xpd = TRUE, col = "white", border = "white")
title("ROC's of Unbalanced Training Set Before Feature Selection")

# After comparing all the AUC values it seems like the Logistic Regression & Random Forest models performs 
# better than the Decision Tree, Logistic Regression 3 CV, Logistic Regression 10 CV to predict churn in this 
# particular dataset.


#Feature Selection

# Rank Features By Importance

# ensure results are repeatable
set.seed(2930)


# prepare training scheme
control <- trainControl(method ="repeatedcv", number = 10, repeats = 3)

# train the model
model <- train(Churn~., data = Cleaned_Data, 
               method = "lvq", preProcess = "scale", trControl = control)

# estimate variable importance
importance <- varImp(model, scale = FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)


# ensure the results are repeatable
set.seed(2930)

# define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# run the RFE algorithm
results <- rfe(Cleaned_Data[,2:19], Cleaned_Data[[1]], sizes = c(1:18), rfeControl = control)

# summarize the results
print(results)

# list the chosen features
predictors(results)

# plot the results
plot(results, type = c("g", "o"))


##Unbalanced Dataset After Feature Selection

#Dataset with selected variables
FS_Data <- subset(Cleaned_Data, select = c(Churn, Tenure, OrderCount, Complain, NumberOfAddress, 
                  CashbackAmount, DaySinceLastOrder, NumberOfDeviceRegistered, WarehouseToHome, CityTier))

#Splitting Test and Train set

set.seed(2930) #randomly chosen to be able to to produce reproducible results for this assignment but, best not to use in real world scenarios

FS.train.index <- createDataPartition(FS_Data$Churn, p = .7, list = FALSE)

UNFS.train_set <- FS_Data[ train.index,]
UNFS.test_set  <- FS_Data[-train.index,]

summary(UNFS.train_set)
sum(is.na(UNFS.train_set))

#Logistic Regression

# Fit the model
LR.model <- glm(Churn ~., data = UNFS.train_set, family = binomial)

# Summarize the model

varImp(LR.model, decreasing = T)

imp <- as.data.frame(varImp(LR.model))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

#UNB.modelcoef <- summary(LR.model)[["coefficients"]]

#UNB.modelcoef[order(UNB.modelcoef[ , 4]), ]  

# Make predictions
probabilities <- LR.model %>% predict(UNFS.test_set, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Model accuracy
mean(predicted.classes == UNFS.test_set$Churn)
table(is.na(UNFS.test_set))
table(is.na(UNFS.train_set))
fitted.results <- predict(LR.model,newdata = UNFS.test_set,type='response')
fitted.results <- as.factor(ifelse(fitted.results > 0.5,1,0))
misClasificError <- mean(fitted.results != UNFS.test_set$Churn)
misClasificError
print(paste('Accuracy',1-misClasificError))

confusionMatrix(data=fitted.results, reference = UNFS.test_set$Churn)

#F1 Score
FS.LR.F1 <- F1_Score(y_pred = fitted.results, y_true = UNFS.test_set$Churn)
FS.LR.F1

# fitting the model
LR.model <- glm(Churn~., data = UNFS.train_set, family = binomial)

# making predictions
churn.probs <- predict(LR.model, UNFS.test_set, type="response")

contrasts(Cleaned_Data$Churn)  # Yes = 1, No = 0

glm.pred = rep("No", length(churn.probs))

table(glm.pred)
glm.pred[churn.probs > 0.5] = "Yes"

glm.pred <- as.factor(glm.pred)
table(glm.pred)
length(glm.pred)
test_churn <- UNFS.test_set$Churn
levels(test_churn)
levels(test_churn) = c('0' = "No",'1' = "Yes")
levels(test_churn)
table(test_churn)

table(test_churn,glm.pred)

confusionMatrix(glm.pred, test_churn, positive = "Yes")


# need to create prediction object from ROCR

pr <- prediction(churn.probs, UNFS.test_set$Churn)

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
train_control.3 <- trainControl(method = "cv", 
                                number = 3,
                                savePredictions = T)

# train the model on training set
LR.3CV.model <- train(Churn ~ .,
                      data = UNFS.train_set,
                      method = "glm",
                      family = binomial,
                      trControl = train_control.3)

# print cv scores
summary(LR.3CV.model)
print(LR.3CV.model)

#Important Variables
varImp(LR.3CV.model, decreasing = T)


# making predictions
LR.3CV.probs <- predict(LR.3CV.model, UNFS.test_set)

caret::confusionMatrix(LR.3CV.probs, UNFS.test_set$Churn,
                       positive = "1")

table(LR.3CV.probs, UNFS.test_set$Churn)

#F1 Score
FS.LR.3CV.F1 <- F1_Score(y_pred = LR.3CV.probs, y_true = UNFS.test_set$Churn)
FS.LR.3CV.F1

# need to create prediction object from ROCR
LR.3CV.predictions <- as.numeric(predict(LR.3CV.model, UNFS.test_set))
LR.3CV.pr <- prediction(as.numeric(LR.3CV.probs), as.numeric(UNFS.test_set$Churn))

# plotting ROC curve
LR.3CV.prf <- performance(LR.3CV.pr, measure = "tpr", x.measure = "fpr")
plot(LR.3CV.prf, main = "Logistic Regression with 3 CV ROC curve", colorize = T)

# AUC value
LR.3CV.auc <- performance(LR.3CV.pr, measure = "auc")
LR.3CV.auc <- LR.3CV.auc@y.values[[1]]
LR.3CV.auc

# 10 Fold
# define training control
train_control.10 <- trainControl(method = "cv", 
                                 number = 10,
                                 savePredictions = T)

# train the model on training set
LR.10CV.model <- train(Churn ~ .,
                       data = UNFS.train_set,
                       method = "glm",
                       family = binomial,
                       trControl = train_control.10)

# print cv scores
summary(LR.10CV.model)
print(LR.10CV.model)

#Important Variables
varImp(LR.10CV.model, decreasing = T)

# making predictions
LR.10CV.probs <- predict(LR.10CV.model, UNFS.test_set)

caret::confusionMatrix(LR.10CV.probs, UNFS.test_set$Churn,
                       positive = "1")

table(LR.10CV.probs, UNFS.test_set$Churn)

#F1 Score
FS.LR.10CV.F1 <- F1_Score(y_pred = LR.10CV.probs, y_true = UNFS.test_set$Churn)
FS.LR.10CV.F1

# need to create prediction object from ROCR
LR.10CV.predictions <- as.numeric(predict(LR.10CV.model, UNFS.test_set))
LR.10CV.pr <- prediction(as.numeric(LR.10CV.probs), as.numeric(UNFS.test_set$Churn))

# plotting ROC curve
LR.10CV.prf <- performance(LR.10CV.pr, measure = "tpr", x.measure = "fpr")
plot(LR.10CV.prf, main = "Logistic Regression with 10 CV ROC curve", colorize = T)

# AUC value
LR.10CV.auc <- performance(LR.10CV.pr, measure = "auc")
LR.10CV.auc <- LR.10CV.auc@y.values[[1]]
LR.10CV.auc

#Decision Tree

#Fit the model
DT.FS.Model <- rpart(Churn ~. , data = UNFS.train_set, method = "class", control = rpart.control((cp = 0.05)))
summary(DT.FS.Model)

#Important variables
pred_DT <- predict(DT.FS.Model, UNFS.test_set, type = "class")
DT.table <- table(pred_DT, UNFS.test_set$Churn)
DT.table

#Plot Tree
print(DT.FS.Model)
plot(DT.FS.Model, uniform=TRUE,
     main="Important variables")
text(DT.FS.Model, use.n=TRUE, all=TRUE, cex=.8)

#Confusion Matrix
caret::confusionMatrix(pred_DT, UNFS.test_set$Churn,
                       positive = "1")

table(pred_DT, UNFS.test_set$Churn)

#F1 Score
FS.DT.F1 <- F1_Score(y_pred = pred_DT, y_true = UNFS.test_set$Churn)
FS.DT.F1

# need to create prediction object from ROCR
DT.predictions <- as.numeric(predict(DT.FS.Model, UNFS.test_set, type = "class"))
DT.pred <- prediction(DT.predictions, UNFS.test_set$Churn)

# plotting ROC curve
DT.FS.perf <- performance(DT.pred, measure = "tpr", x.measure = "fpr") 
plot(DT.FS.perf, main = "Random Forest ROC curve", colorize = T)

# AUC value
DT.auc <- performance(DT.pred, measure = "auc")
DT.auc <- DT.auc@y.values[[1]]
DT.auc


#Random Forest

#Fit the model
RF_Model <- randomForest(Churn ~ ., data = UNFS.train_set)
print(RF_Model)

#Important variables
varImpPlot(RF_Model, main = "Important Variables")

pred_rf <- predict(RF_Model, UNFS.test_set)
caret::confusionMatrix(pred_rf, UNFS.test_set$Churn,
                       positive = "1")

table(pred_rf, UNFS.test_set$Churn)

#F1 Score
FS.RF.F1 <- F1_Score(y_pred = pred_rf, y_true = UNFS.test_set$Churn)
FS.RF.F1

# need to create prediction object from ROCR
predictions <- as.numeric(predict(RF_Model, UNFS.test_set, type="response"))
pred <- prediction(predictions, UNFS.test_set$Churn)

# plotting ROC curve
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, main = "Random Forest ROC curve", colorize = T)

# AUC value
RF.auc <- performance(pred, measure = "auc")
RF.auc <- RF.auc@y.values[[1]]
RF.auc


#AUC for all models

plot(prf, col = "blue", main = "ROC")
plot(LR.3CV.prf, col = "black", lty = 2, lwd = 2, add = TRUE)
plot(LR.10CV.prf, col = "red", lwd = 1, add = TRUE)
plot(DT.FS.perf, col = "chocolate4", add = TRUE)
plot(perf, col = "dark green", add = TRUE)
legend("bottom", c("Logistic Regression", "Logistic Regression 3 CV", "Logistic Regression 10 CV", 
                   "Decision Tree", "Random Forest"),
       lty = c(1,1), lwd = c(2, 2), col = c("blue", "black", "red", "chocolate4", "dark green"), cex = 0.75)
rect(0, 1.05, 2, 2.5, xpd = TRUE, col = "white", border = "white")
title("ROC's of Unbalanced Training Set After Feature Selection")


##Balanced Train Set Before Feature Selection

#checking how imbalanced is the train set
table(UNB.train_set$Churn)
prop.table(table(UNB.train_set$Churn))

#Balancing Train set by oversampling 

B.train_set <- ovun.sample(Churn ~., data = UNB.train_set,
                           method = "over", N = 4185)$data
summary(B.train_set)
table(B.train_set$Churn)
prop.table(table(B.train_set$Churn))


#Logistic Regression

# Fit the model
B.LRmodel <- glm(Churn ~., data = B.train_set, family = binomial)

# Summarize the model

varImp(B.LRmodel, decreasing = T)

imp <- as.data.frame(varImp(B.LRmodel))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

#UNB.modelcoef <- summary(B.LRmodel)[["coefficients"]]

#UNB.modelcoef[order(UNB.modelcoef[ , 4]), ]  

# Make predictions
probabilities <- B.LRmodel %>% predict(UNB.test_set, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")



# Model accuracy
mean(predicted.classes == UNB.test_set$Churn)
table(is.na(UNB.test_set))
table(is.na(B.train_set))
fitted.results <- predict(B.LRmodel,newdata = UNB.test_set,type='response')
fitted.results <- as.factor(ifelse(fitted.results > 0.5,1,0))
misClasificError <- mean(fitted.results != UNB.test_set$Churn)
misClasificError
print(paste('Accuracy',1-misClasificError))

confusionMatrix(data=fitted.results, reference = UNB.test_set$Churn)

#F1 Score
LR.F1 <- F1_Score(y_pred = fitted.results, y_true = UNB.test_set$Churn)
LR.F1


# fitting the model
B.LRmodel <- glm(Churn~., data = B.train_set, family = binomial)

# making predictions
churn.probs <- predict(B.LRmodel, UNB.test_set, type="response")

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

Bpr <- prediction(churn.probs, UNB.test_set$Churn)

# plotting ROC curve
BLR.prf <- performance(Bpr, measure = "tpr", x.measure = "fpr")
plot(BLR.prf, main = "Logistic Regression ROC curve", colorize = T)

# AUC value
B.LR.auc <- performance(Bpr, measure = "auc")
B.LR.auc <- B.LR.auc@y.values[[1]]
B.LR.auc

# With Cross-Validation

# 3 Fold
# define training control
train_control.3 <- trainControl(method = "cv", 
                                number = 3,
                                savePredictions = T)

# train the model on training set
BLR.3CV.model <- train(Churn ~ .,
                      data = B.train_set,
                      method = "glm",
                      family = binomial,
                      trControl = train_control.3)

# print cv scores
summary(BLR.3CV.model)
print(BLR.3CV.model)

#Important Variables
varImp(BLR.3CV.model, decreasing = T)


# making predictions
BLR.3CV.probs <- predict(BLR.3CV.model, UNB.test_set)

caret::confusionMatrix(BLR.3CV.probs, UNB.test_set$Churn,
                       positive = "1")

table(BLR.3CV.probs, UNB.test_set$Churn)

#F1 Score
LR.3CVF1 <- F1_Score(y_pred = BLR.3CV.probs, y_true = UNB.test_set$Churn)
LR.3CVF1

# need to create prediction object from ROCR
LR.3CV.predictions <- as.numeric(predict(LR.3CV.model, UNB.test_set))
LR.3CV.pr <- prediction(as.numeric(BLR.3CV.probs), as.numeric(UNB.test_set$Churn))

# plotting ROC curve
BLR.3CV.prf <- performance(LR.3CV.pr, measure = "tpr", x.measure = "fpr")
plot(BLR.3CV.prf, main = "Logistic Regression with 3 CV ROC curve", colorize = T)

# AUC value
BLR.3CV.auc <- performance(LR.3CV.pr, measure = "auc")
BLR.3CV.auc <- BLR.3CV.auc@y.values[[1]]
BLR.3CV.auc

# 10 Fold
# define training control
train_control.10 <- trainControl(method = "cv", 
                                 number = 10,
                                 savePredictions = T)

# train the model on training set
LR.10CV.model <- train(Churn ~ .,
                       data = B.train_set,
                       method = "glm",
                       family = binomial,
                       trControl = train_control.10)

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

#F1 Score
LR.10CVF1 <- F1_Score(y_pred = LR.10CV.probs, y_true = UNB.test_set$Churn)
LR.10CVF1

# need to create prediction object from ROCR
LR.10CV.predictions <- as.numeric(predict(LR.10CV.model, UNB.test_set))
LR.10CV.pr <- prediction(as.numeric(LR.10CV.probs), as.numeric(UNB.test_set$Churn))

# plotting ROC curve
BLR.10CV.prf <- performance(LR.10CV.pr, measure = "tpr", x.measure = "fpr")
plot(BLR.10CV.prf, main = "Logistic Regression with 10 CV ROC curve", colorize = T)

# AUC value
LR.10CV.auc <- performance(LR.10CV.pr, measure = "auc")
LR.10CV.auc <- LR.10CV.auc@y.values[[1]]
LR.10CV.auc


#Decision Tree

#Fit the model
DT.Model <- rpart(Churn ~. , data = B.train_set, method = "class", control = rpart.control((cp = 0.05)))
summary(DT.Model)

#Important variables
pred_DT <- predict(DT.Model, UNB.test_set, type = "class")
DT.table <- table(pred_DT, UNB.test_set$Churn)
DT.table

#Plot Tree
print(DT.Model)
plot(DT.Model, uniform=TRUE,
     main="Important variables")
text(DT.Model, use.n=TRUE, all=TRUE, cex=.8)

#Confusion Matrix
caret::confusionMatrix(pred_DT, UNB.test_set$Churn,
                       positive = "1")

table(pred_DT, UNB.test_set$Churn)

#F1 Score
DT.F1 <- F1_Score(y_pred = pred_DT, y_true = UNB.test_set$Churn)
DT.F1

# need to create prediction object from ROCR
DT.predictions <- as.numeric(predict(DT.Model, UNB.test_set, type = "class"))
DT.pred <- prediction(DT.predictions, UNB.test_set$Churn)

# plotting ROC curve
BDT.perf <- performance(DT.pred, measure = "tpr", x.measure = "fpr") 
plot(BDT.perf, main = "Random Forest ROC curve", colorize = T)

# AUC value
DT.auc <- performance(DT.pred, measure = "auc")
DT.auc <- DT.auc@y.values[[1]]
DT.auc


#Random Forest

#Fit the model
rf_classifier <- randomForest(Churn ~ ., data = B.train_set)
print(rf_classifier)

#Important variables
varImpPlot(rf_classifier, main = "Important Variables")

pred_rf <- predict(rf_classifier, UNB.test_set)
caret::confusionMatrix(pred_rf, UNB.test_set$Churn,
                       positive = "1")

table(pred_rf, UNB.test_set$Churn)

#F1 Score
RF.F1 <- F1_Score(y_pred = pred_rf, y_true = UNB.test_set$Churn)
RF.F1

# need to create prediction object from ROCR
predictions <- as.numeric(predict(rf_classifier, UNB.test_set, type="response"))
pred <- prediction(predictions, UNB.test_set$Churn)

# plotting ROC curve
B.RF.perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(B.RF.perf, main = "Random Forest ROC curve", colorize = T)

# AUC value
RF.auc <- performance(pred, measure = "auc")
RF.auc <- RF.auc@y.values[[1]]
RF.auc


#AUC for all models

plot(BLR.prf, col = "blue", main = "ROC")
plot(BLR.3CV.prf, col = "black", lty = 2, lwd = 2, add = TRUE)
plot(BLR.10CV.prf, col = "red", lwd = 1, add = TRUE)
plot(BDT.perf, col = "chocolate4", add = TRUE)
plot(B.RF.perf, col = "dark green", add = TRUE)
legend("bottom", c("Logistic Regression", "Logistic Regression 3 CV", "Logistic Regression 10 CV", 
                   "Decision Tree", "Random Forest"),
       lty = c(1,1), lwd = c(2, 2), col = c("blue", "black", "red", "chocolate4", "dark green"), cex = 0.75)
rect(0, 1.05, 2, 2.5, xpd = TRUE, col = "white", border = "white")
title("ROC's of Balanced Training Set Before Feature Selection")

# It still seems like the Logistic Regression & Random Forest models are performing
# better than the Decision Tree, Logistic Regression 3 CV, Logistic Regression 10 CV to predict churn.


##Balanced Dataset After Feature Selection

#checking how imbalanced the train set is
table(UNFS.train_set$Churn)
prop.table(table(UNFS.train_set$Churn))

#Balancing Train set by oversampling 

BFS.train_set <- ovun.sample(Churn ~., data = UNFS.train_set,
                           method = "over", N = 4185)$data
summary(BFS.train_set)
table(BFS.train_set$Churn)
prop.table(table(BFS.train_set$Churn))



#Logistic Regression

# Fit the model
BFS.LR.model <- glm(Churn ~., data = BFS.train_set, family = binomial)

# Summarize the model

varImp(BFS.LR.model, decreasing = T)

imp <- as.data.frame(varImp(BFS.LR.model))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

#UNB.modelcoef <- summary(BFS.LR.model)[["coefficients"]]

#UNB.modelcoef[order(UNB.modelcoef[ , 4]), ]  

# Make predictions
probabilities <- BFS.LR.model %>% predict(UNFS.test_set, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Model accuracy
mean(predicted.classes == UNFS.test_set$Churn)
table(is.na(UNFS.test_set))
table(is.na(BFS.train_set))
fitted.results <- predict(BFS.LR.model,newdata = UNFS.test_set,type='response')
fitted.results <- as.factor(ifelse(fitted.results > 0.5,1,0))
misClasificError <- mean(fitted.results != UNFS.test_set$Churn)
misClasificError
print(paste('Accuracy',1-misClasificError))

confusionMatrix(data=fitted.results, reference = UNFS.test_set$Churn)

#F1 Score
BFS.LR.F1 <- F1_Score(y_pred = fitted.results, y_true = UNFS.test_set$Churn)
BFS.LR.F1

# fitting the model
BFS.LR.model <- glm(Churn~., data = BFS.train_set, family = binomial)

# making predictions
churn.probs <- predict(BFS.LR.model, UNFS.test_set, type="response")

contrasts(Cleaned_Data$Churn)  # Yes = 1, No = 0

glm.pred = rep("No", length(churn.probs))

table(glm.pred)
glm.pred[churn.probs > 0.5] = "Yes"

glm.pred <- as.factor(glm.pred)
table(glm.pred)
length(glm.pred)
test_churn <- UNFS.test_set$Churn
levels(test_churn)
levels(test_churn) = c('0' = "No",'1' = "Yes")
levels(test_churn)
table(test_churn)

table(test_churn,glm.pred)

confusionMatrix(glm.pred, test_churn, positive = "Yes")


# need to create prediction object from ROCR

BFS.LR.pr <- prediction(churn.probs, UNFS.test_set$Churn)

# plotting ROC curve
BFS.LR.prf <- performance(BFS.LR.pr, measure = "tpr", x.measure = "fpr")
plot(BFS.LR.prf, main = "Logistic Regression ROC curve", colorize = T)

# AUC value
BFS.LR.auc <- performance(BFS.LR.pr, measure = "auc")
BFS.LR.auc <- BFS.LR.auc@y.values[[1]]
BFS.LR.auc

# With Cross-Validation

# 3 Fold
# define training control
train_control.3 <- trainControl(method = "cv", 
                                number = 3,
                                savePredictions = T)

# train the model on training set
BFS.LR.3CV.model <- train(Churn ~ .,
                      data = BFS.train_set,
                      method = "glm",
                      family = binomial,
                      trControl = train_control.3)

# print cv scores
summary(BFS.LR.3CV.model)
print(BFS.LR.3CV.model)

#Important Variables
varImp(BFS.LR.3CV.model, decreasing = T)


# making predictions
LR.3CV.probs <- predict(BFS.LR.3CV.model, UNFS.test_set)

caret::confusionMatrix(LR.3CV.probs, UNFS.test_set$Churn,
                       positive = "1")

table(LR.3CV.probs, UNFS.test_set$Churn)

#F1 Score
FS.LR.3CV.F1 <- F1_Score(y_pred = LR.3CV.probs, y_true = UNFS.test_set$Churn)
FS.LR.3CV.F1

# need to create prediction object from ROCR
LR.3CV.predictions <- as.numeric(predict(BFS.LR.3CV.model, UNFS.test_set))
BFS.LR.3CV.pr <- prediction(as.numeric(LR.3CV.probs), as.numeric(UNFS.test_set$Churn))

# plotting ROC curve
BFS.LR.3CV.prf <- performance(BFS.LR.3CV.pr, measure = "tpr", x.measure = "fpr")
plot(BFS.LR.3CV.prf, main = "Logistic Regression with 3 CV ROC curve", colorize = T)

# AUC value
BFS.LR.3CV.auc <- performance(BFS.LR.3CV.pr, measure = "auc")
BFS.LR.3CV.auc <- BFS.LR.3CV.auc@y.values[[1]]
BFS.LR.3CV.auc

# 10 Fold
# define training control
train_control.10 <- trainControl(method = "cv", 
                                 number = 10,
                                 savePredictions = T)

# train the model on training set
BFS.LR.10CV.model <- train(Churn ~ .,
                       data = BFS.train_set,
                       method = "glm",
                       family = binomial,
                       trControl = train_control.10)

# print cv scores
summary(BFS.LR.10CV.model)
print(BFS.LR.10CV.model)

#Important Variables
varImp(BFS.LR.10CV.model, decreasing = T)

# making predictions
BFS.LR.10CV.probs <- predict(BFS.LR.10CV.model, UNFS.test_set)

caret::confusionMatrix(BFS.LR.10CV.probs, UNFS.test_set$Churn,
                       positive = "1")

table(BFS.LR.10CV.probs, UNFS.test_set$Churn)

#F1 Score
BFS.LR.10CV.F1 <- F1_Score(y_pred = BFS.LR.10CV.probs, y_true = UNFS.test_set$Churn)
BFS.LR.10CV.F1

# need to create prediction object from ROCR
LR.10CV.predictions <- as.numeric(predict(BFS.LR.10CV.model, UNFS.test_set))
LR.10CV.pr <- prediction(as.numeric(BFS.LR.10CV.probs), as.numeric(UNFS.test_set$Churn))

# plotting ROC curve
BFS.LR.10CV.prf <- performance(LR.10CV.pr, measure = "tpr", x.measure = "fpr")
plot(BFS.LR.10CV.prf, main = "Logistic Regression with 10 CV ROC curve", colorize = T)

# AUC value
BFS.LR.10CV.auc <- performance(LR.10CV.pr, measure = "auc")
BFS.LR.10CV.auc <- BFS.LR.10CV.auc@y.values[[1]]
BFS.LR.10CV.auc

#Decision Tree

#Fit the model
BDT.FS.Model <- rpart(Churn ~. , data = BFS.train_set, method = "class", control = rpart.control((cp = 0.05)))
summary(DT.FS.Model)

#Important variables
pred_DT <- predict(BDT.FS.Model, UNFS.test_set, type = "class")
DT.table <- table(pred_DT, UNFS.test_set$Churn)
DT.table

#Plot Tree
print(BDT.FS.Model)
plot(BDT.FS.Model, uniform=TRUE,
     main="Important variables")
text(BDT.FS.Model, use.n=TRUE, all=TRUE, cex=.8)

#Confusion Matrix
caret::confusionMatrix(pred_DT, UNFS.test_set$Churn,
                       positive = "1")

table(pred_DT, UNFS.test_set$Churn)

#F1 Score
BFS.DT.F1 <- F1_Score(y_pred = pred_DT, y_true = UNFS.test_set$Churn)
BFS.DT.F1

# need to create prediction object from ROCR
DT.predictions <- as.numeric(predict(BDT.FS.Model, UNFS.test_set, type = "class"))
BFS.DT.pred <- prediction(DT.predictions, UNFS.test_set$Churn)

# plotting ROC curve
DT.BFS.perf <- performance(BFS.DT.pred, measure = "tpr", x.measure = "fpr") 
plot(DT.BFS.perf, main = "Random Forest ROC curve", colorize = T)

# AUC value
BFS.DT.auc <- performance(BFS.DT.pred, measure = "auc")
BFS.DT.auc <- BFS.DT.auc@y.values[[1]]
BFS.DT.auc


#Random Forest

#Fit the model
BFS.RF_Model <- randomForest(Churn ~ ., data = BFS.train_set)
print(BFS.RF_Model)

#Important variables
varImpPlot(BFS.RF_Model, main = "Important Variables")

pred_rf <- predict(BFS.RF_Model, UNFS.test_set)
caret::confusionMatrix(pred_rf, UNFS.test_set$Churn,
                       positive = "1")

table(pred_rf, UNFS.test_set$Churn)

#F1 Score
BFS.RF.F1 <- F1_Score(y_pred = pred_rf, y_true = UNFS.test_set$Churn)
BFS.RF.F1

# need to create prediction object from ROCR
predictions <- as.numeric(predict(BFS.RF_Model, UNFS.test_set, type="response"))
pred <- prediction(predictions, UNFS.test_set$Churn)

# plotting ROC curve
BFS.RF.perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(BFS.RF.perf, main = "Random Forest ROC curve", colorize = T)

# AUC value
BFS.RF.auc <- performance(pred, measure = "auc")
BFS.RF.auc <- BFS.RF.auc@y.values[[1]]
BFS.RF.auc


#AUC for all models

plot(BFS.LR.prf, col = "blue", main = "ROC")
plot(BFS.LR.3CV.prf, col = "black", lty = 2, lwd = 2, add = TRUE)
plot(BFS.LR.10CV.prf, col = "red", lwd = 1, add = TRUE)
plot(DT.BFS.perf, col = "chocolate4", add = TRUE)
plot(BFS.RF.perf, col = "dark green", add = TRUE)
legend("bottom", c( "Logistic Regression", "Logistic Regression 3 CV", "Logistic Regression 10 CV", 
                   "Decision Tree", "Random Forest"),
       lty = c(1,1), lwd = c(2, 2), col = c("blue", "black", "red", "chocolate4", "dark green"), cex = 0.75)
rect(0, 1.05, 2, 2.5, xpd = TRUE, col = "white", border = "white")
title("ROC's of Balanced Training Set After Feature Selection")



