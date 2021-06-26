library(readr)
library(tidyr)
library(tidyverse)
library(readxl)
library(reshape2)
library(caret)

Data <- read_xlsx("D:/Data Analytics, Big Data, and Predictive Analytics Certificate/CIND 820 DA0 - Big Data Analytics Project - P2021/Data/E Commerce Dataset.xlsx", 
                  sheet = "E Comm",
                  col_names = TRUE)

str(Data)

Data$CustomerID <- as.integer(Data$CustomerID)

str(Data) #Used to visualize the data types of all attributes

summary(Data) 


#sum(is.na(df$col))

#Cleaned_Data <- na.omit(Data) #Removing Rows with NAs Using na.omit() Function

library(dplyr)

Numeric_Data1 <- select(Data, Churn, CityTier, HourSpendOnApp, Complain) 
Numeric_Data2 <- select(Data, Tenure, WarehouseToHome)
Numeric_Data3 <- select(Data, CouponUsed, OrderCount, SatisfactionScore)
Numeric_Data4 <- select(Data, NumberOfDeviceRegistered, NumberOfAddress, DaySinceLastOrder)

boxplot(Numeric_Data1, horizontal = TRUE)
boxplot(Numeric_Data2, horizontal = TRUE)
boxplot(Numeric_Data3, horizontal = TRUE)
boxplot(Numeric_Data4, horizontal = TRUE)

melt.O_data <- melt(Data)

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

Data$PreferredLoginDevice = str_replace_all(Data$PreferredLoginDevice,"Phone", "Mobile Phone")
Data$PreferredLoginDevice = str_replace_all(Data$PreferredLoginDevice,"Mobile Mobile Phone", "Mobile Phone")

Data$PreferedOrderCat = str_replace_all(Data$PreferedOrderCat,"Mobile", "Mobile Phone")
Data$PreferedOrderCat = str_replace_all(Data$PreferedOrderCat,"Mobile Phone Phone", "Mobile Phone")

Data$PreferredPaymentMode = str_replace_all(Data$PreferredPaymentMode,"CC", "Credit Card")
Data$PreferredPaymentMode = str_replace_all(Data$PreferredPaymentMode,"COD", "Cash on Delivery")


colSums(is.na(Data))

Tenure_Table <- (as.data.frame(Data %>% count(Tenure)))
str(Tenure_Table)
Tenure_Table <- as.numeric(Tenure_Table)
plot(Tenure_Table)
Tenure_Table

Data$Tenure[is.na(Data$Tenure)] <- 0 #Used to replace NA's with 0's
colSums(is.na(Data))


library(dplyr)
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
Tenure_Table <- as.numeric(Churn_Table)
plot(Churn_Table)
Churn_Table

#write.csv(Cleaned_Data, "D:/Data Analytics, Big Data, and Predictive Analytics Certificate/CIND 820 DA0 - Big Data Analytics Project - P2021/Data/Cleaned E Commerce Dataset.csv")

library(ggcorrplot)

data(Combined_Numeric_data)
corr <- round(cor(Combined_Numeric_data), 2)

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

#Removing CustomerID
Cleaned_Data <- subset(Cleaned_Data, select = -CustomerID)

Cleaned_Data$Churn <- as.factor(Cleaned_Data$Churn)
str(Cleaned_Data)

#Splitting Test and Train set

train.index <- createDataPartition(Cleaned_Data$Churn, p = .7, list = FALSE)
UNB.train_set <- Data[ train.index,]
UNB.test_set  <- Data[-train.index,]

UNB.train_set <- subset(UNB.train_set, select = -CustomerID)
UNB.test_set <- subset(UNB.test_set, select = -CustomerID)

#Logistic Regression

# Fit the model
UNB.model <- glm(Churn ~., data = UNB.train_set, family = binomial)
# Summarize the model
summary(UNB.model)
# Make predictions
probabilities <- UNB.model %>% predict(UNB.test_set, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes == UNB.test_set$Churn)

fitted.results <- predict(UNB.model,newdata = UNB.test_set,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != UNB.test_set$Churn)
print(paste('Accuracy',1-misClasificError))

confusionMatrix(data=fitted.results, reference = UNB.test_set$Churn)


# fitting the model
UNB.model <- glm(Churn~., data = UNB.train_set, family = binomial)

# making predictions
churn.probs <- predict(UNB.model, UNB.test_set, type="response")
head(churn.probs)

contrasts(Cleaned_Data$Churn)  # Yes = 1, No = 0

glm.pred = rep("No", length(churn.probs))
glm.pred[churn.probs > 0.5] = "Yes"

confusionMatrix(as.factor(glm.pred,levels = 1:2503), 
                as.factor(UNB.test_set$Churn,levels = 1:2503), 
                positive = "Yes")

#Random Forest

library(randomForest)

rf_classifier = randomForest(Churn ~ ., data = UNB.train_set,importance = TRUE, na.action=na.roughfix)
rf_classifier
