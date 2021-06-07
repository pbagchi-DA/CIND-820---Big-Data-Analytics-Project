library(readr)
library(tidyr)
library(tidyverse)
library(readxl)

Data <- read_xlsx("D:/Data Analytics, Big Data, and Predictive Analytics Certificate/CIND 820 DA0 - Big Data Analytics Project - P2021/Data/E Commerce Dataset.xlsx", 
                  sheet = "E Comm",
                  col_names = TRUE)

str(Data)

Data$CustomerID <- as.integer(Data$CustomerID)

str(Data)

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

colSums(is.na(Cleaned_Data))

summary(Cleaned_Data)

Combined_Numeric_data <- select(Cleaned_Data, Churn, CityTier, HourSpendOnApp, Complain, Tenure, WarehouseToHome, CouponUsed, OrderCount, SatisfactionScore, NumberOfDeviceRegistered, NumberOfAddress, DaySinceLastOrder)

library(ggcorrplot)

data(Combined_Numeric_data)
corr <- round(cor(Combined_Numeric_data), 2)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

