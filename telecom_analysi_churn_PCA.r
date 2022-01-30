library(FactoMineR)
library(dplyr)
library(rpart)
library(randomForest)
library(ROCR)
library(rpart.plot)
library(dummies)
library(caret)
library(ggplot2)
library(pROC)
library(DT)
library(prediction)

setwd("C://Users//jrogers//Desktop//C744 WGU")
data_orginal <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

data <- data_orginal

#Search for missing values
sapply(data, function(x) sum(is.na(x))) 

#Remove missing values [rows]
data <- data[complete.cases(data),]

#Wrangle the data, change "No internet service" to "No" in the following columns:
#"OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies"
cols_recode1 <- c(10:15)
for(i in 1:ncol(data[,cols_recode1])) {
  data[,cols_recode1][,i] <- as.factor(mapvalues
                                        (data[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#Wrangle the data, change "No phone service" to "No" for the column "MultilpleLines"
data$MultipleLines <- as.factor(mapvalues(data$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

#Wrangle the data, in column "Tenure", min and max of column
min(data$tenure); max(data$tenure)
#Group into 5 "Tenure" groups: "0-12 Month", "12-24 Month", "24-48 Months", "48-60 Month", "> 60 Month"
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
data$tenure_group <- sapply(data$tenure,group_tenure)
data$tenure_group <- as.factor(data$tenure_group)

#Wrangle the data, in column "SeniorCitizen" change values from 0/1 to "Yes" or "No"
#data$SeniorCitizen <- as.factor(mapvalues(data$SeniorCitizen,
                                       #    from=c("0","1"),
                                        #   to=c("No", "Yes")))

#Wrangle the data, in column "Churn" change values from "Yes" or "No" to 0/1
#churn$Churn <- as.factor(mapvalues(churn$Churn,
# from=c("No","Yes"),
# to=c("0", "1")))


#Wrangle the data, remove uneccessary columns
data$customerID <- NULL
data$tenure <- NULL




str(data)
#Wrangle the data, change the below columns to factors 
data$Churn <- as.factor(data$Churn)
data$gender <- as.factor(data$gender)
data$Partner <- as.factor(data$Partner)
data$Dependents <- as.factor(data$Dependents)
data$InternetService <- as.factor(data$InternetService)
data$Contract <- as.factor(data$Contract)
data$PaperlessBilling <- as.factor(data$PaperlessBilling)
data$PhoneService <- as.factor(data$PhoneService)
data$PaymentMethod <- as.factor(data$PaymentMethod)
data$MultipleLines <- as.factor(data$MultipleLines)
data$OnlineSecurity <- as.factor(data$OnlineSecurity)
data$OnlineBackup <- as.factor(data$OnlineBackup)
data$DeviceProtection <- as.factor(data$DeviceProtection)
data$TechSupport <- as.factor(data$TechSupport)
data$StreamingTV <- as.factor(data$StreamingTV)
data$StreamingMovies <- as.factor(data$StreamingMovies)
data$Partner <- as.factor(data$Partner)

res.pca <- PCA(data, graph = FALSE)

#Wrangle the data, change the below columns to numeric 
data$Churn <- as.numeric(data$Churn)
data$gender <- as.numeric(data$gender)
data$Partner <- as.numeric(data$Partner)
data$Dependents <- as.numeric(data$Dependents)
data$InternetService <- as.numeric(data$InternetService)
data$Contract <- as.numeric(data$Contract)
data$PaperlessBilling <- as.numeric(data$PaperlessBilling)
data$PhoneService <- as.numeric(data$PhoneService)
data$PaymentMethod <- as.numeric(data$PaymentMethod)
data$MultipleLines <- as.numeric(data$MultipleLines)
data$OnlineSecurity <- as.numeric(data$OnlineSecurity)
data$OnlineBackup <- as.numeric(data$OnlineBackup)
data$DeviceProtection <- as.numeric(data$DeviceProtection)
data$TechSupport <- as.numeric(data$TechSupport)
data$StreamingTV <- as.numeric(data$StreamingTV)
data$StreamingMovies <- as.numeric(data$StreamingMovies)
data$Partner <- as.numeric(data$Partner)
data$tenure_group <- as.numeric(data$tenure_group)

str(data)

res.pca <- PCA(data, graph = FALSE)
res.pca
res.pca$eig
res.pca$scores[1:10,]


dummy.data <- dummy.data.frame(data)
intrain.pca <- createDataPartition(y = dummy.data$Churn, p = 0.7, list = FALSE)
pca.train <- dummy.data[intrain.pca, ]
pca.test <- dummy.data[-intrain.pca, ]
pca.train1 <- pca.train[,-c(47,48)] #Removing the dependent variables
pca.test1 <- pca.test[,-c(47,48)] #Removing the dependent variables
pr_train <- prcomp(pca.train1, center = T, scale. = T)
summary(pr_train)

screeplot(pr_train, type = "line", main="Scree Plot")

screeplot(pr_train)

biplot(pr_train, scale = 0)

plot(pr_train, choix = "ind", autoLab = "yes")




pca_var <- pr_train$sdev ^ 2
pca_var_prop <- pca_var / sum(pca_var)
plot(cumsum(pca_var_prop), type = 'b', ylab = "Cummulative Variance", xlab = "Principal Components",main = "Principal Components VS Variance Explained")

pca.final.train <- data.frame(Churn = pca.train$ChurnYes, pr_train$x)
pca.final.train1 <- pca.final.train[,1:16] #Selecting the first 15 Principal components
pca.final.test <- predict(pr_train, pca.test1)
pca.final.test <- as.data.frame(pca.final.test)
pca.final.test1 <- pca.final.test[,1:15]
rpart_pca <- rpart(ChurnYes ~ ., data = pca.final.train1)
predict_rpart.pca <- predict(rpart_pca, newdata = pca.final.test1)
pred_rpart_pca <- prediction(predict_rpart.pca, pca.test$ChurnNo)
perf_rpart_pca <- performance(pred_rpart_pca, "tpr", "fpr")
plot(perf_rpart_pca, colorize = TRUE)