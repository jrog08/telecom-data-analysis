library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(e1071)
library(hrbrthemes)

setwd("C://Users//jrogers//Desktop//C744 WGU")
churn <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')
str(churn)
churn
#Search for missing values
sapply(churn, function(x) sum(is.na(x))) 

#Remove missing values [rows]
churn <- churn[complete.cases(churn), ]

#Wrangle the data, change "No internet service" to "No" in the following columns:
#"OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies"
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#Wrangle the data, change "No phone service" to "No" for the column "MultilpleLines"
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

#Wrangle the data, in column "Tenure", min and max of column
min(churn$tenure); max(churn$tenure)
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
churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)

#Wrangle the data, in column "SeniorCitizen" change values from 0/1 to "Yes" or "No"
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

#Wrangle the data, in column "Churn" change values from "Yes" or "No" to 0/1
#churn$Churn <- as.factor(mapvalues(churn$Churn,
                                          # from=c("No","Yes"),
                                           # to=c("0", "1")))


#Wrangle the data, remove uneccessary columns
churn$customerID <- NULL
churn$tenure <- NULL

#Wrangle the data, change the below columns to factors 
churn$Churn <- as.factor(churn$Churn)
churn$gender <- as.factor(churn$gender)
churn$Partner <- as.factor(churn$Partner)
churn$Dependents <- as.factor(churn$Dependents)
churn$InternetService <- as.factor(churn$InternetService)
churn$Contract <- as.factor(churn$Contract)
churn$PaperlessBilling <- as.factor(churn$PaperlessBilling)
churn$PhoneService <- as.factor(churn$PhoneService)
churn$PaymentMethod <- as.factor(churn$PaymentMethod)
str(churn)
table(churn$Churn)
#Explore the data and features, look for correlations between numeric variables
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")
#High correlation between "MonthlyCharges" and "TotalCharges" columns
#Remove "TotalCharges" column
churn$TotalCharges <- NULL


#Explore data and features, look for good distribution between catagorical variables
#After viewing the data, no need to remove any columns as of now
p01 <- ggplot(churn, aes(x = gender, fill = Churn)) + geom_bar(position = "stack")
p02 <- ggplot(churn, aes(x = SeniorCitizen, fill = Churn)) + geom_bar(position = "stack")
p03 <- ggplot(churn, aes(x = Partner, fill = Churn)) + geom_bar(position = "stack")
p04 <- ggplot(churn, aes(x = Dependents, fill = Churn)) + geom_bar(position = "stack")
grid.arrange(p01, p02, p03, p04, ncol=2)

p05 <- ggplot(churn, aes(x = PhoneService, fill = Churn)) + geom_bar(position = "stack")
p06 <- ggplot(churn, aes(x = MultipleLines, fill = Churn)) + geom_bar(position = "stack")
p07 <- ggplot(churn, aes(x = InternetService, fill = Churn)) + geom_bar(position = "stack")
p08 <- ggplot(churn, aes(x = OnlineSecurity, fill = Churn)) + geom_bar(position = "stack")
grid.arrange(p05, p06, p07, p08, ncol=2)

p09 <- ggplot(churn, aes(x = OnlineBackup, fill = Churn)) + geom_bar(position = "stack")
p10 <- ggplot(churn, aes(x = DeviceProtection, fill = Churn)) + geom_bar(position = "stack")
p11 <- ggplot(churn, aes(x = TechSupport, fill = Churn)) + geom_bar(position = "stack")
p12 <- ggplot(churn, aes(x = StreamingTV, fill = Churn)) + geom_bar(position = "stack")
grid.arrange(p09, p10, p11, p12, ncol=2)

p13 <- ggplot(churn, aes(x = StreamingMovies, fill = Churn)) + geom_bar(position = "stack")
p14 <- ggplot(churn, aes(x = Contract, fill = Churn)) + geom_bar(position = "stack")
p15 <- ggplot(churn, aes(x = PaperlessBilling, fill = Churn)) + geom_bar(position = "stack")
grid.arrange(p13, p14, p15, ncol=2)

p16 <- ggplot(churn, aes(x = PaymentMethod, fill = Churn)) + geom_bar(position = "stack")
p17 <- ggplot(churn, aes(x = tenure_group, fill = Churn)) + geom_bar(position = "stack")
grid.arrange(p16, p17, ncol=1)


#Univariate graphs
p18 <- ggplot(churn, aes(gender)) +
  geom_bar(aes(fill = gender))
p19 <- ggplot(churn, aes(SeniorCitizen)) +
  geom_bar(aes(fill = SeniorCitizen))
p20 <- ggplot(churn, aes(Partner)) +
  geom_bar(aes(fill = Partner))
p21 <-ggplot(churn, aes(Dependents)) +
  geom_bar(aes(fill = Dependents))
grid.arrange(p18, p19, p20, p21, ncol=2)

p22 <- ggplot(churn, aes(PhoneService)) +
  geom_bar(aes(fill = PhoneService))
p23 <- ggplot(churn, aes(MultipleLines)) +
  geom_bar(aes(fill = MultipleLines))
p24 <- ggplot(churn, aes(InternetService)) +
  geom_bar(aes(fill = InternetService))
p25 <-ggplot(churn, aes(OnlineSecurity)) +
  geom_bar(aes(fill = OnlineSecurity))
grid.arrange(p22, p23, p24, p25, ncol=2)

p26 <- ggplot(churn, aes(OnlineBackup)) +
  geom_bar(aes(fill = OnlineBackup))
p27 <- ggplot(churn, aes(DeviceProtection)) +
  geom_bar(aes(fill = DeviceProtection))
p28 <- ggplot(churn, aes(TechSupport)) +
  geom_bar(aes(fill = TechSupport))
p29 <-ggplot(churn, aes(StreamingTV)) +
  geom_bar(aes(fill = StreamingTV))
grid.arrange(p26, p27, p28, p29, ncol=2)

p30 <- ggplot(churn, aes(StreamingMovies)) +
  geom_bar(aes(fill = StreamingMovies))
p31 <- ggplot(churn, aes(Contract)) +
  geom_bar(aes(fill = Contract))
p32 <- ggplot(churn, aes(PaperlessBilling)) +
  geom_bar(aes(fill = PaperlessBilling))
grid.arrange(p30, p31, p32, ncol=2)

p33 <-ggplot(churn, aes(PaymentMethod)) +
  geom_bar(aes(fill = PaymentMethod))
p34 <-ggplot(churn, aes(tenure_group)) +
  geom_bar(aes(fill = tenure_group))
grid.arrange(p33, p34, ncol=1)

ggplot(churn, aes(x = gender, fill = Churn)) + geom_bar(position = "stack")

#Split date into training and testing sets
intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]
#Check accuracy of split
dim(training); dim(testing)

#Create randomforest and view
rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

#View Confusion Matrix and Stats
pred_rf <- predict(rfModel, testing)
confusionMatrix(pred_rf, testing$Churn)

#View the randomforest error rate,OOB rate platueas after about 150 trees
plot(rfModel)

#Tune the rfmodel, OOB error is lowest at ,try = 2, use that number.
t <- tuneRF(training[, -18], training[, 18], stepFactor = 0.5, plot = 
              TRUE, ntreeTry = 150, trace = TRUE, improve = 0.05)

#Fit rfmodel, decrease of OOB error rate 
rfModel_new <- randomForest(Churn ~., data = training, ntree = 150, mtry = 2, importance = TRUE, proximity = TRUE)
print(rfModel_new)

#Confusion Matrix and Stats after tuning, accuracy and sensitivity improved
pred_rf_new <- predict(rfModel_new, testing)
confusionMatrix(pred_rf_new, testing$Churn)

#View feature importance
varImpPlot(rfModel_new, sort=T, n.var = 18, main = 'Variable Importance')

###use smote or ROCR package to balance the class