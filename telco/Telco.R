#Class ProjeCt

#Class: 367919 - Introduction to Data Science
#Student: Shiqi Li 
#Professor: Daniel Gutierrez
#--------------------------------------------------------------------------------
library(randomForest)
library(survival)	
library(dplyr)
library(ggplot2)
library(Hmisc)
library(ggthemes)

#1.Access the Data Set
telco <- read.csv("~/Shiqi.RDATA/telco/Telco-Customer-Churn.csv")
head(telco)
summary(telco)

#2.cleaning the data preliminary
telco$TotalCharges <- ifelse(is.na(telco$TotalCharges==TRUE),telco$MonthlyCharges*telco$tenure,telco$TotalCharges)
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1, 'YES', 'NO'))

#3.Exploratory Data Analysis (EDA)
#churn~gender/senior/partner/dependents/PhoneService/MultipleLines/InternetService/OnlineSecurity/
#TechSupport/StreamingTV/StreamingMovies/Contract/PaperlessBilling/PaymentMethod
ggplot(telco)+geom_bar(aes(x=gender,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=SeniorCitizen,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=Partner,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=Dependents,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=PhoneService,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=MultipleLines,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=InternetService,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=OnlineSecurity,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=OnlineBackup,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=DeviceProtection,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=TechSupport,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=StreamingTV,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=StreamingMovies,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=Contract,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=PaperlessBilling,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
ggplot(telco)+geom_bar(aes(x=PaymentMethod,fill=Churn),stat="count",alpha = 0.7,width=0.5)+theme_bw()
#churn~MonthlyCharges/TotalCharges
ggplot(telco)+geom_point(aes(x=MonthlyCharges,y=tenure), colour="skyblue")+theme_bw()
ggplot(telco)+geom_point(aes(x=TotalCharges,y=tenure),colour="skyblue")+theme_bw()

#4.Survival Curve
telco$Churn <- ifelse(telco$Churn=='Yes',1,0)
plot(survfit(Surv(telco$tenure,telco$Churn)~1),xlab="time customer has stayed",ylab="survival rate",col="#E7B800")
#churn~senior/dependents/PhoneService/InternetService/OnlineSecurity/TechSupport
#/Contract/PaperlessBilling/PaymentMethod
par(mfrow=c(2,2))
plot(survfit(Surv(telco$tenure,telco$Churn)~telco$SeniorCitizen),col=c("#FC4E07", "#E7B800"),lty=c(1,2),xlab="time customer has stayed",ylab="survival rate")
plot(survfit(Surv(telco$tenure,telco$Churn)~telco$Dependents),col=c("#FC4E07", "#E7B800"),lty=c(1,2),xlab="time customer has stayed",ylab="survival rate")
plot(survfit(Surv(telco$tenure,telco$Churn)~telco$PhoneService),col=c("#FC4E07", "#E7B800"),lty=c(1,2),xlab="time customer has stayed",ylab="survival rate")
plot(survfit(Surv(telco$tenure,telco$Churn)~telco$InternetService),col=c("#FC4E07", "#E7B800"),lty=c(1,2),xlab="time customer has stayed",ylab="survival rate")
plot(survfit(Surv(telco$tenure,telco$Churn)~telco$OnlineSecurity),col=c("#FC4E07", "#E7B800"),lty=c(1,2),xlab="time customer has stayed",ylab="survival rate")
plot(survfit(Surv(telco$tenure,telco$Churn)~telco$TechSupport),col=c("#FC4E07", "#E7B800"),lty=c(1,2),xlab="time customer has stayed",ylab="survival rate")
plot(survfit(Surv(telco$tenure,telco$Churn)~telco$Contract),col=c("#FC4E07", "#E7B800"),lty=c(1,2),xlab="time customer has stayed",ylab="survival rate")
plot(survfit(Surv(telco$tenure,telco$Churn)~telco$PaperlessBilling),col=c("#FC4E07", "#E7B800"),lty=c(1,2),xlab="time customer has stayed",ylab="survival rate")
plot(survfit(Surv(telco$tenure,telco$Churn)~telco$PaymentMethod),col=c("#FC4E07", "#E7B800"),lty=c(1,2),xlab="time customer has stayed",ylab="survival rate")

#5.Data Transforrmation
telco$Churn <- as.factor(telco$Churn)
#replce "No internet service/No phone service" with "No"
telco_sub <- telco[,c(-1,-6, -19,-20,-21)] 
telco_sub <- data.frame(lapply(telco_sub, function(x) {gsub("No internet service", "No", x)}))
telco_sub <- data.frame(lapply(telco_sub, function(x) {gsub("No phone service", "No", x)}))
telco_final<-cbind(telco[,c(1,6, 19,20,21)],telco_sub)

#6.Creating Training and Test Sets
n <- nrow(telco_final)
ntrain <- round(n*0.7)
set.seed(116)
tindex <- sample(n,ntrain)

train <- telco_final[tindex,] 
test <- telco_final[-tindex,]  

#7.Supervised Machine Leaning

#random forest
#use random forest to predice if the customers will stay or not using all predictors
rf <- randomForest(telco_sub,y=telco$Churn, ntree=500, mtry=2, importance=TRUE)
#use trained model rf, predict test set response values
prediction <- predict(rf, newdata=test, type="class")
table(prediction, test$Churn)
misclassification_error_rate <- sum(test$Churn != prediction) / nrow(test)*100
importance(rf)

#cox model
telco_final$Churn <- ifelse(telco_final$Churn==0,FALSE,TRUE)
fit=coxph(Surv(tenure,Churn)~SeniorCitizen+Dependents+PhoneService+InternetService+OnlineSecurity+TechSupport+Contract+PaperlessBilling+PaymentMethod+TotalCharges+MonthlyCharges,data=telco_final)	
summary(fit)		
