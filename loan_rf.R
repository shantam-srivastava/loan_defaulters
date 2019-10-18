#------------LOAN DATA ASSIGNMENT---------#

#-----DATA DICTIONARY-----#
#.	credit.policy: 1 if the customer meets the credit underwriting criteria of LendingClub.com, and 0 otherwise.
#.	purpose: The purpose of the loan (takes values "credit_card", "debt_consolidation", "educational", "major_purchase", "small_business", and "all_other").
#.	int.rate: The interest rate of the loan, as a proportion (a rate of 11% would be stored as 0.11). Borrowers judged by LendingClub.com to be more risky are assigned higher interest rates.
#.	installment: The monthly installments owed by the borrower if the loan is funded.
#.	log.annual.inc: The natural log of the self-reported annual income of the borrower.
#.	dti: The debt-to-income ratio of the borrower (amount of debt divided by annual income).
#.	fico: The FICO credit score of the borrower.
#.	days.with.cr.line: The number of days the borrower has had a credit line.
#.	revol.bal: The borrower's revolving balance (amount unpaid at the end of the credit card billing cycle).
#.	revol.util: The borrower's revolving line utilization rate (the amount of the credit line used relative to total credit available).
#.	inq.last.6mths: The borrower's number of inquiries by creditors in the last 6 months.
#.	delinq.2yrs: The number of times the borrower had been 30+ days past due on a payment in the past 2 years.
#.	pub.rec: The borrower's number of derogatory public records (bankruptcy filings, tax liens, or judgments).


##Importing the Dataset
loan_data <- read.csv("loan_data.csv", stringsAsFactors = F, check.names = F)

##Structure of the data.
str(loan_data)
summary(loan_data)  

##packages
library(dplyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(caret)
library(car)
library(caTools)
library(mice)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(dummies)

##DATA CLEANSING
#NA's
colSums(is.na(loan_data))  ##no NA's in the whole data.

#BLANKS
colSums(loan_data=="")  ##no BLANKS

#cheking distinct values.
sapply(loan_data, n_distinct)  ##Too many categories in some categorical variable.

#Duplicates
sum(duplicated(loan_data)) #no duplicates.

####
table(loan_data$not.fully.paid)
prop.table(table(loan_data$not.fully.paid))*100    ##only 16% peole did not pay full loan. Target variable is quite imbalanced.
##84% people paid the full the full loan.


####_____EDA_____####

##DATA VISUALISATION based on each variable.
#creating seperate functions for visualising categorical and continous variables.
cat_plot <- function(cat_var){
  p1 <- ggplot(loan_data[1:9578,],aes(x = loan_data[1:9578,cat_var], fill = as.factor(not.fully.paid))) + 
    geom_bar(col = "black") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = cat_var, y = 'not.fully.paid')
  p2 <- ggplot(loan_data[1:9578,],aes(x = loan_data[1:9578,cat_var], fill = as.factor(not.fully.paid))) + 
    geom_bar(col = "black",position = "fill") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = cat_var, y = "not.fully.paid")
  grid.arrange(p1, p2)}


cont_plot <- function(cont_var){
  p1 <- ggplot(loan_data[1:9578,],aes(x = loan_data[1:9578,cont_var], fill = as.factor(not.fully.paid))) + 
    geom_histogram(col = "black", bins = 10) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = cont_var, y = "not.fully.paid")
  p2 <- ggplot(loan_data[1:9578,],aes(x = loan_data[1:9578,cont_var], fill = as.factor(not.fully.paid)))+ 
    geom_histogram(col = "black",position = "fill", bins = 10) +theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = cont_var, y = "not.fully.paid")
  grid.arrange(p1, p2)}


#######UNIVARIATE/BIVARIATE ANALYSIS#######

### 1.CREDIT POLICY
table(loan_data$credit.policy)
prop.table(table(loan_data$credit.policy))*100 #19.5% people got the loan without meeting the credit underwriting criteria. Quite STRANGE.

cat_plot("credit.policy") #approx. 25% of people who did not meet the credit underwriting criteria are defaulters.
#approx. 13% of people who meet the credit underwriting criteria are defaulters.

#lets try to find out how so many people got the loan without meeting the credit underwriting criteria?
credit.policy0 <- subset(loan_data, credit.policy==0)
summary(credit.policy0)                     

credit.policy1 <- subset(loan_data, credit.policy==1)
summary(credit.policy1)                     
#on comparing the two summaries we can point that the people who got the loan without meeting the credit underwriting citeria have
#in general HIGHER INTEREsT RATES, LOW INSTALLMENTS, SLIGHTLY LOW ANNUAL INCOME, HIGH DEBT TO INCOME RATIO,LOW CREDIT SCORE, LESS DAYS FOR CREDIT LINE,
# LOW UNPAID BALANCE,HIGHER EXPENDITURE, MORE ENQUIRIES BY CREDITORS, OVERDUE ISSUES OVER 30 DAYS, MORE UNACCEPTABLe CASES.
####Maybe these are the ones who are in "desperate" need of money.
knitr::kable(table(credit.policy0$purpose,credit.policy0$not.fully.paid))
knitr::kable(table(credit.policy1$purpose,credit.policy1$not.fully.paid))
#clearly the ratio of defaulters from credit policy=0 is much higher.


### 2. PURPOSE
cat_plot("purpose")  # maximum people took the loan for "debt consolidation" but maximum percentage of defaulters belongs to "small business's group".
prop.table(table(loan_data$purpose))*100
knitr::kable(table(loan_data$purpose,loan_data$not.fully.paid)) 
n_distinct(loan_data$purpose)

### 3. INTEREST RATE
cont_plot("int.rate") #not much clear with the graph.

n_distinct(loan_data$int.rate)

plot(quantile(loan_data$int.rate, seq(0,1,0.01))) #outliers present.lets check.
quantile(loan_data$int.rate, seq(0,1,0.01))

#outlier treatment
loan_data$int.rate[loan_data$int.rate <0.07] <- 0.068
loan_data$int.rate[loan_data$int.rate >0.189] <- 0.194
plot(quantile(loan_data$int.rate, seq(0,1,0.01))) #seems much better.

### 4. INSTALLMENTS
n_distinct(loan_data$installment)
cont_plot("installment")

plot(quantile(loan_data$installment, seq(0,1,0.01))) #outliers present.lets check.
quantile(loan_data$installment, seq(0,1,0.01))

#outlier treatment
loan_data$installment[loan_data$installment >871] <- 895
plot(quantile(loan_data$installment, seq(0,1,0.01)))


### 5. LOG ANNUAL INCOME

cont_plot("log.annual.inc")

n_distinct(loan_data$log.annual.inc)

plot(quantile(loan_data$log.annual.inc, seq(0,1,0.01))) #outliers present.lets check.
quantile(loan_data$log.annual.inc, seq(0,1,0.01))

#outlier treatment
loan_data$log.annual.inc[loan_data$log.annual.inc <7.6] <- 9
plot(quantile(loan_data$log.annual.inc, seq(0,1,0.01)))
loan_data$log.annual.inc[loan_data$log.annual.inc >12.5] <- 13          
plot(quantile(loan_data$installment, seq(0,1,0.01)))     #seems much better.

### 6.DEBT TO INCOME RATIO

cont_plot("dti")
n_distinct(loan_data$dti)   

plot(quantile(loan_data$dti, seq(0,1,0.01))) #outliers present.lets check.
quantile(loan_data$dti, seq(0,1,0.01))

#outlier treatment
loan_data$dti[loan_data$dti>26.5] <- 27               
plot(quantile(loan_data$dti, seq(0,1,0.01)))

### 7. FICO
n_distinct(loan_data$fico)
cont_plot("fico")       #maximum defaulters belongs to low credit score.

plot(quantile(loan_data$fico, seq(0,1,0.01))) 
quantile(loan_data$fico, seq(0,1,0.01))

#outlier treatment
loan_data$fico[loan_data$fico>802] <- 808               
plot(quantile(loan_data$fico, seq(0,1,0.01)))

loan_data$fico[loan_data$fico<642] <- 637               
plot(quantile(loan_data$fico, seq(0,1,0.01)))


### 8. DAYS WITH CREDIT LINE
n_distinct(loan_data$days.with.cr.line)
cont_plot("days.with.cr.line")  ##maximum no. of defaulters does not have long time to repay their loan's but highest defaulters are from longest time group.
summary(loan_data$days.with.cr.line)  #(after checking the summary)

days_cred_line <-loan_data%>%group_by(days.with.cr.line)%>%summarise(total_defaulters=sum(not.fully.paid,na.rm = T))%>%arrange(desc(total_defaulters)) 

plot(quantile(loan_data$days.with.cr.line, seq(0,1,0.01))) #outlier present.
quantile(loan_data$days.with.cr.line, seq(0,1,0.01))

#outlier treatment
loan_data$days.with.cr.line[loan_data$days.with.cr.line>12931] <- 14300             
plot(quantile(loan_data$days.with.cr.line, seq(0,1,0.01)))

### 9. REVOLVING BALANCE
n_distinct(loan_data$revol.bal)
cont_plot("revol.bal")

summary(loan_data$revol.bal)

revol_bal<- loan_data%>%group_by(revol.bal)%>%summarise(total_defaulters=sum(not.fully.paid,na.rm = T))%>%arrange(desc(total_defaulters)) 
#hence, maximum defaulters does not have outstanding balance. #STRANGE.

plot(quantile(loan_data$revol.bal, seq(0,1,0.01))) #outlier present.
quantile(loan_data$revol.bal, seq(0,1,0.01))

#outlier treatment
loan_data$revol.bal[loan_data$revol.bal>100000] <- 91000              
plot(quantile(loan_data$days.with.cr.line, seq(0,1,0.01))) ##seems fair.


### 10. REVOLVING LINE UTILIZATION RATE (Amount of credit line used relative to availability)
n_distinct(loan_data$revol.util)
cont_plot("revol.util") 
summary(loan_data$revol.util) 


revol_util<- loan_data%>%group_by(revol.util)%>%summarise(total_defaulters=sum(not.fully.paid,na.rm = T))%>%arrange(desc(total_defaulters)) 

plot(quantile(loan_data$revol.util, seq(0,1,0.01))) #outlier present.
quantile(loan_data$revol.util, seq(0,1,0.01))

#outlier treatment
loan_data$revol.util[loan_data$revol.util>100] <- 100.5              
plot(quantile(loan_data$revol.util, seq(0,1,0.01)))

### 11. ENQUIRIES IN LAST 6 MONTHS
n_distinct(loan_data$inq.last.6mths)
summary(loan_data$inq.last.6mths)
enq_6_mths <- loan_data%>%group_by(inq.last.6mths)%>%summarise(total_defaulters=sum(not.fully.paid,na.rm = T))%>%arrange(desc(total_defaulters)) 
#max. no. of defaluters were those to whom less no. of inquiries were made, and less defaulters were those to whom more enquiries were made.
cont_plot("inq.last.6mths")

plot(quantile(loan_data$inq.last.6mths, seq(0,1,0.01))) #outlier present.
quantile(loan_data$inq.last.6mths, seq(0,1,0.01))
table(loan_data$inq.last.6mths)

#outlier treatment
loan_data$inq.last.6mths[loan_data$inq.last.6mths>16] <- 17           
plot(quantile(loan_data$inq.last.6mths, seq(0,1,0.01)))


### 12. DELINQ 2 YEARS (The no. of times payment is overdue by more than 30 days)
n_distinct(loan_data$delinq.2yrs)
table(loan_data$delinq.2yrs)
cat_plot("delinq.2yrs")

enq_6_mths <- loan_data%>%group_by(inq.last.6mths)%>%summarise(total_defaulters=sum(not.fully.paid,na.rm = T))%>%arrange(desc(total_defaulters))


### 13. PUBLIC RECORDS
n_distinct(loan_data$pub.rec)
table(loan_data$pub.rec)
cat_plot("pub.rec")

### 14. NOT FULLY PAID- TARGET VARIABLE
#leaving it as it is.

###----FEATURE ENGINEERING---###

### 15. ACTUAL INCOME OF AN INDIVISUAL
loan_data$actual.income <- 2.72^loan_data$log.annual.inc

### 16. APPROXIMATE DEBT OF AN INDIVISUAL
loan_data$debt <- loan_data$actual.income*loan_data$dti

### 17. CREDIT LINE IN YEARS
loan_data$cr.line.yrs <- loan_data$days.with.cr.line/365

### 18. JOINING THE COLUMN CREDIT POLICY AND PUBLIC RECODRS SEPERATED BY "&"
loan_data$policy_records <- paste(loan_data$credit.policy, loan_data$pub.rec, sep = " & ")

### 19. YEARLY INSTALLMENTS
loan_data$yearly.installments <- loan_data$installment*((1+loan_data$int.rate)^loan_data$cr.line.yrs)

loan_data_targetvar <- loan_data$not.fully.paid
loan_data$not.fully.paid <- NULL
loan_data$not.fully.paid <- loan_data_targetvar


##deleting unecessary variables
rm(credit.policy0,credit.policy1,days_cred_line,enq_6_mths,revol_bal,revol_util)

#Separating categorical and continous variables
colnames(loan_data)
sapply(loan_data, n_distinct)

cont_vars <- loan_data[,c("int.rate", "installment", "log.annual.inc","dti","days.with.cr.line",
                          "fico", "revol.bal", "revol.util", "inq.last.6mths", "actual.income", "debt", "cr.line.yrs",
                          "yearly.installments")]


# Check correlation among continuous variables
cormat <- cor(cont_vars)
require(corrplot)
corrplot(cormat, method = 'number')
#relatively less correlation between the variables, but FICO has -ve correlation with the other variables.

# Scale continuous variables
cont_vars <- as.data.frame(sapply(cont_vars, scale))

# categorical variables
cat_vars <- loan_data[, !colnames(loan_data) %in% colnames(cont_vars)]
names(cat_vars)

summary(cat_vars)

# Converting to factor (as categorical)
cat_vars <- as.data.frame(sapply(cat_vars, as.factor))
summary(cat_vars)

#combining the categorical and continuous variables
loan_data1 <- cbind(cont_vars, cat_vars)

# EDA Complete...

#### ---- Model Building ---- ####

prop.table( table(loan_data1$not.fully.paid)) * 100
table(loan_data1$not.fully.paid)
#target variable imbalanced.

require(DMwR)
set.seed(123)
loan_data1 <- SMOTE(not.fully.paid ~ ., data = loan_data1, perc.over = 500, perc.under = 200)

prop.table(table(loan_data1$not.fully.paid)) * 100
table(loan_data1$not.fully.paid)

require(caTools)

set.seed(999)
index = sample.split(loan_data1$not.fully.paid, SplitRatio = 0.75)

train <- loan_data1[index, ]
test <- loan_data1[!index, ]

require(randomForest)

rf_model <- randomForest(not.fully.paid ~ ., data = train, ntree = 1000,
                         proximity = F, do.trace = T, importance = T)

predicted_probability <- predict(rf_model, test[,-19], type = "prob")[,2]
summary(predicted_probability)

actual_not.fully.paid <- as.factor(test$not.fully.paid)

OUT <- matrix(nrow = 100, ncol = 3)
s <- seq(min(predicted_probability), max(predicted_probability), length.out = 100)

require(caret)

cutoff_finder <- function(cutoff) {
  pred_not.fully.paid <- factor(ifelse(predicted_probability >= cutoff, "1", "0"))
  conf <- confusionMatrix(pred_not.fully.paid, actual_not.fully.paid, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- c(sens, spec, acc) 
  return(out) }

for(j in 1:100) {OUT[j,] <- cutoff_finder(s[j])}


cutoff <- s[which.min(abs(OUT[,1]-OUT[,2]))]
cutoff

# Thus, predicting final booking as per cutoff value of predicted probability
pred_not.fully.paid <- factor(ifelse(predicted_probability >= cutoff, "1", "0"))

con_mat <- confusionMatrix(pred_not.fully.paid, actual_not.fully.paid, positive = "1")
con_mat

# Accuracy : 0.9302;  Sensitivity : 0.9296;   Specificity : 0.9306