#Decision Tree

Data <-read.csv("cleaned_salaries_data", stringsAsFactors = TRUE, header = TRUE)
str(Data)


#create dummy variables

#location
Data$CompanyLocation <- ifelse(Data$company_location == "United States", "United States","Foreign Country")
Data$SalaryCurrency <- ifelse(Data$salary_currency== "USD", "USD", "NonUSD")

#roles
Data$JobGroup_Analyst_Scientist <- ifelse(
  grepl("scientist|analyst|research", tolower(Data$job_title)), 1, 0
)

Data$JobGroup_Engineer_Developer <- ifelse(
  grepl("engineer|developer", tolower(Data$job_title)), 1, 0
)

#Leadership_Other" is the leftover group
Data$JobGroup_Leadership_Other <- ifelse(
  Data$JobGroup_Analyst_Scientist == 0 & Data$JobGroup_Engineer_Developer == 0, 1, 0
)


#split data into training and testing data
set.seed(355)
train <- sample(1:nrow(Data), nrow(Data)*(2/3))
train
Data.train <- Data[train,]
Data.test <- Data[-train,]

#grow tree
fit <- rpart(sixfigures_or_not ~.
             -salary
             -salary_in_usd
             -employee_residence
             -job_title
             -company_location
             -salary_currency
             -SalaryCurrency
             -work_year,
             data = Data.train,
             method = "class",
             control = rpart.control(xval = 0, minsplit = 100),
             parms = list(split = "gini"))
fit
rpart.plot(fit, type = 1, extra = 1)

#Extract predicted
Data.pred <- predict(fit,Data.train,type = "class")
Data.actual <- Data.train$sixfigures_or_not
confusion.matrix <-table(Data.pred,Data.actual)
confusion.matrix


#Accuracy on Training Data
pt <- (prop.table(confusion.matrix))
pt[1,1]+pt[2,2]

#Accuracy on Testing Data
Data.pred2 <- predict(fit,Data.test,type = "class")
Data.actual2 <- Data.test$sixfigures_or_not
confusion.matrix2 <- table(Data.pred2, Data.actual2)
cm <- confusion.matrix2
pt2 <- prop.table(confusion.matrix2)
pt2[1,1]+pt2[2,2]


tp <- cm[2,2]
tn <- cm[1,1]
fp <- cm[2,1]
fn <- cm[1,2]

# Accuracy
(tp + tn)/(tp + tn + fp + fn)

# TPR = Recall = Sensitivity
tp/(fn+tp)

# TNR = Specificity
tn/(fp+tn)

# FPR
fp/(fp+tn)

# FNR
fn/(fn+tp)

