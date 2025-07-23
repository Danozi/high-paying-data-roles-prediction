#Logistic Regression
rm(list = ls())

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
set.seed(123)
train <- sample(1:nrow(Data), nrow(Data)*(2/3))
train
Data.train <- Data[train,]
Data.test <- Data[-train,]


#regression
logit.reg <- glm(sixfigures_or_not ~ experience_level + employment_type + work_models +
                   company_size + CompanyLocation + JobGroup_Leadership_Other +
                   JobGroup_Analyst_Scientist + JobGroup_Engineer_Developer, 
                   data=Data.train, family = "binomial")

summary(logit.reg)

#Confusion Matrix Predict probabilities of being class 1 (six-figure)
logit.predict <- predict(logit.reg, Data.test,type = "response")
logitpredictclass <- ifelse(logit.predict > 0.5,1,0)

actual <- Data.test$sixfigures_or_not
predict <- logitpredictclass
cm <- table(predict,actual)
cm

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


