library(dplyr)

#read the file
salaries<- read.csv("data_science_salaries.csv",stringsAsFactors = TRUE, header=TRUE)

#understand the file
summary(salaries)

#check for missing values
is.na(salaries)
colSums(is.na(salaries))

#identify outliers using a boxplot
v1 <- boxplot(salaries$salary_in_usd, horizontal = TRUE, xaxt="n",
              main = "Boxplot of Data Science Salaries (USD)",
              pars = list(outpch = 21, outbg = "pink"))

min_outlier <- min(v1$out)
max_outlier <- max(v1$out)


#Remove outlier: salaries greater than 320000 and filter them out
#number of outliers:107
count(salaries %>% filter(salary_in_usd>320000))
#filter outliers
salaries <- salaries %>% filter(salary_in_usd<320000)

#create binary variable/ target variable
salaries$sixfigures_or_not <- ifelse(salaries$salary_in_usd>=100000, 1,0)

#save the cleaned data
write.csv(salaries,"cleaned_salaries_data")
