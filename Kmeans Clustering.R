#clean the environment
rm(list = ls())

#load the csv
data <- read.csv("Group Project 1 Data Science Salaries (1).csv", 
                 stringsAsFactors = FALSE, header = TRUE)
str(data)
head(data)


#Find out unique company locations
unique_locations <- unique(data$company_location)
print(unique_locations)

# Create dummy variables for company location based on continent

data$region_Europe <- ifelse(data$company_location %in% 
                               c("DE", "FR", "IT", "ES", "GB", "PL", "NL", "CH", 
                                 "BE", "AT", "SE", "FI", "DK", "CZ", "SK", "RO", 
                                 "PT", "EE", "SI", "HR", "LT", "LV", "MK", "BA", 
                                 "AM", "AL", "LU", "MD", "MT"), 1, 0)
data$region_Asia <- ifelse(data$company_location %in% 
                             c("IN", "HK", "CN", "JP", "PK", "SG", "TH", "VN", 
                               "ID", "MY", "IR", "IQ"), 1, 0)
data$region_Africa <- ifelse(data$company_location %in% 
                               c("NG", "EG", "MA", "DZ", "KE", "GH", "CF"), 1, 0)
data$region_Americas <- ifelse(data$company_location %in% 
                                 c("US", "CA", "PR", "CR", "BS", "HN", "AR", "CO", 
                                   "MX", "BR", "CL", "BO"), 1, 0)
data$region_Oceania <- ifelse(data$company_location %in% 
                                c("AU", "NZ", "AS"), 1, 0)


# Create dummy variables for job title
data$JobGroup_Analyst_Scientist <- ifelse(
  grepl("scientist|analyst|research", tolower(data$job_title)), 1, 0
)

data$JobGroup_Engineer_Developer <- ifelse(
  grepl("engineer|developer", tolower(data$job_title)), 1, 0
)

#Leadership_Other" is the leftover group
data$JobGroup_Leadership_Other <- ifelse(
  data$JobGroup_Analyst_Scientist == 0 & data$JobGroup_Engineer_Developer == 0, 1, 0
)

#Exclude redundant variables dependent variable
columns_to_exclude <- c("work_year", "salary", "salary_currency", "employee_residence"
                        ,"X6.Figure.or.Not","experience_level","employment_type",
                        "company_size","company_location","job_title","remote_ratio"
                        ,"company_size","experience_level")

features <- data[, !(names(data) %in% columns_to_exclude)]

#Standardization
features_scaled <- scale(features)
summary(features_scaled[,"salary_in_usd"])

#Using K - Silhouette
fviz_nbclust(features_scaled, kmeans, method=
               "silhouette")
#run kmeans clustering
set.seed(140)
kmeans_result <- kmeans(features_scaled, centers=5)
kmeans_result_table <- data.frame(kmeans_result$size, kmeans_result$centers)

#Add cluster information to data table
data$cluster <- kmeans_result$cluster


#View clusters
fviz_cluster(kmeans_result, data = features_scaled, ellipse.type = "norm", geom = "blank") + theme_minimal()
head(features_scaled)


data$SixFigure <- ifelse(data$salary_in_usd >= 100000, 1, 0)

# 2. Summary Table by Cluster
library(dplyr)

cluster_summary <- data %>%
  group_by(cluster) %>%
  summarise(
    Count = n(),
    Avg_Salary = round(mean(salary_in_usd), 0),
    Median_Salary = round(median(salary_in_usd), 0),
    Percent_6Figure = round(mean(SixFigure) * 100, 1),
    Percent_Analyst_Scientist = round(mean(JobGroup_Analyst_Scientist) * 100, 1),
    Percent_Engineer_Developer = round(mean(JobGroup_Engineer_Developer) * 100, 1),
    Percent_Leadership_Other = round(mean(JobGroup_Leadership_Other) * 100, 1),
    Europe = round(mean(region_Europe) * 100, 1),
    Americas = round(mean(region_Americas) * 100, 1),
    Asia = round(mean(region_Asia) * 100, 1),
    Africa = round(mean(region_Africa) * 100, 1),
    Oceania = round(mean(region_Oceania) * 100, 1)
  )

# 3. View the summary
print(cluster_summary)
library(ggplot2)

ggplot(cluster_summary, aes(x = factor(cluster), y = Percent_6Figure)) +
  geom_col(fill = "steelblue") +
  labs(title = "Percentage of 6-Figure Earners by Cluster",
       x = "Cluster",
       y = "% Earning 6 Figures") +
  theme_minimal()


region_data <- cluster_summary %>%
  select(cluster, Europe, Americas, Asia, Africa, Oceania) %>%
  pivot_longer(-cluster, names_to = "Region", values_to = "Percentage")

ggplot(region_data, aes(x = factor(cluster), y = Percentage, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Regional Distribution by Cluster",
       x = "Cluster",
       y = "Percentage") +
  theme_minimal()