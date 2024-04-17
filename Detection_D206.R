#Detection Inputs
#load all Packages and read in data
library(readr)
library(dplyr)
library(ggplot2)
library(visdat)
library(plyr)
library(factoextra)
#Import Dataset
churn_raw_data <- read_csv("Downloads/d206-churn.dictionary.files (1)/churn_raw_data.csv")
View(churn_raw_data)

#check for duplicates
duplicated(churn_raw_data)
n_distinct(churn_raw_data$CaseOrder)
n_distinct(churn_raw_data$Customer_id)
n_distinct(churn_raw_data$Interaction)
sum(duplicated(churn_raw_data))

#Check for missing values
is.na(churn_raw_data)
sum(is.na(churn_raw_data))
colSums(is.na(churn_raw_data))

#visualize missing values
vis_miss(churn_raw_data)
str(churn_raw_data)

#Check Education for all value rankings
unique(churn_raw_data$Education)
View(churn_raw_data)

#Detecting Outliers
#scale z-value and create column in main df
churn_raw_data$latz <- scale(churn_raw_data$Lat, center = TRUE, scale = TRUE)
#subset outliers into separate df
lat_outliers <- churn_raw_data[which(churn_raw_data$latz < -3 | churn_raw_data$latz > 3),]
str(lat_outliers)
#getting outlier range from boxplots
boxplot(churn_raw_data$Lat)
#query outlier range
lat_query <- churn_raw_data[which(churn_raw_data$Lat < 25 | churn_raw_data$Lat > 50), ]
select(lat_query, Customer_id, Lat)

churn_raw_data$lngz <- scale(churn_raw_data$Lng, center = TRUE, scale = TRUE)
lng_outliers <- churn_raw_data[which(churn_raw_data$lngz < -3 | churn_raw_data$lngz > 3),]
str(lng_outliers)
boxplot(churn_raw_data$Lng)
lng_query <- churn_raw_data[which(churn_raw_data$Lng < -120 | churn_raw_data$Lng > -65), ]
select(lng_query, Customer_id, Lng)

churn_raw_data$populationz <- scale(churn_raw_data$Population, center = TRUE, scale = TRUE)
population_outliers <- churn_raw_data[which(churn_raw_data$populationz < -3 | churn_raw_data$populationz > 3),]
str(population_outliers)
boxplot(churn_raw_data$Population)
population_query <- churn_raw_data[which(churn_raw_data$Population < 0e+00 | churn_raw_data$Population > 4e+04), ]
select(population_query, Customer_id, Population)

churn_raw_data$childrenz <- scale(churn_raw_data$Children, center = TRUE, scale = TRUE)
children_outliers <- churn_raw_data[which(churn_raw_data$childrenz < -3 | churn_raw_data$childrenz > 3),]
str(children_outliers)
boxplot(churn_raw_data$Children)
children_query <- churn_raw_data[which(churn_raw_data$Children < 0 | churn_raw_data$Children > 7), ]
select(children_query, Customer_id, Children)

churn_raw_data$agez <- scale(churn_raw_data$Age, center = TRUE, scale = TRUE)
age_outliers <- churn_raw_data[which(churn_raw_data$agez < -3 | churn_raw_data$agez > 3),]
str(age_outliers)
boxplot(churn_raw_data$Age)
age_query <- churn_raw_data[which(churn_raw_data$Age < 17 | churn_raw_data$Age > 90), ]
select(age_query, Customer_id, Age)

churn_raw_data$incomez <- scale(churn_raw_data$Income, center = TRUE, scale = TRUE)
income_outliers <- churn_raw_data[which(churn_raw_data$incomez < -3 | churn_raw_data$incomez > 3),]
str(income_outliers)
boxplot(churn_raw_data$Income)
income_query <- churn_raw_data[which(churn_raw_data$Income < 0 | churn_raw_data$Income > 120000), ]
select(income_query, Customer_id, Income)

churn_raw_data$outagez <- scale(churn_raw_data$Outage_sec_perweek, center = TRUE, scale = TRUE)
outage_outliers <- churn_raw_data[which(churn_raw_data$outagez < -3 | churn_raw_data$outagez > 3),]
str(outage_outliers)
boxplot(churn_raw_data$Outage_sec_perweek)
outage_query <- churn_raw_data[which(churn_raw_data$Outage_sec_perweek < 1 | churn_raw_data$Outage_sec_perweek > 20), ]
select(outage_query, Customer_id, Outage_sec_perweek)

churn_raw_data$emailz <- scale(churn_raw_data$Email, center = TRUE, scale = TRUE)
email_outliers <- churn_raw_data[which(churn_raw_data$emailz < -3 | churn_raw_data$emailz > 3),]
str(email_outliers)
boxplot(churn_raw_data$Email)
email_query <- churn_raw_data[which(churn_raw_data$Email < 3 | churn_raw_data$Email > 21), ]
select(email_query, Customer_id, Email)

churn_raw_data$contactsz <- scale(churn_raw_data$Contacts, center = TRUE, scale = TRUE)
contacts_outliers <- churn_raw_data[which(churn_raw_data$contactsz < -3 | churn_raw_data$contactsz > 3),]
str(contacts_outliers)
boxplot(churn_raw_data$Contacts)
contacts_query <- churn_raw_data[which(churn_raw_data$Contacts < 0 | churn_raw_data$Contacts > 5), ]
select(contacts_query, Customer_id, Contacts)

churn_raw_data$yearlyz <- scale(churn_raw_data$Yearly_equip_failure, center = TRUE, scale = TRUE)
yearly_outliers <- churn_raw_data[which(churn_raw_data$yearlyz < -3 | churn_raw_data$yearlyz > 3),]
str(yearly_outliers)
boxplot(churn_raw_data$Yearly_equip_failure)
yearly_query <- churn_raw_data[which(churn_raw_data$Yearly_equip_failure < 0 | churn_raw_data$Yearly_equip_failure > 2), ]
select(yearly_query, Customer_id, Yearly_equip_failure)

churn_raw_data$tenurez <- scale(churn_raw_data$Tenure, center = TRUE, scale = TRUE)
tenure_outliers <- churn_raw_data[which(churn_raw_data$tenurez < -3 | churn_raw_data$tenurez > 3),]
str(tenure_outliers)
boxplot(churn_raw_data$Tenure)
tenure_query <- churn_raw_data[which(churn_raw_data$Tenure < 0 | churn_raw_data$Tenure > 73), ]
select(tenure_query, Customer_id, Tenure)

churn_raw_data$monthlyz <- scale(churn_raw_data$MonthlyCharge, center = TRUE, scale = TRUE)
monthly_outliers <- churn_raw_data[which(churn_raw_data$monthlyz < -3 | churn_raw_data$monthlyz > 3),]
str(monthly_outliers)
boxplot(churn_raw_data$MonthlyCharge)
monthly_query <- churn_raw_data[which(churn_raw_data$MonthlyCharge < 60 | churn_raw_data$MonthlyCharge > 300), ]
select(monthly_query, Customer_id, MonthlyCharge)

churn_raw_data$bandwidthz <- scale(churn_raw_data$Bandwidth_GB_Year, center = TRUE, scale = TRUE)
bandwidth_outliers <- churn_raw_data[which(churn_raw_data$bandwidthz < -3 | churn_raw_data$bandwidthz > 3),]
str(bandwidth_outliers)
boxplot(churn_raw_data$Bandwidth_GB_Year)
bandwidth_query <- churn_raw_data[which(churn_raw_data$Bandwidth_GB_Year < 0 | churn_raw_data$Bandwidth_GB_Year > 7200), ]
select(bandwidth_query, Customer_id, Bandwidth_GB_Year)


