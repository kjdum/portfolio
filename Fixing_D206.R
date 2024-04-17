#Fixing missing data
#Check histogram for distribution
hist(churn_raw_data$Children)
#Impute with mean, median, or mode
churn_raw_data$Children[is.na(churn_raw_data$Children)] <- median(churn_raw_data$Children, na.rm = TRUE)
#check the column has no missing values
colSums(is.na(churn_raw_data))

hist(churn_raw_data$Age)
churn_raw_data$Age[is.na(churn_raw_data$Age)] <- mean(churn_raw_data$Age, na.rm =TRUE)
churn_raw_data$Age <- round(churn_raw_data$Age, 0)                                    

hist(churn_raw_data$Income)
churn_raw_data$Income[is.na(churn_raw_data$Income)] <- median(churn_raw_data$Income, na.rm = TRUE)

churn_raw_data$Phone[is.na(churn_raw_data$Phone)] <- (names(which.max(table(churn_raw_data$Phone))))

churn_raw_data$Techie[is.na(churn_raw_data$Techie)] <- (names(which.max(table(churn_raw_data$Techie))))

churn_raw_data$TechSupport[is.na(churn_raw_data$TechSupport)] <- (names(which.max(table(churn_raw_data$TechSupport))))

hist(churn_raw_data$Tenure)
churn_raw_data$Tenure[is.na(churn_raw_data$Tenure)] <- as.numeric(names(which.max(table(churn_raw_data$Tenure))))

hist(churn_raw_data$Bandwidth_GB_Year)
churn_raw_data$Bandwidth_GB_Year[is.na(churn_raw_data$Bandwidth_GB_Year)] <- as.numeric(names(which.max(table(churn_raw_data$Bandwidth_GB_Year))))
colSums(is.na(churn_raw_data))

#Fixing outliers
#filters Lat to find outliers by z-score, then sets to NA
churn_raw_data$Lat[churn_raw_data$latz <= -3 | churn_raw_data$latz >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$latz)))
#Imputes new NA with median
churn_raw_data$Lat[is.na(churn_raw_data$Lat)] <- median(churn_raw_data$Lat, na.rm = TRUE)
#Verify changes
colSums(is.na(churn_raw_data))

churn_raw_data$Lng[churn_raw_data$lngz <= -3 | churn_raw_data$lngz >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$lngz)))
churn_raw_data$Lng[is.na(churn_raw_data$Lng)] <- median(churn_raw_data$Lng, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Population[churn_raw_data$populationz <= -3 | churn_raw_data$populationz >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$populationz)))
churn_raw_data$Population[is.na(churn_raw_data$Population)] <- median(churn_raw_data$Population, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Children[churn_raw_data$childrenz <= -3 | churn_raw_data$childrenz >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$childrenz)))
churn_raw_data$Children[is.na(churn_raw_data$Children)] <- median(churn_raw_data$Children, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Age[churn_raw_data$agez <= -3 | churn_raw_data$agez >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$agez)))
churn_raw_data$Age[is.na(churn_raw_data$Age)] <- median(churn_raw_data$Age, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Income[churn_raw_data$incomez <= -3 | churn_raw_data$incomez >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$incomez)))
churn_raw_data$Income[is.na(churn_raw_data$Income)] <- median(churn_raw_data$Income, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Outage_sec_perweek[churn_raw_data$outagez <= -3 | churn_raw_data$outagez >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$outagez)))
churn_raw_data$Outage_sec_perweek[is.na(churn_raw_data$Outage_sec_perweek)] <- median(churn_raw_data$Outage_sec_perweek, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Email[churn_raw_data$emailz <= -3 | churn_raw_data$emailz >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$emailz)))
churn_raw_data$Email[is.na(churn_raw_data$Email)] <- median(churn_raw_data$Email, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Contacts[churn_raw_data$contactsz <= -3 | churn_raw_data$contactsz >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$contactsz)))
churn_raw_data$Contacts[is.na(churn_raw_data$Contacts)] <- median(churn_raw_data$Contacts, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Yearly_equip_failure[churn_raw_data$yearlyz <= -3 | churn_raw_data$yearlyz >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$yearlyz)))
churn_raw_data$Yearly_equip_failure[is.na(churn_raw_data$Yearly_equip_failure)] <- median(churn_raw_data$Yearly_equip_failure, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Tenure[churn_raw_data$tenurez <= -3 | churn_raw_data$tenurez >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$tenurez)))
churn_raw_data$Tenure[is.na(churn_raw_data$Tenure)] <- median(churn_raw_data$Tenure, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$MonthlyCharge[churn_raw_data$monthlyz <= -3 | churn_raw_data$monthlyz >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$monthlyz)))
churn_raw_data$MonthlyCharge[is.na(churn_raw_data$MonthlyCharge)] <- median(churn_raw_data$MonthlyCharge, na.rm = TRUE)
colSums(is.na(churn_raw_data))

churn_raw_data$Bandwidth_GB_Year[churn_raw_data$bandwidthz <= -3 | churn_raw_data$bandwidthz >= 3] <- NA
colSums(is.na(churn_raw_data))
sum(is.na((churn_raw_data$bandwidthz)))
churn_raw_data$Bandwidth_GB_Year[is.na(churn_raw_data$Bandwidth_GB_Year)] <- median(churn_raw_data$Bandwidth_GB_Year, na.rm = TRUE)
colSums(is.na(churn_raw_data))

#Re-expressing categorical variable : Education
unique(churn_raw_data$Education)
#Ordinal Encoding
edu.num <- revalue(x = churn_raw_data$Education, replace = c("No Schooling Completed" = 0,"Nursery School to 8th Grade" = 1, "9th Grade to 12th Grade, No Diploma" = 2, "GED or Alternative Credential" = 3, "Regular High School Diploma" = 4, "Some College, Less than 1 Year" = 5, "Some College, 1 or More Years, No Degree" = 6,"Associate's Degree" = 7, "Bachelor's Degree" = 8, "Master's Degree" = 9, "Professional School Degree" = 10, "Doctorate Degree" = 11  ))
churn_raw_data$Education_numeric <- as.numeric(edu.num)

#Drop z-score columns
churn_clean <- subset(churn_raw_data, select = -c(latz, lngz, populationz, childrenz, agez, incomez, outagez, emailz, contactsz, yearlyz, tenurez, monthlyz, bandwidthz))
colSums(is.na(churn_clean))
