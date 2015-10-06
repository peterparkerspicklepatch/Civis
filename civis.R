#package that we'll use to download the data
library(curl)
library(foreign)

#import data
indivudal_data <- read.csv('individual_data.csv', header = TRUE)

#see what's up
str(indivudal_data)
head(indivudal_data)

#Correct gender variable to be consistent
library(car)
levels(indivudal_data$gender)
#actual recode
indivudal_data$gender <- recode(indivudal_data$gender, "'M' = 'Male'; 'MALE' = 'Male'; 'F' = 'Female'; 'FEMALE' = 'Female'")

#check to make sure
str(indivudal_data)

#create 18 or older variable
indivudal_data$adult[indivudal_data$age >= 18] <- "1"
indivudal_data$adult[indivudal_data$age < 18] <- "0"
indivudal_data$adult <- as.factor(indivudal_data$adult)

#Create a percentage table
how_many_adults <- data.frame(table(indivudal_data$adult))
colnames(how_many_adults) <- c('Adult', 'Frequency')
how_many_adults$Precentage <- how_many_adults$Frequency / sum(how_many_adults$Frequency) * 100
how_many_adults

#Create an average table
adults_per_hh <- as.data.frame.matrix(table(indivudal_data$hh_id, indivudal_data$adult))
colnames(adults_per_hh) <- c('Children', 'Adults')
mean(adults_per_hh$Children)
mean(adults_per_hh$Adults)

#adults as college grads
adults_college <- subset(indivudal_data, adult == "1")
adults_college_table <- data.frame(table(adults_college$is_college_graduate))
colnames(adults_college_table) <- c('College?', 'Frequency')
adults_college_table$Precentage <- adults_college_table$Frequency / sum(adults_college_table$Frequency) * 100
adults_college_table

#People as college grads
people_grads <- as.data.frame(table(indivudal_data$is_college_graduate))
colnames(people_grads) <- c('College?', 'Frequency')
people_grads$Precentage <- people_grads$Frequency / sum(people_grads$Frequency) * 100
people_grads

#create household variables
#I remove children that are not heads of households from the data. I make sure this is the case using the follow table
table(indivudal_data$adult, indivudal_data$is_head_of_household)
#In doing this subset I assume children cannot be unwed couples
#Used: https://stackoverflow.com/questions/8420288/r-how-can-i-delete-rows-if-an-element-in-a-row-satisfies-certain-characteristic
indivdual_adult_data <- indivudal_data[indivudal_data$adult == 1,]

#if you're the only member of a household & you're single then you're not an unmarried couple
#Filtering for one member households that are female
#Used for mutate function: http://r4stats.com/2013/01/22/comparing-tranformation-styles/
#And to futher understand pipe operator and group by: https://stats.stackexchange.com/questions/8225/how-to-summarize-data-by-group-in-r
library(dplyr)
indivudal_data2 <- indivdual_adult_data %>% 
group_by(hh_id) %>%
mutate(hh_type = ifelse(n()==1 & gender=="Female", "single_female_hh", "0"))

#I identify one member households that are male
indivudal_data3 <- indivdual_adult_data %>% 
group_by(hh_id) %>%
mutate(hh_type = ifelse(n()==1 & gender=="Male", "single_male_hh", "0"))

#combine that single_hh varaibles
indivdual_adult_data$hh_type_female <- indivudal_data2$hh_type
indivdual_adult_data$hh_type_male <- indivudal_data3$hh_type

#combine that single_hh varaibles in one variable and code the rest
indivdual_adult_data$hh_type_final[indivdual_adult_data$hh_type_female == 'single_female_hh'] <- "single_female_hh"
indivdual_adult_data$hh_type_final[indivdual_adult_data$hh_type_male == 'single_male_hh'] <- "single_male_hh"
indivdual_adult_data$hh_type_final[indivdual_adult_data$married == 'Married']  <- "married_couple"

#Remaining one's must be unwed couples
#Used: http://rprogramming.net/recode-data-in-r/
indivdual_adult_data$hh_type_final <- recode(indivdual_adult_data$hh_type_final, "NA = 'unwed_couple'")

#table summurizing the results
#Remember that the married couple and unwed_couples appear twice so divide by 2 to count them as one unit
  hh_groups <- data.frame(table(indivdual_adult_data$hh_type_final))
colnames(hh_groups) <- c('Type', 'Frequency')
hh_groups$Frequency[1] <- (hh_groups$Frequency[1]/2)
hh_groups$Frequency[4] <- (hh_groups$Frequency[4]/2)

#create table
hh_groups$Precentage <- hh_groups$Frequency / sum(hh_groups$Frequency) * 100
hh_groups

#Read in data
hh_data<- read.csv('household_data.csv', header = TRUE)
#Merge Data
hh_and_indivdual <- merge(hh_data, indivdual_adult_data, by="hh_id")

#Create a data set that won't double count certain characteristics 
#Used: http://www.cookbook-r.com/Manipulating_data/Finding_and_removing_duplicate_records/
hh_and_indivudal_no_dup <- hh_and_indivdual[!duplicated(hh_and_indivdual$hh_id),]

#A Little exploratory
library(Hmisc)
describe(hh_and_indivdual) 

#Create race table
race <- table(hh_and_indivdual$hh_type_final, hh_and_indivdual$race)

#Do row percentages
prop.table(race, 1)

#Create Rich Bin
hh_and_indivudal_no_dup$Wealth <- cut(hh_and_indivudal_no_dup$Household.Income, breaks = c(0, 50000, 10000000000),
                                      labels = c('Below $50,000','Above $50,000'), include.lowest = TRUE)
wealth <- table(hh_and_indivudal_no_dup$hh_type_final, hh_and_indivudal_no_dup$Wealth)
prop.table(wealth, 1)

#import
r_data  <- read.csv('registration_status.csv', header = TRUE)
r_data <- na.omit(r_data)
r_data_prediction <-  read.csv('registration_status.csv', header = TRUE)

#combine
all_data <- merge(hh_and_indivdual, r_data, by = "person_id")
all_data_for_prediction <- merge(hh_and_indivdual, r_data_prediction, by = "person_id")

#Make sure no NA's in data set
summary(all_data)
str(all_data)

#Create Afican-American Variable
#Used for NA coding: http://rprogramming.net/recode-data-in-r/
all_data$black[all_data$race == "Black"] <- "1"
all_data$black[is.na(all_data$black)] <- "0"

#Run models
library(aod)
logit <- glm(is_registered_democrat ~  gender + black + age + is_college_graduate + Is.Urban.Household , data = all_data, family = binomial)
summary(logit)
logit2 <- glm(is_registered_democrat ~  gender + race + age + is_college_graduate + Is.Urban.Household + Tercile.of.Census.Tract, data = all_data, family = binomial)
summary(logit2)

#check significane 
1-pchisq(9952.1,8055)
1-pchisq(9588.8,8060)
library(ResourceSelection)
hoslem.test(all_data$is_registered_democrat, fitted(logit2))

#Make dataframe of predictions
scores <- data.frame(predict(logit2, all_data_for_prediction, type = 'response'))
scores$person_id <- 1:16092
score_done <- merge(scores, all_data_for_prediction, by = 'person_id')

#write all data
#write.csv(score_done, file = "Where you want the prediction scores to go")

