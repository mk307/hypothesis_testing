#Loading Libraries
install.packages("dplyr")
library(dplyr)
library("plyr")
library("tidyr")
library("readxl")
library("knitr")

#DEALING WITH MISSING VALUES

#Reading data 
data1 <- read.csv("CityPayrollDataset.csv")
head(data1)

#Using na.strings
data2 <- read.csv("CityPayrollDataset.csv", header=T, na.strings=c(""," ","NA"))
head(data2)

#Counting NA in each column to confirm if all missing values have been replaced with NA
colSums(is.na(data2))

#Replacing NA with Median in Hourly.or.Event.Rate, which turned out to be $37.09
data2 %>% 
  summarise(median_event_rate = median(Hourly.or.Event.Rate, na.rm = TRUE))
data3 <- data2 %>%
  replace_na(list(Hourly.or.Event.Rate = "$37.09" ))
  
#Replacing NA with Mode in Lump.Sum.Pay, which turned out to be $0.00
data3 <- data3 %>%
  replace_na(list(Lump.Sum.Pay = "$0.00" ))
  
#Replacing NA with Mode in Overtime.Pay, which turned out to be $0.00
data3 <- data3 %>%
  replace_na(list(Overtime.Pay = "$0.00" ))
  
#Converting type of numerical values from character to numeric  
	#Hourly.or.Event.Rate
data3$Hourly.or.Event.Rate = as.factor(gsub(",", "", data3$Hourly.or.Event.Rate))
data3$Hourly.or.Event.Rate = as.numeric(gsub("\\$", "", data3$Hourly.or.Event.Rate))
	#Projected.Annual.Salary
data3$Projected.Annual.Salary = as.factor(gsub(",", "", data3$Projected.Annual.Salary))
data3$Projected.Annual.Salary = as.numeric(gsub("\\$", "", data3$Projected.Annual.Salary))
	#Q1.Payments
data3$Q1.Payments = as.factor(gsub(",", "", data3$Q1.Payments))
data3$Q1.Payments = as.numeric(gsub("\\$", "", data3$Q1.Payments))	
	#Q2.Payments
data3$Q2.Payments = as.factor(gsub(",", "", data3$Q2.Payments))
data3$Q2.Payments = as.numeric(gsub("\\$", "", data3$Q2.Payments))	
	#Q3.Payments
data3$Q3.Payments = as.factor(gsub(",", "", data3$Q3.Payments))
data3$Q3.Payments = as.numeric(gsub("\\$", "", data3$Q3.Payments))	
	#Q4.Payments
data3$Q4.Payments = as.factor(gsub(",", "", data3$Q4.Payments))
data3$Q4.Payments = as.numeric(gsub("\\$", "", data3$Q4.Payments))	
	#Payments.Over.Base.Pay
data3$Payments.Over.Base.Pay = as.factor(gsub(",", "", data3$Payments.Over.Base.Pay))
data3$Payments.Over.Base.Pay = as.numeric(gsub("\\$", "", data3$Payments.Over.Base.Pay))	
	#X..Over.Base.Pay
data3$X..Over.Base.Pay = as.factor(gsub(",", "", data3$X..Over.Base.Pay))
data3$X..Over.Base.Pay = as.numeric(gsub("\\%", "", data3$X..Over.Base.Pay))	
	#Total.Payments
data3$Total.Payments = as.factor(gsub(",", "", data3$Total.Payments))
data3$Total.Payments = as.numeric(gsub("\\$", "", data3$Total.Payments))	
	#Base.Pay
data3$Base.Pay = as.factor(gsub(",", "", data3$Base.Pay))
data3$Base.Pay = as.numeric(gsub("\\$", "", data3$Base.Pay))	
	#Permanent.Bonus.Pay
data3$Permanent.Bonus.Pay = as.factor(gsub(",", "", data3$Permanent.Bonus.Pay))
data3$Permanent.Bonus.Pay = as.numeric(gsub("\\$", "", data3$Permanent.Bonus.Pay))	
	#Longevity.Bonus.Pay
data3$Longevity.Bonus.Pay = as.factor(gsub(",", "", data3$Longevity.Bonus.Pay))
data3$Longevity.Bonus.Pay = as.numeric(gsub("\\$", "", data3$Longevity.Bonus.Pay))	
	#Temporary.Bonus.Pay
data3$Temporary.Bonus.Pay = as.factor(gsub(",", "", data3$Temporary.Bonus.Pay))
data3$Temporary.Bonus.Pay = as.numeric(gsub("\\$", "", data3$Temporary.Bonus.Pay))	
	#Lump.Sum.Pay
data3$Lump.Sum.Pay = as.factor(gsub(",", "", data3$Lump.Sum.Pay))
data3$Lump.Sum.Pay = as.numeric(gsub("\\$", "", data3$Lump.Sum.Pay))	
	#Overtime.Pay
data3$Overtime.Pay = as.factor(gsub(",", "", data3$Overtime.Pay))
data3$Overtime.Pay = as.numeric(gsub("\\$", "", data3$Overtime.Pay))	
	#Other.Pay...Adjustments
data3$Other.Pay...Adjustments = as.factor(gsub(",", "", data3$Other.Pay...Adjustments))
data3$Other.Pay...Adjustments = as.numeric(gsub("\\$", "", data3$Other.Pay...Adjustments))	
	#Other.Pay..Payroll.Explorer.
data3$Other.Pay..Payroll.Explorer. = as.factor(gsub(",", "", data3$Other.Pay..Payroll.Explorer.))
data3$Other.Pay..Payroll.Explorer. = as.numeric(gsub("\\$", "", data3$Other.Pay..Payroll.Explorer.))	
	#Average.Health.Cost
data3$Average.Health.Cost = as.factor(gsub(",", "", data3$Average.Health.Cost))
data3$Average.Health.Cost = as.numeric(gsub("\\$", "", data3$Average.Health.Cost))	
	#Average.Dental.Cost
data3$Average.Dental.Cost = as.factor(gsub(",", "", data3$Average.Dental.Cost))
data3$Average.Dental.Cost = as.numeric(gsub("\\$", "", data3$Average.Dental.Cost))	
	#Average.Basic.Life
data3$Average.Basic.Life = as.factor(gsub(",", "", data3$Average.Basic.Life))
data3$Average.Basic.Life = as.numeric(gsub("\\$", "", data3$Average.Basic.Life))	
	#Average.Benefit.Cost
data3$Average.Benefit.Cost = as.factor(gsub(",", "", data3$Average.Benefit.Cost))
data3$Average.Benefit.Cost = as.numeric(gsub("\\$", "", data3$Average.Benefit.Cost))	

#DATA DISTRIBUTION
hist(data3$Hourly.or.Event.Rate, main="Hourly.or.Event.Rate Distribution ", col="Blue",prob=TRUE, xlab="Hourly.or.Event.Rate")
lines(density(data3$Hourly.or.Event.Rate), col = "red")
hist(data3$Hourly.or.Event.Rate, main="Hourly.or.Event.Rate Distribution ", col="Blue",prob=TRUE, xlab="Hourly.or.Event.Rate")
lines(density(data3$Hourly.or.Event.Rate), col = "red")

hist(data3$Temporary.Bonus.Pay, main="Temporary.Bonus.Pay Distribution ", col="Blue",prob=TRUE, xlab="Temporary.Bonus.Pay")
lines(density(data3$Temporary.Bonus.Pay), col = "red")
hist(data3$Temporary.Bonus.Pay, main="Temporary.Bonus.Pay Distribution ", col="Blue",prob=TRUE, xlab="Temporary.Bonus.Pay")
lines(density(data3$Temporary.Bonus.Pay), col = "red")

hist(data3$Permanent.Bonus.Pay, main="Permanent.Bonus.Pay Distribution ", col="Blue",prob=TRUE, xlab="Permanent.Bonus.Pay")
lines(density(data3$Permanent.Bonus.Pay), col = "red")
hist(data3$Permanent.Bonus.Pay, main="Permanent.Bonus.Pay Distribution ", col="Blue",prob=TRUE, xlab="Permanent.Bonus.Pay")
lines(density(data3$Permanent.Bonus.Pay), col = "red")

hist(data3$Base.Pay, main="Base.Pay Distribution ", col="Blue",prob=TRUE, xlab="Base.Pay")
lines(density(data3$Base.Pay), col = "red")
hist(data3$Base.Pay, main="Base.Pay Distribution ", col="Blue",prob=TRUE, xlab="Base.Pay")
lines(density(data3$Base.Pay), col = "red")

hist(data3$Overtime.Pay, main="Overtime.Pay Distribution ", col="Blue",prob=TRUE, xlab="Overtime.Pay")
lines(density(data3$Overtime.Pay), col = "red")
hist(data3$Overtime.Pay, main="Overtime.Pay Distribution ", col="Blue",prob=TRUE, xlab="Overtime.Pay")
lines(density(data3$Overtime.Pay), col = "red")

hist(data3$X..Over.Base.Pay, main="Over.Base.Pay Distribution", col="Blue",prob=TRUE, xlab="Over.Base.Pay")
lines(density(data3$X..Over.Base.Pay), col = "red")
hist(data3$X..Over.Base.Pay, main="Over.Base.Pay Distribution", col="Blue",prob=TRUE, xlab="Over.Base.Pay")
lines(density(data3$X..Over.Base.Pay), col = "red")

hist(data3$Average.Benefit.Cost, main="Average.Benefit.Cost Distribution", col="Blue",prob=TRUE, xlab="Average.Benefit.Cost")
lines(density(data3$Average.Benefit.Cost), col = "red")
hist(data3$Average.Benefit.Cost, main="Average.Benefit.Cost Distribution", col="Blue",prob=TRUE, xlab="Average.Benefit.Cost")
lines(density(data3$Average.Benefit.Cost), col = "red")

hist(data3$Average.Health.Cost, main="Average.Health.Cost Distribution", col="Blue",prob=TRUE, xlab="Average.Health.Cost")
lines(density(data3$Average.Health.Cost), col = "red")
hist(data3$Average.Health.Cost, main="Average.Health.Cost Distribution", col="Blue",prob=TRUE, xlab="Average.Health.Cost")
lines(density(data3$Average.Health.Cost), col = "red")

hist(data3$Longevity.Bonus.Pay, main="Longevity.Bonus.Pay Distribution", col="Blue",prob=TRUE, xlab="Longevity.Bonus.Pay")
lines(density(data3$Longevity.Bonus.Pay), col = "red")
hist(data3$Longevity.Bonus.Pay, main="Longevity.Bonus.Pay Distribution", col="Blue",prob=TRUE, xlab="Longevity.Bonus.Pay")
lines(density(data3$Longevity.Bonus.Pay), col = "red")

'''
QUESTION 1:
Do the employees working as Police Officer-II have a better chance of getting Temporary Bonus Pay?
'''
Q1_Subset <- data3[which(data3$Job.Class.Title == "Police Officer II"), names(data3) %in% c("Job.Class.Title", "Hourly.or.Event.Rate", "Temporary.Bonus.Pay")]
Q1_Subset
xbar1912a=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q1_Subset$Hourly.or.Event.Rate, size=1912)
  xbar1912a[i]=mean(x)}
Q1_Hourly.or.Event.Rate <- rnorm(1912, mean(xbar1912a), sd(xbar1912a))
qqnorm(Q1_Hourly.or.Event.Rate)
qqline(Q1_Hourly.or.Event.Rate, col = "red")
xbar1912b=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q1_Subset$Temporary.Bonus.Pay, size=1912)
  xbar1912b[i]=mean(x)}
Q1_Temporary.Bonus.Pay <- rnorm(1912, mean(xbar1912b), sd(xbar1912b))
qqnorm(Q1_Temporary.Bonus.Pay)
qqline(Q1_Temporary.Bonus.Pay, col = "red")
cor(xbar1912a, xbar1912b)
cor.test(xbar1912a, xbar1912b)

'''
QUESTION 2:
Employees who get Permanent Bonus Pay are most likely to be from Public Works-Sanitation Department?
'''
Q2_Subset <- data3[which(data3$Department.Title == "Public Works - Sanitation"), names(data3) %in% c("Department.Title", "Base.Pay", "Permanent.Bonus.Pay")]
Q2_Subset
xbar1327a=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q2_Subset$Base.Pay, size=1327)
  xbar1327a[i]=mean(x)}
Q2_Base.Pay <- rnorm(1327, mean(xbar1327a), sd(xbar1327a))
qqnorm(Q2_Base.Pay)
qqline(Q2_Base.Pay, col = "red")
xbar1327b=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q2_Subset$Permanent.Bonus.Pay, size=1327)
  xbar1327b[i]=mean(x)}
Q2_Permanent.Bonus.Pay <- rnorm(1327, mean(xbar1327b), sd(xbar1327b))
qqnorm(Q2_Permanent.Bonus.Pay)
qqline(Q2_Permanent.Bonus.Pay, col = "red")
cor(xbar1327a, xbar1327b)
cor.test(xbar1327a, xbar1327b)

'''
QUESTION 3:
Do the employees working in Water and Power (DWP) Department have a better chance of being employed overtime?
'''
Q3_Subset <- data3[which(data3$Department.Title == "Water And Power (DWP)"), names(data3) %in% c("Department.Title", "Base.Pay", "Overtime.Pay")]
Q3_Subset
xbar4819a=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q3_Subset$Base.Pay, size=4819)
  xbar4819a[i]=mean(x)}
Q3_Base.Pay <- rnorm(4819, mean(xbar4819a), sd(xbar4819a))
qqnorm(Q3_Base.Pay)
qqline(Q3_Base.Pay, col = "red")
xbar4819b=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q3_Subset$Overtime.Pay, size=4819)
  xbar4819b[i]=mean(x)}
Q3_Overtime.Pay <- rnorm(4819, mean(xbar4819b), sd(xbar4819b))
qqnorm(Q3_Overtime.Pay)
qqline(Q3_Overtime.Pay, col = "red")
cor(xbar4819a, xbar4819b)
cor.test(xbar4819a, xbar4819b)

'''
QUESTION 4:
Do the employees who work Part Time instead of Full Time have a better chance to be from the Airports (LAWA) Department?
'''
Q4a_Subset <- data3[which(data3$Department.Title == "Airports (LAWA)"), names(data3) %in% c("Department.Title", "Employment.Type", "Base.Pay", "Hourly.or.Event.Rate")]
Q4a_Subset
Q4b_Subset <- Q4a_Subset[which(Q4a_Subset$Employment.Type == "Part Time"), names(Q4a_Subset) %in% c("Department.Title", "Employment.Type", "Base.Pay", "Hourly.or.Event.Rate")]
Q4b_Subset
Q4c_Subset <- Q4a_Subset[which(Q4a_Subset$Employment.Type == "Full Time"), names(Q4a_Subset) %in% c("Department.Title", "Employment.Type", "Base.Pay", "Hourly.or.Event.Rate")]
Q4c_Subset
xbar100a=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q4b_Subset$Base.Pay, size=100)
  xbar100a[i]=mean(x)}
Q4b_Base.Pay <- rnorm(100, mean(xbar100a), sd(xbar100a))
qqnorm(Q4b_Base.Pay)
qqline(Q4b_Base.Pay, col = "red")
xbar100b=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q4b_Subset$Hourly.or.Event.Rate, size=100)
  xbar100b[i]=mean(x)}
Q4b_Hourly.or.Event.Rate <- rnorm(100, mean(xbar100b), sd(xbar100b))
qqnorm(Q4b_Hourly.or.Event.Rate)
qqline(Q4b_Hourly.or.Event.Rate, col = "red")
xbar1525a=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q4c_Subset$Base.Pay, size=1525)
  xbar1525a[i]=mean(x)}
Q4c_Base.Pay <- rnorm(100, mean(xbar1525a), sd(xbar1525a))
qqnorm(Q4c_Base.Pay)
qqline(Q4c_Base.Pay, col = "red")
xbar1525b=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q4c_Subset$Hourly.or.Event.Rate, size=1525)
  xbar1525b[i]=mean(x)}
Q4c_Hourly.or.Event.Rate <- rnorm(100, mean(xbar1525b), sd(xbar1525b))
qqnorm(Q4c_Hourly.or.Event.Rate)
qqline(Q4c_Hourly.or.Event.Rate, col = "red")
cor(xbar100a, xbar100b)
cor.test(xbar100a, xbar100b)
cor(xbar1525a, xbar1525b)
cor.test(xbar1525a, xbar1525b)

'''
QUESTION 5:
After 2013, Police (LAPD) Department has experienced the highest pay raise as compared to other departments?
'''
Q5a_Subset <- data3[which(data3$Department.Title == "Police (LAPD)"), names(data3) %in% c("Year", "Department.Title", "Base.Pay", "X..Over.Base.Pay")]
Q5a_Subset
Q5b_Subset <- Q5a_Subset[which(Q5a_Subset$Year != "2013"), names(Q5a_Subset) %in% c("Year", "Department.Title", "Base.Pay", "X..Over.Base.Pay")]
Q5b_Subset
xbar4736a=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q5b_Subset$Base.Pay, size=4736)
  xbar4736a[i]=mean(x)}
Q5b_Base.Pay <- rnorm(4736, mean(xbar4736a), sd(xbar4736a))
qqnorm(Q5b_Base.Pay)
qqline(Q5b_Base.Pay, col = "red")
xbar4736b=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q5b_Subset$X..Over.Base.Pay, size=4736)
  xbar4736b[i]=mean(x)}
Q5b_X..Over.Base.Pay <- rnorm(4736, mean(xbar4736b), sd(xbar4736b))
qqnorm(Q5b_X..Over.Base.Pay)
qqline(Q5b_X..Over.Base.Pay, col = "red")
cor(xbar4736a, xbar4736b)
cor.test(xbar4736a, xbar4736b)

'''
QUESTION 6 - Is there any relationship between Department and Employment type?
'''
chisq.test(data3$Department.Title, data3$Employment.Type)

'''
#QUESTION 7
Potential future employees heard a rumor that Average Base Pay of employees in General Services Department is more than $20,746,
but they are not sure about this. Confirm this rumor based on the given dataset.
'''
Q7_Subset <- data3[which(data3$Department.Title == "General Services"), names(data3) %in% c("Department.Title", "Base.Pay")]
Q7_Subset
xbar689=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q7_Subset$Base.Pay, size=689)
  xbar689[i]=mean(x)}
Q7_Base.Pay <- rnorm(689, mean(xbar689), sd(xbar689))
qqnorm(Q7_Base.Pay)
qqline(Q7_Base.Pay, col = "red")
t.test(xbar689, mu = 20746, alternative = 'greater')

'''
QUESTION 8 
It has been established that Water and Power (DWP) Department pays on average more Benefit Cost to its employees than all other departments. 
Confirm this hypothesis with respect to the data given to you.
'''
Q8a_Subset <- data3[which(data3$Department.Title == "Water And Power (DWP)"), names(data3) %in% c("Department.Title", "Average.Benefit.Cost")]
Q8a_Subset
xbar4819=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q8a_Subset$Average.Benefit.Cost, size=4819)
  xbar4819[i]=mean(x)}
Q8a_Average.Benefit.Cost <- rnorm(4819, mean(xbar4819), sd(xbar4819))
qqnorm(Q8a_Average.Benefit.Cost)
qqline(Q8a_Average.Benefit.Cost, col = "red")
Q8b_Subset <- data3[which(data3$Department.Title != "Water And Power (DWP)"), names(data3) %in% c("Department.Title", "Average.Benefit.Cost")]
Q8b_Subset 
xbar20832=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q8b_Subset$Average.Benefit.Cost, size=20832)
  xbar20832[i]=mean(x)}
Q8b_Average.Benefit.Cost <- rnorm(20832, mean(xbar20832), sd(xbar20832))
qqnorm(Q8b_Average.Benefit.Cost)
qqline(Q8b_Average.Benefit.Cost, col = "red")
t.test(xbar20832, mu = mean(xbar4819), alternative = 'less')

'''
QUESTION 9:
In 2014, employees of Recreation and Parks Department were complaining that they have been denied the Longevity Bonus Pay. 
Confirm their complaint from the given dataset.
''' 
Q9a_Subset <- data3[which(data3$Department.Title == "Recreation And Parks"), names(data3) %in% c("Year", "Department.Title", "Longevity.Bonus.Pay")]
Q9a_Subset
Q9b_Subset <- Q9a_Subset[which(Q9a_Subset$Year == "2014"), names(Q9a_Subset) %in% c("Year", "Department.Title", "Longevity.Bonus.Pay")]
Q9b_Subset
xbar641=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q9b_Subset$Longevity.Bonus.Pay, size=641)
  xbar641[i]=mean(x)}
Q9_Longevity.Bonus.Pay <- rnorm(641, mean(xbar641), sd(xbar641))
qqnorm(Q9_Longevity.Bonus.Pay)
qqline(Q9_Longevity.Bonus.Pay, col = "red")
t.test(xbar641, mu = 0, alternative = 'greater')

'''
QUESTION 10:
Senior Clerk Typist from Harbor (Port of LA) Department has been telling Senior Clerk Typist of Water and Power (DWP) Department that they have more Average Health Cost than them. 
Accept or Reject this claim according to the dataset given. 
'''
Q10a_Subset <- data3[which(data3$Job.Class.Title == "Senior Clerk Typist"), names(data3) %in% c("Job.Class.Title", "Department.Title", "Average.Health.Cost")]
Q10a_Subset
Q10b_Subset <- Q10a_Subset[which(Q10a_Subset$Department.Title == "Harbor (Port of LA)"), names(Q10a_Subset) %in% c("Job.Class.Title", "Department.Title", "Average.Health.Cost")]
Q10b_Subset
xbar9=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q10b_Subset$Average.Health.Cost, size=9)
  xbar9[i]=mean(x)}
Q10b_Average.Health.Cost <- rnorm(9, mean(xbar9), sd(xbar9))
qqnorm(Q10b_Average.Health.Cost)
qqline(Q10b_Average.Health.Cost, col = "red")
Q10c_Subset <- Q10a_Subset[which(Q10a_Subset$Department.Title == "Water And Power (DWP)"), names(Q10a_Subset) %in% c("Job.Class.Title", "Department.Title", "Average.Health.Cost")]
Q10c_Subset
xbar114=rep(NA, 1000)
for (i in 1:1000)
  {x=sample(Q10c_Subset$Average.Health.Cost, size=114)
  xbar114[i]=mean(x)}
Q10c_Average.Health.Cost <- rnorm(114, mean(xbar114), sd(xbar114))
qqnorm(Q10c_Average.Health.Cost)
qqline(Q10c_Average.Health.Cost, col = "red")
t.test(xbar9, mu = mean(xbar114), alternative = 'less')
