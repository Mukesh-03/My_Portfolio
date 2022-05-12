# Importing Dataset
library(readxl)
hospdata = read_excel("hospitalcosts.xlsx",sheet="HospitalCosts")
View(hospdata)
summary(hospdata)

#To find the age category of people who frequent the hospital 
attach(hospdata)
hist(AGE)
#To see the value for age group 0-1
table(AGE)

#To find the age category of people who has maximum expenditure 

tapply(TOTCHG,AGE,sum)
which.max(tapply(TOTCHG,AGE,sum))

#2.In order of severity of the diagnosis and treatments and to find out the 
#expensive treatments, the agency wants to find the diagnosis related group 
#that has maximum hospitalization and expenditure.

summary(as.factor(APRDRG))


#to get the diagnostic realted cost

tapply(TOTCHG,as.factor(APRDRG),sum)

# To get maximum cost

which.max(tapply(TOTCHG,as.factor(APRDRG),sum))
max(tapply(TOTCHG,as.factor(APRDRG),sum))

#3. To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.

#h0: the race of patient is related to the hospitalization cost
#ha: No relation between race and cost

summary(as.factor(RACE))

#Omitting the NA values

hspdt=na.omit(hospdata)
summary(as.factor(hspdt$RACE))

#Applying ANNOVA

anv<- aov(TOTCHG~RACE,data=hspdt)
summary(anv) 

#To properly utilize the costs, the agency has to analyze the severity of the 
#hospital costs by age and gender for proper allocation of resources.

summary(as.factor(hspdt$FEMALE))

#almost equal distribution of male and female

#Applying regression modeling

md1<- lm(formula = TOTCHG~AGE+FEMALE, data = hspdt)
summary(md1)

# Since the length of stay is the crucial factor for inpatients, the agency wants 
#to find if the length of stay can be predicted from age, gender, and race.

md2<-lm(formula=LOS~AGE+FEMALE+RACE, data = hspdt)
summary(md2)


#6.	To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs.

md3<- lm(formula=TOTCHG~ ., data = hspdt)
summary(md3)
