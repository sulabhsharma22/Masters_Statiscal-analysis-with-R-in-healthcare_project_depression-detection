getwd()
setwd("C:\\Users\\dharm\\Desktop\\Fall 20 SEM3\\IS 777\\Project")
mydata <- read.csv("b_depressed.csv",header=T)
head(mydata)
tail(mydata)


install.packages('knitr')
library(knitr)
install.packages("tidyverse")
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages("mice")
install.packages('mice')
library(mice)
install.packages('lattice')
library(lattice)
install.packages('reshape2')
library(reshape2)
install.packages("DataExplorer") 
library(DataExplorer)

nrow(mydata) #No of Rows
ncol(mydata) #No of Columns
head(mydata$depressed)
str(mydata) # Variable Types of Col
colnames(mydata) # Column name in dataset



#dropping cols
mydata$Survey_id <- NULL #Survey id columns as it is not essential and do not contribute to the output
mydata$Ville_id <- NULL #Removing id columns as it is not essential and do not contribute to the output
head(mydata)

#checking for missing values and null values and na values
introduce(mydata)

#handling na with mean
#finding mean of no_lasting_investmen
mydata[is.na(mydata)] = 0
is.null(mydata$no_lasting_investmen)
colnames(mydata)[colSums(is.na(mydata)) > 0]
lapply(mydata,function(x) { length(which(is.na(x)))})

mean_nli <- mean(mydata$no_lasting_investmen)
mean_nli

#replacing na with mean
for (i in 1:nrow(mydata)) {
  if(mydata$no_lasting_investmen[i] == 0){
    mydata$no_lasting_investmen[i] <- mean_nli
  }
}

# Checking if null values are removed and replaced
introduce(mydata)

mydata$no_lasting_investmen # checking if the NA values are replaced by column mean values

cor(mydata)

#descriptive analysis
summary(mydata)


# Checking correlation among data
plot_correlation(na.omit(mydata), maxcat = 5L)

#Histogram for Data
plot_histogram(mydata)



#Correlation of the variables
#Extract all the numeric columns from mydata
newdata <- mydata[, sapply(mydata, class) != "factor"]
head(newdata)

#Using correlation cor()
cor(newdata)
cor(non_fact_data)

#heat map
rel <- cor(newdata)
rel <- as.matrix(rel)
plot(rel)
pairs(rel)


str(mydata)
#converting qualitative variables into factor variables (correction for deliverables 2)
non_fact_data <- mydata
#converting the binary variables into factors
names(mydata)
mydata$sex <- as.factor(mydata$sex)
mydata$Married <- as.factor(mydata$Married)
mydata$incoming_salary <- as.factor(mydata$incoming_salary)
mydata$incoming_own_farm <- as.factor(mydata$incoming_own_farm)
mydata$incoming_business <- as.factor(mydata$incoming_business)
mydata$incoming_no_business <- as.factor(mydata$incoming_no_business)
mydata$labor_primary <- as.factor(mydata$labor_primary)

#converting the target variable into factor variable.
mydata$depressed <- as.factor(mydata$depressed)
str(mydata)
