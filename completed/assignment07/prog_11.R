# Assignment: Exercise 9
# Name: Patkhedkar, Ninad
# Date: 2020-10-01

## Set the working directory to the root of your DSC 520 directory
setwd("/cloud/project")

## Load the `data/r4ds/heights.csv` to
library(ggplot2)
library(GGally)
library(readxl)
library(dplyr)
library(lm.beta)
library(knitr)

setwd("/cloud/project")
housing_df <-read_excel("data/week-6-housing.xlsx")
head(housing_df)
View(hosing_df)
clean_housing_df = subset(housing_df, select = -c(1,sale_reason,sale_instrument,sale_warning,zip5,ctyname,sitetype,addr_full,postalctyn,lon,lat,building_grade,year_built,year_renovated,current_zoning,prop_type,present_use) )
clean_housing_df <- rename(clean_housing_df,sale_price = "Sale Price")
head(clean_housing_df)
ggpairs(data=clean_housing_df,columns =c(1,2,3,7) , title="Housing Data")

clean_housing_df$total_bathroom <-clean_housing_df$bath_full_count + clean_housing_df$bath_half_count + clean_housing_df$bath_3qtr_count
head(clean_housing_df)
clean_housing_df <- subset(clean_housing_df,select = -c(bath_full_count,bath_half_count,bath_3qtr_count))

head(clean_housing_df)
ggpairs(data=clean_housing_df, title="Housing Data")
clean_housing_df <- rename(clean_housing_df,sale_price = "Sale Price")
housing_lm <- lm(sale_price ~ square_feet_total_living + bedrooms + sq_ft_lot,data=clean_housing_df)
summary(housing_lm)
housing_lm <- lm(sale_price ~ square_feet_total_living + bedrooms + sq_ft_lot + total_bathroom ,data=clean_housing_df)
summary(housing_lm)
lm.beta(housing_lm)

confint(housing_lm, level = 0.90)

summary(beta_coef)
head(student_survey_df)
nrow(student_survey_df)

student_survey_df <- student_survey_df$TimeReading * 60
head(student_survey_df)

gender0_df <-student_survey_df[ which( student_survey_df$Gender == "0"), ]
cov(gender0_df)
nrow(gender0_df)
paste('Covariance for Gender1')
gender1_df <-student_survey_df[ which( student_survey_df$Gender == "1"), ]
cov(gender1_df)
nrow(gender1_df)
cor(student_survey_df)
cov(student_survey_df$TimeReading*60, student_survey_df$TimeTV)