library(dplyr)
library("outliers")
library("corrplot")
library("MASS")
library("formattable")
library("stats")
library("PerformanceAnalytics")
library(tidyverse)
library(caret)
library(e1071)

employee_dataset <- read.csv("employee-attrition.csv")

#remove columns not need
# EmployeeCount is 1 for all
# EmployeeNumber is irrelevant 
# Standard Hours is 80 for all
# Over18 is Y for each value
employee_dataset <- employee_dataset %>%
  select(-(c(EmployeeCount, EmployeeNumber, StandardHours, Over18)))

NPREPROCESSING_initialFieldType(employee_dataset)

#Code 2
newdf2 <- employee_dataset %>% mutate(across(everything(),~as.factor(.))) %>%
  mutate(across(c(Attrition, BusinessTravel, Department, EducationField, Gender, JobRole, MaritalStatus, OverTime),.fns = list(value = ~ as.numeric(.))))

# Attrition <- Yes - 2, No - 1
# BusinessTravels <- Travel_Rarely - 3, Travel_Frequently - 2, Non-Travel - 1
# Department <- Sales - 3, R & D - 2, Human Resources - 1
# EducationField <- Life Sciences - 2, Other - 5, Medical - 4, Marketing - 3, Technical Degree - 6, Human Resources - 1
#  OHE -> Gender <- Female - 1, Male - 2
# Job Role <- Sales Ex - 8, Res Sc - 7, Lab - 3, Manufa - 5, Healthca - 1, Mana - 4, sales - 9, resea dir - 6, human res - 2
# Maritial Status <- Single - 3, Married - 2, Divorced -1
# OHE -> Overtime <- Yes - 2, No - 1


newdf3 <- newdf2 %>% 
  select(-c(Attrition, BusinessTravel, Department, EducationField, JobRole, MaritalStatus)) %>%
  rename("Attrition" = Attrition_value, 
         "BusinessTravel" = BusinessTravel_value,
         "Department" = Department_value,
         "EducationField" = EducationField_value,
         "JobRole" = JobRole_value,
         "MaritialStatus" = MaritalStatus_value)


# all in one move
library(dplyr)
#Code
newdf <- employee_dataset %>% mutate(across(c(Attrition, BusinessTravel, Department, EducationField, JobRole, MaritalStatus),
                                            ~as.numeric(as.factor(.))))

#One hot encoding done here. 
ohe_feats_train = c('Gender', 'OverTime')
dummies_train <- dummyVars(~  Gender + OverTime, data = newdf)
df_all_ohe_train <- as.data.frame(predict(dummies_train, newdata = newdf))
df_all_combined_train <- cbind(newdf[,-c(which(colnames(newdf) %in% ohe_feats_train))],df_all_ohe_train)

# Create Bins 
data_1 <- df_all_combined_train %>% mutate(MonthlyIncome = case_when(MonthlyIncome <= 2500 ~ '1',
                                                                   MonthlyIncome > 2500   & MonthlyIncome <= 5000 ~ '2',
                                                                   MonthlyIncome > 5000   & MonthlyIncome <= 7500 ~ '3',
                                                                   MonthlyIncome > 7500   & MonthlyIncome <= 10000 ~ '4',
                                                                   MonthlyIncome > 10000   & MonthlyIncome <= 12500 ~ '5',
                                                                   MonthlyIncome > 12500   & MonthlyIncome <= 15000 ~ '6',
                                                                   MonthlyIncome > 15000   & MonthlyIncome <= 17500 ~ '7',
                                                                   MonthlyIncome > 17500   & MonthlyIncome <= 20000 ~ '8')) # end function
