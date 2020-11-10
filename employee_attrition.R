###########################################################################################################################################################################
# Can we predict whether a member of staff is likely to leave based on 
# factors such as role, time in employment, salary and others? 
# ************************************************
# by The Ryman-Tubletts:
#     Lewis Playfoot
#     Varun Kale
#     Jack Cannings
#     Ibukunoluwa Odumade
#     Wisdom Njoku
#     Adrian Constantinescu
#  
# University of Surrey - COM3018
#
# 3 November 2020
###########################################################################################################################################################################


###########################################################################################################################################################################
# clears all objects in "global environment"
rm(list=ls())

###########################################################################################################################################################################
# Define and load libraries for project
MYLIBRARIES <-c("outliers",
                "corrplot",
                "MASS",
                "formattable",
                "stats",
                "PerformanceAnalytics",
                "caret")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

###########################################################################################################################################################################
# Global environment variables
TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric

###########################################################################################################################################################################
# load the dataset
employee_dataset <- read.csv("employee-attrition.csv")

print("Dataset is in a frame titled employee_dataset")

###########################################################################################################################################################################
# Defined global functions

# ***********************************************************
# oneHotEncoding() :
#   Pre-processing method to convert categorical fields into binary representation
# Input :
#   Fields to hot encode
#
# Output :
#   Encoded copy of input fields
# ***********************************************************
oneHotEncoding<-function(inputField1,inputField2){
  fieldsForEncoding = c(inputField1, inputField2)
  dmy <- dummyVars(" ~ .", data = fieldsForEncoding)
  trsf<-data.frame(predict(dmy, newdata=employee_dataset))
  encodedDataset <- cbind(employee_dataset,trsf)
  newData<-subset(encodedDataset, select = -c(Gender,OverTime))
  return(newData)
}


###########################################################################################################################################################################
# main() function
main<-function(){
  originalStatistics<-basicStatistics(employee_dataset)
  
  
  #newdf <- employee_dataset %>% mutate(across(c(Attrition, BusinessTravel, Department, EducationField, JobRole, MaritalStatus, Over18),
  #                                            ~as.numeric(as.factor(.))))
  
  
  ohe_fields<-oneHotEncoding(inputField1=employee_dataset['Gender'], inputField2=employee_dataset['OverTime'])
}


main()

#remove columns not need
# EmployeeCount is 1 for all
# EmployeeNumber is irrelevant 
# Standard Hours is 80 for all
# Over18 is Y for each value
# employee_dataset <- employee_dataset %>%
#   select(-c(EmployeeCount, EmployeeNumber, StandardHours, Over18))

#just another way of doing the statement above
#select(employee_dataset, -c(EmployeeCount, EmployeeNumber, StandardHours, Over18))

#NPREPROCESSING_initialFieldType(employee_dataset)

###########################################################################################################################################################################
# Converting all the columns to numeric

# Change each column to a a number 
# Attrition <- Yes - 2, No - 1
# BusinessTravels <- Travel_Rarely - 3, Travel_Frequently - 2, Non-Travel - 1
# Department <- Sales - 3, R & D - 2, Human Resources - 1
# EducationField <- Life Sciences - 2, Other - 5, Medical - 4, Marketing - 3, Technical Degree - 6, Human Resources - 1
#  OHE -> Gender
# Job Role <- Sales Ex - 8, Res Sc - 7, Lab - 3, Manufa - 5, Healthca - 1, Mana - 4, sales - 9, resea dir - 6, human res - 2
# Maritial Status <- Single - 3, Married - 2, Divorced -1
# OHE -> Overtime 
# Over18 < 1
          # newdf <- employee_dataset %>% mutate(across(c(Attrition, BusinessTravel, Department, EducationField, JobRole, MaritalStatus, Over18),
          #                                             ~as.numeric(as.factor(.))))
          # 
          # #One hot encoding done here. 
          # ohe_feats_train = c('Gender', 'OverTime')
          # dummies_train <- dummyVars(~  Gender + OverTime, data = newdf)
          # df_all_ohe_train <- as.data.frame(predict(dummies_train, newdata = newdf))
          # df_all_combined_train <- cbind(newdf[,-c(which(colnames(newdf) %in% ohe_feats_train))],df_all_ohe_train)
          # 
          # # Create Bins and complete dataset before normalisation
          # dataBeforeNormalisation <- df_all_combined_train %>% mutate(MonthlyIncome = case_when(MonthlyIncome <= 2500 ~ 1,
          #                                                                                       MonthlyIncome > 2500   & MonthlyIncome <= 5000 ~ 2,
          #                                                                                       MonthlyIncome > 5000   & MonthlyIncome <= 7500 ~ 3,
          #                                                                                       MonthlyIncome > 7500   & MonthlyIncome <= 10000 ~ 4,
          #                                                                                       MonthlyIncome > 10000   & MonthlyIncome <= 12500 ~ 5,
          #                                                                                       MonthlyIncome > 12500   & MonthlyIncome <= 15000 ~ 6,
          #                                                                                       MonthlyIncome > 15000   & MonthlyIncome <= 17500 ~ 7,
          #                                                                                       MonthlyIncome > 17500   & MonthlyIncome <= 20000 ~ 8)) # end function
          # 
          # if(all(sapply(dataBeforeNormalisation, is.numeric ))){
          #   print("All Fields Are Numeric")
          # }


###########################################################################################################################################################################
# Check for outliers

# fieldTypes <- NPREPROCESSING_initialFieldType(dataBeforeNormalisation)
# fieldTypes1 <- NPREPROCESSING_discreetNumeric(dataset = dataBeforeNormalisation, field_types = fieldTypes, cutoff = DISCREET_BINS)

# ordinals<-dataBeforeNormalisation[,which(fieldTypes1==TYPE_ORDINAL)]

              # for (i in colnames(dataBeforeNormalisation)){
              #   boxplots = boxplot(dataBeforeNormalisation)$i
              #   
              #   which(dataBeforeNormalisation %in% boxplots)
              #   print(paste("There are", length(which(dataBeforeNormalisation %in% boxplots)), "Outliers for the field", i))
              #   
              #   # cant figure out how to change to mean instead of removing
              #   dataBeforeNormalisation[ !(dataBeforeNormalisation %in% boxplots) ]
              #   
              # }
              # 
              # #make sure there are no NA's
              # any(is.na(dataBeforeNormalisation))

########################################################################################################################################################
# removing redundant fields
# from https://www.listendata.com/2015/06/simplest-dimensionality-reduction-with-r.html

            # # remove fields that have zero variance
            # toRemove <- nearZeroVar(dataBeforeNormalisation)
            # dataBeforeNormalisation <- dataBeforeNormalisation[, -toRemove]
            # 
            # # Calculate correlation matrix
            # corMatrix <- cor(dataBeforeNormalisation)
            # 
            # # find attributes that are highly corrected
            # highlyCorrelated <- findCorrelation(corMatrix, cutoff=0.9)
            # 
            # #names of highly correlated fields
            # highlyCorCol <- colnames(dataBeforeNormalisation)[highlyCorrelated]
            # 
            # # remove highly correlated fields
            # dataForNormalisation <- dataBeforeNormalisation[, -which(colnames(dataBeforeNormalisation) %in% highlyCorCol)]
            # dim(dataForNormalisation)
            # # tmp <- NPREPROCESSING_redundantFields(dataBeforeNormalisation, 0.9)

###########################################################################################################################################################################

            # # (Values - Minimum) / (Maximum - Minimum)
            # # Normalise Data - our own function 
            # normalise <- function(values) {
            #   return ((values - min(values)) / (max(values) - min(values)))
            # }
            # normalisedDataset <- as.data.frame(lapply(dataBeforeNormalisation,normalise))

###########################################################################################################################################################################
# pairs(normalisedDataset[,c("EnvironmentSatisfaction","DailyRate","Attrition")])
# pairs(normalisedDataset)


#Denormalise Dataset - This is not needed yet, just for reference
# minvec <- sapply(dataBeforeNormalisation,min)
# maxvec <- sapply(dataBeforeNormalisation,max)
# denormalize <- function(x,minval,maxval) {
#   x*(maxval-minval) + minval
# }
# denormalzedDataset <- as.data.frame(Map(denormalize,ddnorm,minvec,maxvec))