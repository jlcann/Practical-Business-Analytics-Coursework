rm(list=ls())
# Global Environment variables
# - i.e. available to all functions
# Good practice to place "constants" in named variables
# I use UPPERCASE to identify these in my code

DATASET_FILENAME  <- "employee-attrition.csv"          # Name of input dataset file
OUTPUT_FIELD      <- "Attrition"             # Field name of the output class to predict

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded
DISCREET_BINS     <- 5                    # Number of Discreet Bins Required for 
OUTLIER_CONFIDENCE <- 0.99                # Confidence of discreet 
CUTOFF            <- 0.90                 # Correlation cutoff
FREQCUT           <- 99/1                 # To remove zero variance fields




# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4
# Carat         
MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "PerformanceAnalytics",
               "caret",
               "dplyr")

# clears the console area
cat("\014")

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("employee_attrition_functions.R")

set.seed(123)

# originalDataSet <- readDataset(DATASET_FILENAME)
originalDataset <- read.csv("employee-attrition.csv")

# ****************
# oneHotEncoding() :
#   Pre-processing method to convert appropriate 
#   categorical fields into binary representation
#
# INPUT       :   dataframe - dataset           - dataset to one hot encode
#                 vector    - fieldsForEncoding -  
#
# OUTPUT      :   Encoded fields
# ****************
oneHotEncoding<-function(dataset,fieldsForEncoding){
  # Combine input fields for encoding
  stringToFormulate <- substring(paste(" + ", fieldsForEncoding, sep = "", collapse = ""), 4)
  
  OHEFormula <- as.formula(paste("~",stringToFormulate))
  
  # One hot encode fields listed in function
  dmy <- dummyVars(OHEFormula, data = dataset)
  trsf<- data.frame(predict(dmy, newdata = originalDataset[,which(field_types==TYPE_SYMBOLIC)]))
  
  # Combine the encoded fields back to the originalDataset
  encodedDataset <- cbind(originalDataset[,which(field_types==TYPE_SYMBOLIC)],trsf)
  
  # Remove original fields that have been hot encoded
  newData<- encodedDataset %>% select(-c(fieldsForEncoding))
  # Return new dataset
  return(newData)
}


# ************************************************
# normalise() :
#   Normalise fields between 1 and 0
#
# INPUT       :   Fields to normalise
#
# OUTPUT      :   Normalised fields between 1 and 0
# ************************************************
normalise <- function(values) {
  return ((values - min(values)) / (max(values) - min(values)))
}


# ************************************************
# preprocessing() :
# data preprocessing function
#
# INPUT       :   dataframe - originalDataset - the original dataset 
#
# OUTPUT      :   dataframe - normalisedDataset - dataset set to be used for the ML models
# ************************************************
preprocessing <- function(originalDataset){
  
  print(DATASET_FILENAME)
  
  #Print statistics of originalDataSet into the viewer.
  basicStatistics(originalDataset)
  
  # Check for NA fields in original dataset
  print("Checking to see for any missing data:")
  print(sapply(originalDataset,function(x) sum(is.na(x))))
  
  # Determine if fields are SYMBOLIC or NUMERIC (global)
  field_types<<-FieldTypes(originalDataset)
  
  # Determine if NUMERIC fields are DISCREET or ORDINAL
  field_Types_Discreet_Ordinal<- NPREPROCESSING_discreetNumeric(originalDataset,field_types,DISCREET_BINS)
  discreet_fields <- names(originalDataset)[field_Types_Discreet_Ordinal=="DISCREET"]
  
  results<-data.frame(field=names(originalDataset),initial=field_types,types1=field_Types_Discreet_Ordinal)
  print(formattable::formattable(results))
  
  # Discreet subset 
  discreetDataset<-originalDataset[,which(field_Types_Discreet_Ordinal==TYPE_DISCREET)]
  
  # Ordinals subset
  ordinals<-originalDataset[,which(field_Types_Discreet_Ordinal==TYPE_ORDINAL)]
  
  # Create Bins and complete dataset before normalisation
  ordinalBinsDataset <- ordinals %>% mutate(MonthlyIncome = case_when(MonthlyIncome <= 2500 ~ 1,
                                                                      MonthlyIncome > 2500   & MonthlyIncome <= 5000 ~ 2,
                                                                      MonthlyIncome > 5000   & MonthlyIncome <= 7500 ~ 3,
                                                                      MonthlyIncome > 7500   & MonthlyIncome <= 10000 ~ 4,
                                                                      MonthlyIncome > 10000   & MonthlyIncome <= 12500 ~ 5,
                                                                      MonthlyIncome > 12500   & MonthlyIncome <= 15000 ~ 6,
                                                                      MonthlyIncome > 15000   & MonthlyIncome <= 17500 ~ 7,
                                                                      MonthlyIncome > 17500 ~ 8)) # end function
  
  # Test if any ordinals are outliers and replace with mean values
  ordinalsDataset <- NPREPROCESSING_outlier(ordinals = ordinalBinsDataset, OUTLIER_CONFIDENCE)
  
  # Symbolic subset
  symbolicDataset<-originalDataset[,which(field_types==TYPE_SYMBOLIC)]
  
  # One hot encode the following fields: Gender, OverTime, Attrition, MaritalStatus
  fieldsForEncoding <- c("Gender", "OverTime", "Attrition", "MaritalStatus", "BusinessTravel", "Department", "JobRole", "EducationField")
  oneHotDataset <- oneHotEncoding(dataset = symbolicDataset, fieldsForEncoding =fieldsForEncoding)
  
  # Create factors for ordered categorical fields
  newSymbolicDataset <- oneHotDataset %>% mutate(across(c(Over18),
                                                        ~as.numeric(as.factor(.))))
  
  # Combine symbolic and numeric datasets
  dataBeforeNormalisation<-cbind(ordinalBinsDataset,discreetDataset,newSymbolicDataset)
  
  if(all(sapply(dataBeforeNormalisation, is.numeric ))){
    print("All Fields Are Numeric")
  }
  
  # remove fields that have zero variance
  toRemove <- nearZeroVar(dataBeforeNormalisation, freqCut = FREQCUT) 
  removedCols <- colnames(dataBeforeNormalisation)[toRemove]
  print(paste("Removing the following columns as all values are the same"))
  print(removedCols)
  dataBeforeNormalisation <- dataBeforeNormalisation[, -toRemove]
  
  # Calculate correlation matrix
  corMatrix <- cor(dataBeforeNormalisation)
  
  # find attributes that are highly corrected
  highlyCorrelated <- findCorrelation(corMatrix, CUTOFF)
  
  #names of highly correlated fields
  highlyCorCol <- colnames(dataBeforeNormalisation)[highlyCorrelated]
  print("Removing the following columns due to high correlation")
  print(highlyCorCol)
  
  # remove highly correlated fields
  dataForNormalisation <- dataBeforeNormalisation[, -which(colnames(dataBeforeNormalisation) %in% highlyCorCol)]
  dim(dataForNormalisation)
  
  # normalise dataset using function above
  normalisedDataset <- as.data.frame(lapply(dataForNormalisation,normalise))
  
  # Remove employee number from normalised dataset
  normalisedDataset<-subset(normalisedDataset, select = -EmployeeNumber)
  
  # Transforms all inputs to a logistic model mapped against AttritionYes
  logisticModelTransformAllInputs<-glm(AttritionYes~.,family=binomial(link='logit'),data=normalisedDataset)
  
  # OPTIONAL Show importance
  # uses caret library
  #print(summary(logisticModelTransformAllInputs))
  
  # Analyze the table of deviance
  print("Printing Deviance Analysis:")
  print(anova(logisticModelTransformAllInputs))
  
  # Use caret library to determine scaled "importance"
  importance<-as.data.frame(caret::varImp(logisticModelTransformAllInputs, scale = TRUE))
  
  # Plot the % importance ordered from lowest to highest
  barplot(t(importance[order(importance$Overall),,drop=FALSE]), las = 2, border = 0, cex.names = 0.8)
  
  return(normalisedDataset)
}
# ************************************************
# main() :
# main entry point to execute analytics
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# Keeps all objects as local to this function
# ************************************************
main<-function(){
  print("Inside main function")
  
  normalisedDataset <<- preprocessing(originalDataset = originalDataset)
  
  print("Leaving main")
  
} #endof main()

main()