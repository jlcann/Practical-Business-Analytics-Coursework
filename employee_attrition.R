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

# ************************************************
# GLOBAL FUNCTIONS

# ************************************************
# oneHotEncoding() :
#   Pre-processing method to convert appropriate 
#   categorical fields into binary representation
#
# INPUT       :   Categoric fields to encode
#
# OUTPUT      :   Encoded fields
# ************************************************
oneHotEncoding<-function(...){
  # Combine input fields for encoding
  fieldsForEncoding = c(...)
  
  # One hot encode fields listed in function
  dmy <- dummyVars(" ~ .", data = fieldsForEncoding)
  trsf<- data.frame(predict(dmy, newdata = originalDataset[,which(field_types==TYPE_SYMBOLIC)]))
  
  # Combine the encoded fields back to the originalDataset
  encodedDataset <- cbind(originalDataset[,which(field_types==TYPE_SYMBOLIC)],trsf)
  
  # Remove original fields that have been hot encoded
  newData <- subset(encodedDataset, select = -c(Gender, OverTime, Attrition, MaritalStatus)) # HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 
  
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
  
  print(DATASET_FILENAME)
  
  #Load the dataset into a variable named originalDataSet.
  
  
  #Do you have unexpected field names or data? There are some complexities when loading text from different systems,
  #e.g. Windows PC or Mac, where characters are “encoded” differently. You may want to search google on “UTF-8” if you
  #have compatibility issues. To solve this, load the CSV file into Excel and then “Save as…”, selecting the FILE FORMAT to
  #save as “MS-DOS Comma Seperated”. Spare a moment to think about what issues you might face with much larger datasets,
  #collected on different IT systems, maybe in different counties and what might be involved to get this data into a format that you
  #can use for ML.
  #Print statistics of originalDataSet into the viewer.
  basicStatistics(originalDataset)
  
  # Determine if fields are SYMBOLIC or NUMERIC (global)
  field_types<<-FieldTypes(originalDataset)
  
  # Determine if NUMERIC fields are DISCREET or ORDINAL
  field_Types_Discreet_Ordinal<- NPREPROCESSING_discreetNumeric(originalDataset,field_types,DISCREET_BINS)
  discreet_fields <- names(originalDataset)[field_Types_Discreet_Ordinal=="DISCREET"]
  
  results<-data.frame(field=names(originalDataset),initial=field_types,types1=field_Types_Discreet_Ordinal)
  print(formattable::formattable(results))
  
  # Ordinals subset
  discreetDataset<-originalDataset[,which(field_Types_Discreet_Ordinal==TYPE_DISCREET)]
  
  # Ordinals subset
  ordinals<-originalDataset[,which(field_Types_Discreet_Ordinal==TYPE_ORDINAL)]
  
  # Test if any ordinals are outliers and replace with mean values
  ordinalsDataset <- NPREPROCESSING_outlier(ordinals = ordinals, OUTLIER_CONFIDENCE)
  
  # Create Bins and complete dataset before normalisation
  ordinalBinsDataset <- ordinalsDataset %>% mutate(MonthlyIncome = case_when(MonthlyIncome <= 2500 ~ 1,
                                                                             MonthlyIncome > 2500   & MonthlyIncome <= 5000 ~ 2,
                                                                             MonthlyIncome > 5000   & MonthlyIncome <= 7500 ~ 3,
                                                                             MonthlyIncome > 7500   & MonthlyIncome <= 10000 ~ 4,
                                                                             MonthlyIncome > 10000   & MonthlyIncome <= 12500 ~ 5,
                                                                             MonthlyIncome > 12500   & MonthlyIncome <= 15000 ~ 6,
                                                                             MonthlyIncome > 15000   & MonthlyIncome <= 17500 ~ 7,
                                                                             MonthlyIncome > 17500   & MonthlyIncome <= 20000 ~ 8)) # end function
  
  # Symbolic subset
  symbolicDataset<-originalDataset[,which(field_types==TYPE_SYMBOLIC)]
  
  # One hot encode the following fields: Gender, OverTime, Attrition, MaritalStatus
  oneHotDataset <- oneHotEncoding(symbolicDataset['Gender'],symbolicDataset['OverTime'],
                                  symbolicDataset['Attrition'],symbolicDataset["MaritalStatus"])
  
  # Create factors for ordered categorical fields
  newSymbolicDataset <- oneHotDataset %>% mutate(across(c(BusinessTravel, Department, JobRole, EducationField, Over18), # HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE
                                                        ~as.numeric(as.factor(.))))
  
  # Combine symbolic and numeric datasets
  dataBeforeNormalisation<-cbind(ordinalBinsDataset,discreetDataset,newSymbolicDataset)
  
  if(all(sapply(dataBeforeNormalisation, is.numeric ))){
    print("All Fields Are Numeric")
  }
  
  # Make sure there are no NA's
  any(is.na(dataBeforeNormalisation))
  
  # remove fields that have zero variance
  toRemove <- nearZeroVar(dataBeforeNormalisation) 
  dataBeforeNormalisation <- dataBeforeNormalisation[, -toRemove] # HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE
  
  # Calculate correlation matrix
  corMatrix <- cor(dataBeforeNormalisation)
  
  # find attributes that are highly corrected
  highlyCorrelated <- findCorrelation(corMatrix, cutoff=0.9)
  
  #names of highly correlated fields
  highlyCorCol <- colnames(dataBeforeNormalisation)[highlyCorrelated]
  
  # remove highly correlated fields
  dataForNormalisation <- dataBeforeNormalisation[, -which(colnames(dataBeforeNormalisation) %in% highlyCorCol)]
  dim(dataForNormalisation)
  
  # normalise dataset using function above
  normalisedDataset <- as.data.frame(lapply(dataForNormalisation,normalise))
  
  # Transforms all inputs to a linear model mapped against AttritionYes
  linearModelTransformAllInputs<-lm(AttritionYes~.,data=normalisedDataset)
  
  # OPTIONAL Show importance
  # uses caret library
  print(summary(linearModelTransformAllInputs))
  
  # Use caret library to determine scaled "importance"
  importance<-as.data.frame(caret::varImp(linearModelTransformAllInputs, scale = TRUE))
  
  # Plot the % importance ordered from lowest to highest
  barplot(t(importance[order(importance$Overall),,drop=FALSE]))
  
  print("Leaving main")
  
} #endof main()

main()

