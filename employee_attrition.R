rm(list=ls())
# Global Environment variables
# - i.e. available to all functions
# Constants are definied at the top of the file in capital snake case

DATASET_FILENAME  <- "employee-attrition.csv"   # Name of input dataset file
OUTPUT_FIELD      <- "AttritionYes"             # Field name of the output class to predict
ORIGINAL_OUTPUT_FIELD <- "Attrition"            # Field name of the output class in the original dataset

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded
DISCREET_BINS     <- 5                    # Number of Discreet Bins Required for 
OUTLIER_CONFIDENCE <- 0.99                # Confidence of discreet 
CUTOFF            <- 0.90                 # Correlation cutoff
HOLDOUT           <- 70                   # Holdout percentage for training set
K_FOLDS           <- 10                    # Number of holds for stratified cross validation
FREQCUT           <- 99/1                 # To remove zero variance fields
FOREST_SIZE       <- 1000                 # Number of trees in the forest

NN_HIDDEN_LAYER_NEURONS <- 10 # 10 hidden layer neurons
NN_EPOCHS <- 50# Maximum number of training epochs



# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3-51.6
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4
# Carat                  6.0-86
# dplyr                  2.0.0
# C50                    0.1.3.1
# randomForest           4.6-14
# keras                  2.3.0.0
# tensorflow             2.2.0
# stringr                1.4.0
# tidyrules              0.1.5
MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "PerformanceAnalytics",
               "caret",
               "dplyr",
               "C50",
               "randomForest",
               "keras",
               "tensorflow",
               "stringr",
               "tidyrules")

gc() # garbage collection to automatically release memory

# clears the console area
cat("\014")

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("employee_attrition_functions.R")
source("employee-attrition_model_functions.R")

set.seed(321)


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
  
  originalDataset <<- read.csv(DATASET_FILENAME, encoding = "UTF-8", stringsAsFactors = FALSE)
  
  preprocessedDataset <<- preprocessing(originalDataset)
  
  randomisedDataset <- preprocessedDataset[sample(nrow(preprocessedDataset)),]
  
  #Create a training Sample Size
  trainingSampleSize <- round(nrow(randomisedDataset))*(HOLDOUT/100)
  

  #Create a stratified data frame ready for stratified k-fold validation
  stratifiedData <- stratifyDataset(preprocessedDataset,OUTPUT_FIELD,K_FOLDS)

  #Create the training Set
  trainingSet <- stratifiedData[1:trainingSampleSize,]
  
  #Create the test Set
  testSet <- stratifiedData[-(1:trainingSampleSize),]

  #Create a stratified data frame ready for stratified k-fold validation
  stratifiedData <- stratifyDataset(preprocessedDataset,OUTPUT_FIELD,K_FOLDS)

  #Uncomment below to test the MLP model with 70/30 holdout
  #first_model <<- train_MLP_Model(trainingSet,OUTPUT_FIELD,NN_HIDDEN_LAYER_NEURONS,NN_EPOCHS,testSet)
  
  
  #Returns the dataframe containing the results of each split at the moment, as the averages have not been calculated.
  #Play around with the Hidden Layer Neurons, Epochs and the number of folds.
  #Model layers can be played with in the model_functions script.
  #Will return a data.frame in the evironment for you to look at.
  splitModelMeans <<- kFoldModel(train_MLP_Model,stratifiedData,OUTPUT_FIELD,NN_HIDDEN_LAYER_NEURONS,NN_EPOCHS)
  #Create standard decision trees from raw data and pre-processed data
  processedDT <- createDT(trainingSet, OUTPUT_FIELD, T)
  
  randomisedRawDataset <- originalDataset[sample(nrow(originalDataset)),]
  
  # The decision tree cannot be made if there are fields where each record has the same value, take these out
  randomisedRawDatasetWithoutConstantFields = select(randomisedRawDataset, -c("Over18", "EmployeeCount"))
  rawTrainingSet <- randomisedRawDatasetWithoutConstantFields[1:trainingSampleSize,]
  rawTestSet <- randomisedRawDatasetWithoutConstantFields[-(1:trainingSampleSize),]
  rawDT <- createDT(rawTrainingSet, ORIGINAL_OUTPUT_FIELD)
  
  # Evaluate and compare the decision trees
  processedDTClassifications <- getTreeClassifications(processedDT, testSet, OUTPUT_FIELD)
  processedDTMetrics <<- getTreeMetrics(processedDTClassifications, testSet, OUTPUT_FIELD)
  
  rawDTClassifications <- getTreeClassifications(rawDT, rawTestSet, ORIGINAL_OUTPUT_FIELD)
  rawDTMetrics <<- getTreeMetrics(rawDTClassifications, rawTestSet, ORIGINAL_OUTPUT_FIELD, classLabelChar = 'Yes')
  
  # Print the rules for the trees
  processedDTRules <<- getTreeRules(processedDT, T)
  rawDTRules <<- getTreeRules(rawDT, T)

  #processedForest <- createForest(trainingSet,OUTPUT_FIELD,FOREST_SIZE)

  #negativeImp<-getNegativeImportance(processedForest)
  
  newDatasetForForest = select(stratifiedData, -negativeImp)
  
  #ReCreate the training Set
  trainingSetForest <- newDatasetForForest[1:trainingSampleSize,]
  
  reProcessedForest <<- createForest(trainingSetForest,OUTPUT_FIELD,FOREST_SIZE)
  
   
  return("test")



}

main()