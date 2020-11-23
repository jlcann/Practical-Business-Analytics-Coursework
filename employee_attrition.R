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
FREQCUT           <- 99/1                 # To remove zero variance fields
FOREST_SIZE       <- 1000                 # Number of trees in the forest

NN_BATCH_SIZE <- 30
NN_OPTIMISER <- "adam"
NN_DROPOUT <- 0.1
NN_HIDDEN_RELU <- 16
NN_HIDDEN_SIGMOID <- 1
NN_EPOCHS <- 100# Maximum number of training epochs

K_FOLDS        <- 20 # Number of holds for stratified cross validation


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
               "tidyrules",
               "OneR",
               "ggplot2",
               "ggpubr",
               "GGally",
               "hrbrthemes",
               "hexbin")

gc() # garbage collection to automatically release memory

# clears the console area
cat("\014")

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("employee-attrition_model_functions.R")
source("employee_attrition_exploration.R")
source("employee-attrition_preprocessing.R")

set.seed(123)



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
  
  #Return a list containing the training and the test datasets with holdout method.
  #holdoutDataset <<- createHoldoutDataset(randomisedDataset, HOLDOUT)

  #Create a stratified data frame ready for stratified k-fold validation
  #stratifiedData <- stratifyDataset(preprocessedDataset,OUTPUT_FIELD,K_FOLDS)
  
    
  #first_model <<- train_MLP_Model(holdoutDataset$training,holdoutDataset$test,OUTPUT_FIELD,plotConf = T)
  
  
  
  #splitModelMeans <<- kFoldModel(train_MLP_Model,stratifiedData,OUTPUT_FIELD)
  
  #Create standard decision trees from raw data and pre-processed data
  
  #processedDT <- createDT(trainingSet, testSet, OUTPUT_FIELD, T)
    
  #kfoldTree <<- kFoldModel(createDT, stratifiedData, OUTPUT_FIELD, plot=T)
  
  #kfoldTree <<- kFoldModel(createForest, stratifiedData, OUTPUT_FIELD, forestSize = FOREST_SIZE, plot=T)
  
  #randomisedRawDataset <- originalDataset[sample(nrow(originalDataset)),]
  
  # The decision tree cannot be made if there are fields where each record has the same value, take these out
   
  # randomisedRawDatasetWithoutConstantFields = select(randomisedRawDataset, -c("Over18", "EmployeeCount"))
  
  # rawTrainingSet <- randomisedRawDatasetWithoutConstantFields[1:trainingSampleSize,]
  
  # rawTestSet <- randomisedRawDatasetWithoutConstantFields[-(1:trainingSampleSize),]
  
  # rawDT <- createDT(rawTrainingSet, rawTestSet, ORIGINAL_OUTPUT_FIELD)

  
  # Print the rules for the trees
  #kfoldTreeRules <<- getTreeRules(kfoldTree, T)
  
  # rawDTRules <<- getTreeRules(rawDT, T)

   

  #processedForest <- createForest(trainingSet,OUTPUT_FIELD,FOREST_SIZE)

  #negativeImp<-getNegativeImportance(processedForest)
  
  #newDatasetForForest = select(stratifiedData, -negativeImp)
  
  #ReCreate the training Set
  
  #trainingSetForest <- newDatasetForForest[1:trainingSampleSize,]
  
  #reProcessedForest <<- createForest(trainingSetForest,OUTPUT_FIELD,FOREST_SIZE)
  
  # Plots for the exploration phase
  explorationPlots()
  
  
}

main()