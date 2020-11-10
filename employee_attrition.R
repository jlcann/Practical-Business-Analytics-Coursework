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
               "caret")





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
  basicStatistics(originalDataSet)
  
  
  field_types<-FieldTypes(originalDataSet)

  #numeric_fields <- names(originalDataSet)[field_types=="NUMERIC"]
  #print(paste("Numeric Fields =", length(numeric_fields)))
  #print(numeric_fields)
  #symbolic_fields <- names(originalDataSet)[field_types=="SYMBOLIC"]
  #print(paste("SYMBOLIC Fields =", length(symbolic_fields)))
  #print(symbolic_fields)

  discreetDataset <- NPREPROCESSING_discreetNumeric(originalDataSet,field_types,DISCREET_BINS)
  
  discreet_fields <- names(originalDataSet)[discreetDataset=="DISCREET"]
  print(paste("Discreet Fields =", length(discreet_fields)))
  print(discreet_fields)
  print("Leaving main")
  
} #endof main()


# clears the console area
cat("\014")

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("employee_attrition_functions.R")

set.seed(123)

originalDataSet <- readDataset(DATASET_FILENAME)
main()