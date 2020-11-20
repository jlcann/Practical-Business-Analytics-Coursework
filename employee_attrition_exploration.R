# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()


# ************************************************
# basicStatistics()
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
basicStatistics<-function(dataset,...){
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}


# ************************************************
# FieldTypes() :
#
# Test each field for NUMERIC or SYMBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
FieldTypes<-function(dataset){
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }
    
    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}


# ****************
# barplotDiscreet() :
#   Exploration method to plot a barplot for each 
#   discreet field to visualize field spread
#
# INPUT       :   dataframe - dataset           - dataset to barplot
#
# OUTPUT      :   barplot for each field in dataset
# ****************

barplotDiscreet <-function(dataset){
  for(field in 1:(ncol(dataset))){
    single <- table(dataset[field])
    barplot(single,
            main = paste("Barplot for: ", names(dataset[field])),
            beside=TRUE,
            xlab = "Level/rating",
            col = rainbow(10)
            
    )
  }
}


# ****************
# boxplotAll() :
#   Exploration method to plot a boxplot for each 
#   field to visualize data and see outliers
#
# INPUT       :   dataframe - dataset           - dataset to boxplot
#
# OUTPUT      :   boxplot for each field in dataset
# ****************
boxplotAll <-function(dataset){
  for(field in 1:(ncol(dataset))){
    boxplot(dataset[field],
            main = paste("Boxplot for: ", names(dataset[field])),
            xlab = "Units",
            ylab = names(dataset[field]),
            col = "#FF3F3F",
            border = "black",
            horizontal = TRUE
    )
  }
}


# ****************
# attritionBars() :
#   Exploration method to plot a barplot the top 5 
#   important field against Attrition
#
# INPUT       :   dataframe - dataset           - dataset to barplot
#                 field - outcome               - field to plot against
#
# OUTPUT      :   barplot for each field in dataset
# ****************


attritionBars <- function(dataset, field){ # EDIT X AXIS EDIT X AXIS EDIT X AXIS EDIT X AXIS EDIT X
  
  for (i in colnames(dataset)){
    if (i == field){
      next
    } else {
      print(i)
      
      graph <- dataset %>%
        ggplot(aes_string(x = i, group = field)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
                 stat="count", 
                 alpha = 0.7) +
        geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
                  stat= "count", 
                  vjust = -0.4) +
        labs(y = "Percentage", fill= i) +
        facet_grid(as.formula(paste("~", field))) +
        theme_minimal()+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
        ggtitle(paste("Attrition VS", i))
      
      print(graph)
    }
  }
  
}



lewisPlots <-function() {
  # Barplot for Attrition count
  barplotAttrition <- originalDataset %>% # LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS
    group_by(Attrition) %>%
    tally() %>%
    ggplot(aes(x = Attrition, y = n,fill=Attrition)) +
    geom_bar(stat = "identity") +
    theme_minimal()+
    scale_fill_manual(values=c("#FF3F3F", "#9EFF95"))+
    labs(x="Attrition", y="Count of Attriation")+
    ggtitle("Attrition Count")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_text(aes(label = n), vjust = -0.4, position = position_dodge(0.7))
  
  # Print the Attrition barplot
  print(barplotAttrition) # LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS
  
  # Barplot job role against Attrition
  jobRoleAttrition <- originalDataset %>%
    ggplot(mapping = aes(x = JobRole)) +
    scale_fill_manual(values=c("#FF3F3F", "#9EFF95"))+
    geom_histogram(aes(fill = Attrition), stat = "count")+
    stat_count(aes(y=..count..,label=..count.., group = Attrition),geom="text",vjust=-1)+
    theme(legend.position = "top", plot.title = element_text(hjust = 0.5))+
    labs(x="Job Role", y="Number Attriation")+
    ggtitle("Attrition in regards to Job Role")
  
  # Print the Job Role against Attrition barplot
  print(jobRoleAttrition)
  
  # Barplot Monthly income against Attrition
  monthlyIncomeAttrition<-originalDataset %>%
    ggplot(mapping = aes(x = MonthlyIncome)) + 
    scale_fill_manual(values=c("#FF3F3F", "#9EFF95"))+
    geom_histogram(aes(fill = Attrition), bins=8)+
    stat_bin(bins = 8, geom="text",aes(label=..count.., group=Attrition), vjust=-1)+
    theme(legend.position = "top", plot.title = element_text(hjust = 0.5))+
    labs(x="Monthlt Income", y="Number Attriation")+
    ggtitle("Attrition in regards to Monthly Income")
  
  # Print the Monthly Income against Attrition barplot
  print(monthlyIncomeAttrition)
  
  #crash = crashhere(print) # LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS
  
  # Boxplot all numeric fields
  boxplotAllFields<-boxplotAll(originalDataset[,which(field_types==TYPE_NUMERIC)]) # LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS
  
  # Pearson correlation plot for all numeric fields
  #print(ggcorr(originalDataset[,which(field_types==TYPE_NUMERIC)], method = c("everything", "pearson")))# LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS
  
  # Boxplot Monthly Income for each Job Level
  boxplotIncomeJob <- boxplot(MonthlyIncome~JobLevel, # LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS
                              data=originalDataset,
                              main="Boxplots for Monthly Income against Job Level",
                              xlab="Job Level",
                              ylab="MonthlyIncome",
                              col="#FF3F3F",
                              border="black"
  )
  
  # Boxplot Monthly Income for Gender
  boxplotIncomeGender <- boxplot(MonthlyIncome~Gender, # LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS
                                 data=originalDataset,
                                 main="Boxplots for Monthly Income against Gender",
                                 xlab="Gender",
                                 ylab="MonthlyIncome",
                                 col="#9EFF95",
                                 border="black"
  )
  
  # Boxplot Monthly Income for OverTime Yes and No
  boxplotIncomeOverTime <- boxplot(MonthlyIncome~OverTime, # LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS
                                   data=originalDataset,
                                   main="Boxplots for Monthly Income against Over Time",
                                   xlab="OverTime",
                                   ylab="MonthlyIncome",
                                   col="#FF3F3F",
                                   border="black"
  )
  
  # Boxplot Daily Rate for OverTime Yes and No
  boxplotDailyOverTime <- boxplot(DailyRate~OverTime, # LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS LEWIS
                                  data=originalDataset,
                                  main="Boxplots for Daily Rate against Over Time",
                                  xlab="OverTime",
                                  ylab="DailyRate",
                                  col="#9EFF95",
                                  border="black"
  )
  
  # Plot scatterplot for Monthly Income against Total Working Years (experience)
  scatterIncomeExperience <- ggplot(originalDataset, aes(x=TotalWorkingYears, y=MonthlyIncome)) +
    geom_point(size=2, shape=1) +
    geom_smooth(method=lm,color="#FF3F3F")
  
  # Plot scatterplot for Monthly Income against Years At Company (loyalty)
  scatterIncomeLoyalty <- ggplot(originalDataset, aes(x=YearsAtCompany, y=MonthlyIncome)) +
    geom_point(size=2, shape=1) +
    geom_smooth(method=lm,color="#9EFF95")
  
  # Join the scatterplots on one page for Monthly Income based on experience and loyalty for comparison
  doubleScatterPlot <- ggarrange(scatterIncomeExperience, scatterIncomeLoyalty + rremove("x.text"), 
                                 labels = c("Experience", "Loyalty"),
                                 ncol = 2, nrow = 1)
  
  # Print the scatterplots for experience and loyalty
  print(doubleScatterPlot)
  
  # Density hex plot of Age against MonthlyIncome
  densityAgeIncome<-ggplot(originalDataset, aes(x=Age, y=MonthlyIncome) ) +
    geom_hex(bins = 8) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()
  
  # Print density plot of age against monthly income
  print(densityAgeIncome)
  
  # Dataset of the 5 most impactful fields on Attrition
  highImportanceDataset <- subset(originalDataset, select=c(JobInvolvement, JobSatisfaction, NumCompaniesWorked, 
                                                            EnvironmentSatisfaction, OverTime, Attrition))
  
  # Bar plots for the high importance fields against attrition
  barplotImportantFields <- attritionBars(highImportanceDataset, "Attrition") 
}