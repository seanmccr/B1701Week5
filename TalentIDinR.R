# B1701 Week 5 | Talent Identification in R | 11.10.23

# ------ Install & Load Packages ------ 

install.packages("corrplot")
install.packages("fmsb")
install.packages("summarytools")
library(summarytools)
library(tidyverse)
library(moments)
library(corrplot)


# ----- Loading Data -----
CyclingData <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 5/Week5TalentIDFiles/CyclingTI.csv")

# ----- Cleaning Data -----
str(CyclingData)

# Changing the date of birth variable type
CyclingData$Date_of_Birth <- as.character.Date(CyclingData$Date_of_Birth)

# Identifying 'NA' inputs and the sum of 'NA' inputs per variable
MissingData <- is.na(CyclingData)
print(MissingData)
ColMissData <- colSums(MissingData)
print(ColMissData)

# Code to identify duplicated data
which(duplicated(CyclingData))

# Filtering the data
FilteredCyclingData <- CyclingData %>%
  filter(Gender == "Male" & Age >= 15 & Age <= 17)

# ----- Exploring the Data -----

# Code to select the numeric variables in our dataset
NumericVars <- FilteredCyclingData %>%
  select_if(is.numeric)
# Code to provide a summary for the numeric variables
for (var in names(NumericVars)) {
  VarSummary <- summary(NumericVars[[var]],na.rm=TRUE)
  # Code to print the summary statistics
  cat("Variable:", var, "\n")
  print(VarSummary)
  cat("\n")
}

# Use these commands to tidy up the environment and value sections
rm(MissingData)
rm(VarSummary)
rm(var)
# WE HAVE 15 VARIABLES IN 'NumericVars' BECAUSE IT ONLY INCLUDES NUMERICAL VARIABLES!

# ----- Creating Distributions -----

# Create a histogram to visualize the distribution and outliers of our numerical data
NumericVars <- NumericVars %>% select(-1,-4,-9,-10,-11,-12,-13,-14,-15)
  
for (var in names(NumericVars)) {  
  p_value <- (shapiro.test(FilteredCyclingData[[var]]))$p.value
  skew <- skewness(FilteredCyclingData[[var]])
  kurt <- kurtosis(FilteredCyclingData[[var]])
  Shaplabel <- paste("Shapiro-Wilk p-value: ", format(p_value, digits = 4))
  Skewlabel <- paste("Skewness: ", format(skew, digits = 4))
  Kurtlabel <- paste("Kurtosis: ", format(kurt, digits = 4))
  # Histogram code (use aes_string as our variables are treated as strings, rather than there names)
  histo <- ggplot(FilteredCyclingData, aes_string(x = var)) +
    geom_histogram() +
    ggtitle(paste("Histogram of", var)) +
    labs(subtitle = paste(Shaplabel, "\n", Skewlabel, "\n", Kurtlabel, "\n"))
  
  print(histo)
  

}

# WHEN GRAPHS ARE CREATED, THEY ARE DIFFERENT; HAVE I NOT CLEANED THE DATA PROPERLY?

# ----- Correlation -----

# Code to create a correlation matrix from our 'NumericVars' dataframe
CorMatx <- cor(NumericVars, use = "complete.obs")
CorMatx
# Code to create a correlation plot using the correlation matrix created
corrplot(CorMatx, method = "color")

# ----- Creating New Variables -----
# Code to create new variables using existing variables. ADD TO ORIGINAL DATASET!
# This adds the new variables to the original dataset, meaning we can easily find it
# in future practice.
FilteredCyclingData <- FilteredCyclingData %>%
  mutate(
    PeakPowerKG = peak_power/weight,
    FTP_KG = ftp/weight)

# ----- Creating a new Correlation Plot -----

# Creating the dataframe with the selected numeric variables
NumericVars2 <- FilteredCyclingData %>%
  select_if(is.numeric)
# Code to provide a summary of our numeric variables
for (var in names(NumericVars2)) {
  VarSummary2 <- summary(NumericVars2[[var]], na.rm = TRUE)
  # Code to print summary of our numeric variables
  cat("Variable", var, "\n")
  print(VarSummary2)
  cat("\n")
  }
# Code to select the variables we want in our histogram (also includes outliers and distribution)
NumericVars2 <- NumericVars2 %>% select(-1,-2,-4,-5,-6,-9,-10,-11,-12,-13,-14,-15)

for (var in names(NumericVars2)) {  
  p_value <- (shapiro.test(FilteredCyclingData[[var]]))$p.value
  skew <- skewness(FilteredCyclingData[[var]])
  kurt <- kurtosis(FilteredCyclingData[[var]])
  Shaplabel <- paste("Shapiro-Wilk p-value: ", format(p_value, digits = 4))
  Skewlabel <- paste("Skewness: ", format(skew, digits = 4))
  Kurtlabel <- paste("Kurtosis: ", format(kurt, digits = 4))
  # Histogram coding: use aes_string so that the variable is treated as its name and not the string
  histo <- ggplot(FilteredCyclingData, aes_string(x = var)) +
    geom_histogram() +
    ggtitle(paste("Histogram of", var)) +
    labs(subtitle = paste(Shaplabel, "\n", Skewlabel, "\n", Kurtlabel, "\n"))
  
  print(histo)
  
}
# Code to create the correlation matrix
CorMatx2 <- cor(NumericVars2, use = "complete.obs")
CorMatx2
# Code to create the correlation plot
corrplot(CorMatx2, method = "color")

# Creating a Scatter plot using our new Power variables
# Scatterplot No.1
PkPwPlot <- ggplot(FilteredCyclingData, aes(peak_power, weight))+
  geom_point() + stat_smooth(method = "lm",
                             formula = y ~ x,
                             geom = "smooth")
print(PkPwPlot)
# Scatterplot No.2
PkPwPlot2 <- ggplot(FilteredCyclingData, aes(PeakPowerKG, weight))+
  geom_point() + stat_smooth(method = "lm",
                             formula = y ~ x,
                             geom = "smooth")
print(PkPwPlot2)
# Scatterplot No.3
PkPwPlot3 <- ggplot(FilteredCyclingData, aes(ftp, weight))+
  geom_point() + stat_smooth(method = "lm",
                             formula = y ~ x,
                             geom = "smooth")
print(PkPwPlot3)
# Scatterplot No.4
PkPwPlot4 <- ggplot(FilteredCyclingData, aes(FTP_KG, weight))+
  geom_point() + stat_smooth(method = "lm",
                             formula = y ~ x,
                             geom = "smooth")
print(PkPwPlot4)


DescriptiveTable <- desc(FilteredCyclingData[, c(4:19)])
print(DescriptiveTable)