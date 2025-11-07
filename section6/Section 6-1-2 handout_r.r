# Remove all objects from workspace
rm(list = ls())
# Load required packages
library(dplyr)
library(haven)
library(stargazer)

# Load the data
data <- read_dta("C:/Users/rodrigo.morales/OneDrive - Ave Maria University/Documents/classes/230Stats_2025fall/worksheets/Section6/3 Describing Relations - 04 Multiple Regression - wdi data.dta")
#data <- read_dta("C:/Users/rodrigo.morales/OneDrive - Ave Maria University/Documents/classes/230AppliedStats_2025spring/worksheets/3 Describing Relations - 04 Multiple Regression - wdi data.dta")

# Set up the variables
#===============================================================================
# Describe the data
str(data)

# Calculate skewness for each variable
library(e1071) ## Can use this package or the other
#library(moments)
skew_vars <- c("lifeexpectancy", "co2emissions", "gdppercapita", "sanitation", 
               "popgrowth", "maternalmort", "hospitalbeds", "physicians", 
               "nurses", "lfFemale", "hsFemale")
sapply(data[skew_vars], skewness, na.rm = TRUE)
skews <- sapply(data[skew_vars], skewness, na.rm = TRUE)

##library(moments)
# Calculate skewness
#skewness(data[["co2emissions"]], na.rm = TRUE)

# Load all functions from the script
source("C:/Users/rodrigo.morales/OneDrive - Ave Maria University/Documents/classes/230Stats_2025fall/worksheets/Section6/lnskew0.R")

lognames <- skew_vars
# Compute lnskew0 for positive skews:
for(i in skew_vars[skews>0]){
  cat("\n variable = ", i)
  tti <- lnskew0(data[[i]])
  # Create variable name dynamically
  new_var_name <- paste0("log", i)
  lognames[which(i==skew_vars)]<- new_var_name
  data[[new_var_name]] <- tti$transformed
  
  cat("\n k = ", tti$k)
  cat("        new_skewness = ", tti$transformed_skewness)
}

# Compute lnskew with box-cox transformation for neg skewness:
for(i in skew_vars[skews<0]){
  cat("\n variable = ", i)
  tti <- lnskew0(data[[i]])
  # Create variable name dynamically
  new_var_name <- paste0("log", i)
  lognames[which(i==skew_vars)]<- new_var_name
  data[[new_var_name]] <- log(-tti$k) - tti$transformed
  
  cat("\n k = ", tti$k)
  #cat("        new_skewness = ", tti$transformed_skewness)
  cat("        new_skewness = ", skewness(data[[new_var_name]],na.rm=TRUE))
}

# Create log transformations (simplified approach)
# For right-skewed variables
# data$logco2 <- log(data$co2emissions)
# data$loggdp <- log(data$gdppercapita)
# data$logpop <- log(data$popgrowth)
# data$logmater <- log(data$maternalmort)
# data$loghosp <- log(data$hospitalbeds)
# data$logphys <- log(data$physicians)
# data$lognurs <- log(data$nurses)
# 
# # For left-skewed variables (reflect then log)
# data$loglife <- log(max(data$lifeexpectancy, na.rm = TRUE) - data$lifeexpectancy + 1)
# data$logsanitation <- log(max(data$sanitation, na.rm = TRUE) - data$sanitation + 1)
# data$loglf <- log(max(data$lfFemale, na.rm = TRUE) - data$lfFemale + 1)
# data$loghs <- log(max(data$hsFemale, na.rm = TRUE) - data$hsFemale + 1)
# #===============================================================================

##rename some variables:
lognames
data$loglife <- data$loglifeexpectancy
data$logphys <- data$logphysicians
data$logpop <- data$logpopgrowth
data$logco2 <- data$logco2emissions
data$loggdp <- data$loggdppercapita
data$logmater <- data$logmaternalmort
data$loglf <- data$loglfFemale
data$loghs <- data$loghsFemale


# Initial regressions

model1 <- lm(loglifeexpectancy ~ logphys, data = data)
summary(model1)
nobs(model1)
#===============================================================================
# Multiple models with population and CO2
model2 <- lm(loglife ~ logpop, data = data)
model3 <- lm(loglife ~ logco2, data = data)
model4 <- lm(loglife ~ logpop + logco2, data = data)

summary(model2)
summary(model3)
summary(model4)

# Display results in a table
stargazer(model2, model3, model4, type = "text",
          title = "Determinants of Life Expectancy",
          keep.stat = c("n", "rsq", "adj.rsq"))
#===============================================================================

# CO2 and its confounders
#===============================================================================
model5 <- lm(loglife ~ loggdp, data = data)
model6 <- lm(loglife ~ logsanitation, data = data)
model7 <- lm(loglife ~ loggdp + logsanitation, data = data)

stargazer(model5, model6, model7, type = "text",
          title = "Determinants of Life Expectancy",
          keep.stat = c("n", "rsq", "adj.rsq"))

model8 <- lm(loglife ~ logco2, data = data)
model9 <- lm(loglife ~ logco2 + loggdp, data = data)
model10 <- lm(loglife ~ logco2 + loggdp + logsanitation, data = data)

stargazer(model8, model9, model10, type = "text",
          title = "Determinants of Life Expectancy",
          keep.stat = c("n", "rsq", "adj.rsq"))
#===============================================================================

# Life Expectancy and Health Care
#===============================================================================
model11 <- lm(loglife ~ loghospitalbeds, data = data)
model12 <- lm(loglife ~ logphysicians, data = data)
model13 <- lm(loglife ~ lognurses, data = data)

summary(model11)
summary(model12)
summary(model13)

model14 <- lm(loglife ~ loghospitalbeds + logphysicians, data = data)
model15 <- lm(loglife ~ loghospitalbeds + lognurses, data = data)
model16 <- lm(loglife ~ logphysicians + lognurses, data = data)

stargazer(model14, model15, model16, type = "text",
          title = "Determinants of Life Expectancy",
          keep.stat = c("n", "rsq", "adj.rsq"))
#===============================================================================

# Life Expectancy and Status of Women
#===============================================================================
# Scatter plot by region
library(ggplot2)
ggplot(data, aes(x = lfFemale, y = lifeexpectancy, color = region)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Life Expectancy vs Female Labor Force by Region")

model17 <- lm(loglife ~ logmater, data = data)
model18 <- lm(loglife ~ loglf, data = data)
model19 <- lm(loglife ~ loghs, data = data)

summary(model17)
summary(model18)
summary(model19)

model20 <- lm(loggdppercapita ~ loglfFemale, data = data)
summary(model20)

model21 <- lm(loglife ~ logmater + loglf, data = data)
model22 <- lm(loglife ~ logmater + loghs, data = data)
model23 <- lm(loglife ~ loglf + loghs, data = data)
model24 <- lm(loglife ~ logmater + loglf + loghs, data = data)
model25 <- lm(loglife ~ logmater + loglf + loghs + loggdp, data = data)

stargazer(model21, model22, model23, model24, model25, type = "text",
          title = "Determinants of Life Expectancy",
          keep.stat = c("n", "rsq", "adj.rsq"))
#===============================================================================

# Population Growth and its confounders
#===============================================================================
model26 <- lm(loglife ~ logpop, data = data)
model27 <- lm(loglife ~ logpop + logphysicians, data = data)
model28 <- lm(loglife ~ logpop + logmater, data = data)
model29 <- lm(loglife ~ logpop + logphysicians + logmater, data = data)

stargazer(model26, model27, model28, model29, type = "text",
          title = "Determinants of Life Expectancy",
          keep.stat = c("n", "rsq", "adj.rsq"))
#===============================================================================

model30 <- lm(loglf ~ loghospitalbeds, data = data)
summary(model30)

model31 <- lm(loglf ~ loghs, data = data)
summary(model31)

