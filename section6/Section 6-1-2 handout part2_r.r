# Remove all objects from workspace
rm(list = ls())

# Load required packages
library(haven)
library(dplyr)
library(ggplot2)
library(stargazer)

library(Counterfactual)
# Load the NLSW88 dataset
# Note: You need to import this from Stata with the Counterfactual package
# For now, assuming you have the data file
data(nlsw88)

?nlsw88

data <- nlsw88
#data <- read_dta("nlsw88.dta")

# Describe and summarize the data
str(data)
summary(data)

# Boxplot of wage by married status
boxplot(wage~married, data=data, main = "Distribution by Marital Status",
        horizontal = TRUE,
        xlab = "Marital Status",
        ylab = "Response Variable",
        col = c("lightblue", "lightcoral"),
        names = c("Single", "Married"),  # Custom labels
        border = "darkblue") #,   notch = TRUE)  # Notched boxplot for median comparison

ggplot(data, aes(x = factor(married), y = wage)) +
  geom_boxplot() +
  labs(x = "Married", y = "Wage") +
  theme_minimal()

# T-test: wage by married status
t.test(wage ~ married, data = data)
t.test(wage ~ married, data = data, var.equal = TRUE)

# Create log wage variable
data$lwage <- log(data$wage)

# Boxplot of log wage by married status
ggplot(data, aes(x = factor(married), y = lwage)) + #, fill = factor(married)
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(labels = c("0" = "Single", "1" = "Married")) +
  labs(x = "Married", y = "Log Wage") + #, fill = "Group"
  theme_minimal()

# T-test: log wage by married status
t.test(lwage ~ married, data = data)

# Boxplot of wage by college graduate status
ggplot(data, aes(x = factor(collgrad), y = wage)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(labels = c("0" = "No College", "1" = "College")) +
  labs(x = "College Graduate", y = "Wage") +
  theme_minimal()

# T-tests: wage and log wage by college graduate
t.test(wage ~ collgrad, data = data)
t.test(lwage ~ collgrad, data = data)

# Lowess smoothing: wage vs total experience (level)
ggplot(data, aes(x = ttl_exp, y = wage)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Wage vs Total Experience (Level)") +
  theme_minimal()

# Create log experience variable
data$logexp <- log(data$ttl_exp)

# Lowess smoothing: log wage vs log experience
ggplot(data, aes(x = logexp, y = lwage)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Log Wage vs Log Experience") +
  theme_minimal()

# Histograms
hist(data$wage, main = "Histogram of Wage", xlab = "Wage")
hist(data$ttl_exp, main = "Histogram of Total Experience", xlab = "Total Experience")

# Lowess: log wage vs total experience
ggplot(data, aes(x = ttl_exp, y = lwage)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Log Wage vs Total Experience (Log)") +
  theme_minimal()

# Tenure analysis
hist(data$tenure, main = "Histogram of Tenure", xlab = "Tenure")

# Create log tenure variable
data$ltenure <- log(1 + data$tenure)

# Lowess: log wage vs log tenure
ggplot(data, aes(x = ltenure, y = lwage)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Log Wage vs Log Tenure") +
  theme_minimal()

# Lowess: log wage vs tenure (level)
ggplot(data, aes(x = tenure, y = lwage)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Log Wage vs Tenure (Level)") +
  theme_minimal()

# Hours analysis
hist(data$hours, main = "Histogram of Hours", xlab = "Hours")

# Lowess: log wage vs hours
ggplot(data, aes(x = hours, y = lwage)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Log Wage vs Hours") +
  theme_minimal()

# Regression models
model1 <- lm(lwage ~ ttl_exp, data = data)
model2 <- lm(lwage ~ tenure, data = data)
model3 <- lm(lwage ~ hours, data = data)
model4 <- lm(lwage ~ ttl_exp + tenure, data = data)
model5 <- lm(lwage ~ ttl_exp + tenure + hours, data = data)

# Display results
stargazer(model1, model2, model3, model4, model5, type = "text",
          keep.stat = c("n", "rsq", "adj.rsq"))

# Regressions with categorical variables
# Convert to factors if not already
data$collgrad <- factor(data$collgrad)
data$married <- factor(data$married)
data$race <- factor(data$race)

model6 <- lm(lwage ~ collgrad, data = data)
summary(model6)

model7 <- lm(lwage ~ married, data = data)
summary(model7)

model8 <- lm(lwage ~ collgrad + ttl_exp + tenure + hours, data = data)
summary(model8)

model9 <- lm(lwage ~ married + ttl_exp + tenure + hours, data = data)
summary(model9)

# Check race variable
table(data$race)
summary(data$race)

model10 <- lm(lwage ~ race, data = data)
summary(model10)

model11 <- lm(lwage ~ race + ttl_exp + tenure + hours, data = data)
summary(model11)

# Check additional categorical variables
table(data$south)
table(data$smsa)
table(data$c_city)
table(data$industry)
table(data$occupation)

# Convert additional variables to factors
data$south <- factor(data$south)
data$smsa <- factor(data$smsa)
data$c_city <- factor(data$c_city)
data$industry <- factor(data$industry)
data$occupation <- factor(data$occupation)

# Full model with all controls
model12 <- lm(lwage ~ race + collgrad + ttl_exp + tenure + hours + 
              south + smsa + c_city + industry + occupation, data = data)
summary(model12)

# Model with interaction between race and college graduate
model13 <- lm(lwage ~ race * collgrad + ttl_exp + tenure + hours + 
              south + smsa + c_city + industry + occupation, data = data)
summary(model13)
