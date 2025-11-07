# lnskew0
r adaptation/replica of lnskew0 Stata function

This is an R function to replicate the Stata lnskew0 command on R.
 package "e1071" is required. Alternatively, can use package "moments"
Usage requires inputting the variable with data and it automatically finds the optimal value k and sign such that: skewness of newvar is minimized:
 newvar = ln(±exp − 𝑘), 
    ###choosing 𝑘 and the sign of exp so that the skewness of  newvar is close to zero

Input: variable to find optimal k from, and optional tolerance (default is set to 0.001)

Output is alist containing:
      sign = 1 or -1 (positive or negative
      k = optimal k
      transformation = a string with the function: ln(±exp − 𝑘),  
      original_skewness 
      transformed_skewness = final_skew,
      converged = TRUE or FALSE
      tolerance = tol,
      transformed = variable transformed,
      formula = function to apply the transformation to any other variable

  example:

data <- read_dta("WDI_data.dta")
clnm <- colnames(data)
lnskew0(data[[clnm[3]]])
lnskew0(data[[clnm[3]]], 0.001)
skewness(data[[clnm[3]]],na.rm=TRUE)
skewness(log(-data[[clnm[3]]]+91.66195),na.rm=TRUE)
sum(is.na(data[[clnm[3]]]))

tt1 <- lnskew0(data[["co2emissions"]])
tt1

clnm
#co2 gdp pop maternalmort hosp phys nurs
for(i in c(4,5,7,8,9,10,11)){
  tti <- lnskew0(data[[clnm[i]]])
  cat("\n variable = ", clnm[i], " ")
  cat("\n k = ", tti$k)
  cat("        new_skewness = ", tti$transformed_skewness)

}
