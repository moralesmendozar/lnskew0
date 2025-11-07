## lnskew0 equivalent
# function ln(±exp − 𝑘),

lnskew0<- function(exp,tol = 0.001){
  # Function based on Stata's lnksew0()
  # This function creates
      # newvar = ln(±exp − 𝑘), 
    ###choosing 𝑘 and the sign of exp so that the skewness of
  ## newvar is close to zero
  require(e1071)
  x <- exp[!is.na(exp)]
  
  # Function to calculate skewness
  calc_skewness <- function(vals) {
    skewness(vals, na.rm = TRUE)
  }
  
  # Transformation function: ln(sign * x - k)
  # sign: +1 or -1
  # k: shift parameter
  transform_ln <- function(x, sign, k) {
    transformed_vals <- sign * x - k
    
    # Check if all values are positive (required for log)
    if (any(transformed_vals <= 0)) {
      return(NA)
    }else{
      # return log(transf_vals)
      return(log(transformed_vals))
    }
  }
  
  # Objective function to minimize
  objective <- function(params) {
    sign <- params[1]
    k <- params[2]
    
    transformed <- transform_ln(x, sign, k)
    
    # Return large penalty if transformation is invalid
    if (any(is.na(transformed))) {
      return(1e10)
    }
    
    # Return absolute skewness
    #abs(calc_skewness(transformed))
    #calc_skewness(transformed)
    calc_skewness(transformed)^2
  }
  
  skk <- calc_skewness(x)
  if( abs(skk) > tol){
    #x <- exp
    # Try both signs
    results <- list()
    
    for (sign_val in c(1, -1)) {
      # Set bounds for k based on sign
      if (sign_val == 1) {
        k_lower <- -max(x) * 2
        k_upper <- min(x) - 1e-6
      } else {
        k_lower <- -min(x) + 1e-6
        k_upper <- max(x) * 2
      }
      
      k_lower <- -2* abs(max(x))
      k_upper <- abs(min(x))-1e-6
      
      #cat("\n sign_val = ",sign_val)
      #cat("\n k_lower  = ", k_lower, "  and k_upper = ", k_upper, "\n")
      
      # Skip if bounds are invalid
      if (k_lower >= k_upper){
        print("sad: k_lower >= k_upper")
        next
      }
      
      # Optimize k for this sign
      tryCatch({
        opt_result <- optimize(
          f = function(k) objective(c(sign_val, k)),
          interval = c(k_lower, k_upper),
          tol = tol #/ 10
        )
        #cat("opt_result$minimum = ", opt_result$minimum)
        #cat(" skewness = opt_result$objective = ", opt_result$objective, "\n")
        
        results[[length(results) + 1]] <- list(
          sign = sign_val,
          k = opt_result$minimum,
          skewness = opt_result$objective
        )
      }, error = function(e) {
        print("Error in function")
        # Skip if optimization fails
      }, warning = function(w){print("A warning occurred")})
    }
    # Find best result
    if (length(results) == 0) {
      stop("No valid transformation found")
    }
    
    best_idx <- which.min(sapply(results, function(r) r$skewness))
    best <- results[[best_idx]]
    
    # Apply best transformation
    transformed <- transform_ln(x, best$sign, best$k)
    exp2 <- exp
    exp2[!is.na(exp)] <-transformed 
    final_skew <- calc_skewness(transformed)
    
    # Check if within tolerance
    converged <- abs(final_skew) < tol
    
    # Create result
    result <- list(
      sign = best$sign,
      k = best$k,
      transformation = sprintf("ln(%s%.6f)", 
                               ifelse(best$sign == 1, "x - ", "-x + "),
                               abs(best$k)),
      original_skewness = calc_skewness(x),
      transformed_skewness = final_skew,
      converged = converged,
      tolerance = tol,
      transformed = exp2,
      formula = function(new_x) {
        log(best$sign * new_x - best$k)
      })
    class(result) <- "lnskew0"
    return(result)
    
  }else{
    cat("The skewness of original expresion was: ",skk)
    cat("hence, no need to transform the variable")
    return(exp)
  }
  
}

# clnm <- colnames(data)
# lnskew0(data[[clnm[3]]])
# lnskew0(data[[clnm[3]]], 0.001)
# skewness(data[[clnm[3]]],na.rm=TRUE)
# skewness(log(-data[[clnm[3]]]+91.66195),na.rm=TRUE)
# sum(is.na(data[[clnm[3]]]))
# 
# tt1 <- lnskew0(data[["co2emissions"]])
# tt1
# 
# clnm
# #co2 gdp pop maternalmort hosp phys nurs
# for(i in c(4,5,7,8,9,10,11)){
#   tti <- lnskew0(data[[clnm[i]]])
#   cat("\n variable = ", clnm[i], " ")
#   cat("\n k = ", tti$k)
#   cat("        new_skewness = ", tti$transformed_skewness)
# 
# }










# ##This one is wrong or needs revision:
# lnskew0 <- function(x) {
#   x <- x[!is.na(x)]
#   
#   # Function to minimize (absolute skewness)
#   skew_obj <- function(lambda) {
#     if (lambda == 0) {
#       transformed <- log(x)
#     } else {
#       transformed <- (x^lambda - 1) / lambda
#     }
#     abs(skewness(transformed))
#   }
#   
#   # Find optimal lambda
#   result <- optimize(skew_obj, interval = c(-5, 5))
#   
#   lambda_opt <- result$minimum
#   
#   # Apply transformation
#   if (abs(lambda_opt) < 0.001) {
#     transformed <- log(x)
#   } else {
#     transformed <- (x^lambda_opt - 1) / lambda_opt
#   }
#   
#   return(list(
#     lambda = lambda_opt,
#     transformed = transformed,
#     original_skew = skewness(x),
#     transformed_skew = skewness(transformed)
#   ))
# }
# 
# # Use it
# result <- lnskew0(data[["co2emissions"]])
# 
# 
