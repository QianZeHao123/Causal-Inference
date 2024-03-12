# adapted from https://edrub.in/ARE212/section11.html#the_two_stages

# all input data as matrices!
b_2sls <- function(y, X, Z, intercept = T) {
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Add intercept
  if (intercept == T)
    X <- cbind(1, X)
  if (intercept == T)
    Z <- cbind(1, Z)
  # Calculate P_Z
  P_Z <- Z %*% solve(t(Z) %*% Z) %*% t(Z)
  # Calculate b_2sls
  b <- solve(t(X) %*% P_Z %*% X) %*% t(X) %*% P_Z %*% y
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n - k)
  s2 <- as.numeric(s2)
  # Inverse of X' Pz X
  XX_inv <- solve(t(X) %*% P_Z %*% X)
  # Standard error
  se <- sqrt(s2 * diag(XX_inv))
  # Vector of _t_ statistics
  t_stats <- (b - 0) / se
  # Calculate the p-values
  p_values = pt(q = abs(t_stats),
                df = n - k,
                lower.tail = F) * 2
  # Update names
  # if (intercept == T) rownames(b)[1] <- "Intercept"
  # Nice table (data.frame) of results
  results <- data.frame(
    # The rows have the coef. names
    #   effect = rownames(b),
    # Estimated coefficients
    coef = as.vector(b),
    # Standard errors
    std_error = as.vector(se),
    # t statistics
    t_stat = as.vector(t_stats),
    # p-values
    p_value = as.vector(p_values)
  )
  # Return the results
  return(results)
}
