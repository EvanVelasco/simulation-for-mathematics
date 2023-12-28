set.seed(54241343)

covEar <- function(a, lambda, n){
  # Define vectors
  X0 <- numeric(n)
  X1 <- numeric(n)
  
  # Simulate n times
  for (i in 1:n){
    x0 = rexp(1, rate = lambda)
    rn <- runif(1)
    
    if (rn <= a){
      x1 = a*x0
    } else {
      x1 = a*x0 + rexp(1)
    }
    
    X0[i] = x0
    X1[i] = x1
  }
  # Return covariance
  return (cov(X0, X1))
}

# Call function
covEar(a = 0.5, lambda = 1, n = 1000000)