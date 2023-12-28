# function to generate an EAR(1) process
ear1 <- function(a, lambda, n){
  
  # initialize vector + set first term to a random expo variable
  X = numeric(n)
  X[1] = rexp(1, rate = lambda)
  
  # generate EAR(1) process based on definition and store values in X
  for (i in 2:n){
    if (runif(1) <= a){
      X[i] = a * X[i-1]
    } else {
      X[i] = a * X[i-1] + rexp(1, rate = lambda)
    }
  }
  return(X)
} 

# function to generate vector of Cov(X0,X1) results by iteration
covEarVec <- function(a, lambda, n){
  
  # initialize vectors
  cov_array = numeric(n)
  X0 <- numeric(n)
  X1 <- numeric(n)
  
  # generate X0 and a random uniform to determine how to calc X1
  for (i in 1:n){
    x0 = rexp(1, rate = lambda)
    rn <- runif(1)
    
    # generate X1
    if (rn <= a){
      x1 = a*x0
    } else {
      x1 = a*x0 + rexp(1)
    }
    
    # add new terms to vector
    X0[i] = x0
    X1[i] = x1
    
    # recalculate cov(X0,X1)
    cov_array[i] = cov(X0[1:i], X1[1:i])
  }
  
  return (cov_array)
}