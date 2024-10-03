rm(list = ls())

ident <- function(x) {
  if (x == c) {
    return(1)
  } else {
    return(0)
  }
}

ZDP <- function(coefs, mu, y, eta, x) {
  return((1 - wt(coefs, mu, y, i)) * ident(x) 
         + wt(coefs, mu, y, i) * exp(-exp(eta(coefs, mu, y, i))) 
         * exp(eta(coefs, mu, y, i))^x / 
           (factorial(x) * (1 - exp(-exp(eta(coefs, mu, y, i))))) 
         * (1 - ident(x)))
}

rZDP <- function(coefs, mu, y, i) {
  w <- wt(coefs, mu, y, i)
  I <- rbinom(1, 1, prob = w)
  if (I == 0) {
    x <- 0.1
  } else {
    repeat {
      x <- rpois(1, lambda = mu[i])
      if (x > 0) {
        break
      }
    }
  }
  return(x)
}

wt <- function(coefs, mu, y, i) {
  return(exp(coefs[5] + coefs[6] * eta(coefs, mu, y, i-1)) / 
           (1 + exp(coefs[5] + coefs[6] * eta(coefs, mu, y, i-1))))
}

eta <- function(coefs, mu, y, i) {
  return(coefs[1] + coefs[2] * log(i) + coefs[3] * log(y[i-1]) 
         + coefs[4] * log(y[i-1]/mu[i-1]))
}

## Poisson Deflacionada

serie <- function(coefs, tam) {
  mu <- NULL
  mu[1] <- sample(0:15, 1)
  mu[2] <- sample(0:15, 1)
  
  c <- 0.1
  y <- NULL
  y[1] <- max(c, rpois(1, mu[1]))
  y[2] <- max(c, rpois(1, mu[1]))
  
  for (i in 3:tam) {
    mu[i] <- exp(eta(coefs, mu, y, i))
    # i = 3
    y[i] <- rZDP(coefs, mu, y, i)
  }
  return(y)
}

coefs <- c(-1.8, 0.5, -0.5, -0.3, 4, -0.3)
tam <- 1000
y <- serie(coefs, tam)

hist(y[501:1000])
plot.ts(y[501:1000])
table(y[501:1000])/tam
acf(y[501:1000])