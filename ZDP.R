## Função identificadora
ident <- function(x) {
  if (x == c) {
    return(1)
  } else {
    return(0)
  }
}

## Poisson Deflacionada

ZDP <- function(coefs, mu, y, eta, x) {
  return((1 - wt(coefs, mu, y, i)) * ident(x) 
         + wt(coefs, mu, y, i) * exp(-exp(eta(coefs, mu, y, i))) 
         * exp(eta(coefs, mu, y, i))^x / 
           (factorial(x) * (1 - exp(-exp(eta(coefs, mu, y, i))))) 
         * (1 - ident(x)))
}

rZDP <- function(coefs, mu, y, i) {
  w <- min(0.99, wt(coefs, mu, y, i))
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
  return(coefs[1] + coefs[2] * log(i) + coefs[3] * y[i-1] 
         + coefs[4] * log(y[i-1]/mu[i-1]))
}

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

## Funções do Hamiltoniano

U <- function(q) {
  mu <- NULL
  mu[1] <- mean(y)
  
  for (i in 2:length(y)) {
    mu[i] <- exp(eta(q, mu, y, i))
  }
  
  valor <- - 1/(2 * s2) * ((q[1] - medias[1])^2 + (q[2] - medias[2])^2 
                           + (q[3] - medias[3])^2 + (q[4] - medias[4])^2 
                           + (q[5] - medias[5])^2 + (q[6] - medias[6])^2)
  
  for (i in 3:length(y)) {
    valor = valor + ident(y[i]) * (q[5] + q[6] * eta(q, mu, y, i-1) 
                                   + log(wt(q, mu, y, i))) 
    + (1 - ident(y[i])) * (-exp(eta(q, mu, y, i)) 
                           + y[i] * eta(q, mu, y, i) 
                           - log(factorial(y[i]))
                           - log(1 - exp(-exp(eta(q, mu, y, i)))))
  }
  
  return(-valor)
}

l1gama <- function(q, mu, y) {
  valor <- - (q[5] - medias[5]) / s2
  
  for (i in 3:length(y)) {
    valor = valor + (1 - ident(y[i]) - wt(q, mu, y, i))
  }
  
  return(-valor)
}

l1lambda <- function(q, mu, y) {
  valor <- - (q[6] - medias[6]) / s2
  
  for (i in 3:length(y)) {
    valor = valor + (1 - ident(y[i])) + eta(q, mu, y, i-1) 
    - wt(q, mu, y, i) * eta(q, mu, y, i-1)
  }
  
  return(-valor)
}

l2beta0 <- function(q, mu, y) {
  valor <- - (q[1] - medias[1]) / s2
  
  for (i in 2:length(y)) {
    valor = valor + ((1 - ident(y[i])) * y[i] * 1 / exp(eta(q, mu, y, i)) 
  - exp(exp(eta(q, mu, y, i))) / (exp(exp(eta(q, mu, y, i))) - 1)) * exp(eta(q, mu, y, i))
  }
  
  return(-valor)
}

l2beta1 <- function(q, mu, y) {
  valor <- - (q[2] - medias[2]) / s2
  
  for (i in 2:length(y)) {
    valor = valor + ((1 - ident(y[i])) * y[i] * 1 / exp(eta(q, mu, y, i)) 
                     - exp(exp(eta(q, mu, y, i))) / (exp(exp(eta(q, mu, y, i))) - 1)) * log(i) * exp(eta(q, mu, y, i))
  }
  
  return(-valor)
}

l2phi1 <- function(q, mu, y) {
  valor <- - (q[3] - medias[3]) / s2
  
  for (i in 2:length(y)) {
    valor = valor + ((1 - ident(y[i])) * y[i] * 1 / exp(eta(q, mu, y, i)) 
                     - exp(exp(eta(q, mu, y, i))) / (exp(exp(eta(q, mu, y, i))) - 1)) * y[i-1] * exp(eta(q, mu, y, i))
  }
  
  return(-valor)
}

l2theta1 <- function(q, mu, y) {
  valor <- - (q[4] - medias[4]) / s2
  
  for (i in 2:length(y)) {
    valor = valor + ((1 - ident(y[i])) * y[i] * 1 / exp(eta(q, mu, y, i)) 
                     - exp(exp(eta(q, mu, y, i))) / (exp(exp(eta(q, mu, y, i))) - 1)) * log(y[i-1]/mu[i-1]) * exp(eta(q, mu, y, i))
  }
  
  return(-valor)
}

grad_U <- function(q) {
  mu <- NULL
  mu[1] <- mean(y)
  
  for (i in 2:length(y)) {
    mu[i] <- exp(eta(q, mu, y, i))
  }
  
  return(c(l2beta0(q, mu, y), 
         l2beta1(q, mu, y), 
         l2phi1(q, mu, y),
         l2theta1(q, mu, y),
         l1gama(q, mu, y),
         l1lambda(q, mu, y)))
}