rm(list = ls())

source("ZDP.R")
source("HMC.R")
source("aux.R")
source("conv.R")

coefs <- c(-1.8, 0.5, -0.5, -0.3, 4, -0.3)
tam <- 500
y <- serie(coefs, tam)
# table(y)
current_q <- c(0, 0, 0, 0, 0, 0)
iteracoes <- 20000
chain <- matrix(nrow = (iteracoes + 1), ncol = length(coefs))
chain[1,] <- current_q
epsilon <- 0.0001
L <- 20
c <- 0.1
s2 <- 200
medias <- c(0, 0, 0, 0, 0, 0)

for (i in 1:iteracoes) {
  chain[i+1,] <- HMC(U, grad_U, epsilon, L, chain[i,])
  print(i)
}
plot(chain[,1])

n_accepted <- sum(diff(chain[,1]) != 0)
acceptance_rate <- n_accepted / (iteracoes + 1)
