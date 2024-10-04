rm(list = ls())

source("ZDP.R")
source("HMC.R")

coefs <- c(-1.8, 0.5, -0.5, -0.3, 4, -0.3)
tam <- 200
y <- serie(coefs, tam)

hist(y)
plot.ts(y)
table(y)/tam
acf(y)

current_q <- c(0 ,0 ,0 ,0 ,0 ,0)
iteracoes <- 20000
chain <- matrix(nrow = (iteracoes + 1), ncol = length(coefs))
chain[1,] <- current_q
epsilon <- 0.00000001
L <- 15
c <- 0.1
s2 <- 200
medias <- c(0, 0, 0, 0, 0, 0)

for (i in 1:iteracoes-1) {
  chain[i+1,] <- HMC(U, grad_U, epsilon, L, chain[i,])
}
plot(chain[,1])
