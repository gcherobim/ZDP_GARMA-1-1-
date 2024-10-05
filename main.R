rm(list = ls())

source("ZDP.R")
source("HMC.R")

coefs <- c(-1.8, 0.5, -0.5, -0.3, 4, -0.3)
tam <- 200
y <- serie(coefs, tam)

# mut <- cumsum(y) / seq_along(y)
# 
# t1 <- tam-1
# # t2 = n-2
# Y <- y[3:tam]
# Mt <- mut[3:tam]
# yt1 <- y[2:t1]
# # yt2 = log(yt[1:t2])
# ymt1 <- log(y[2:t1]) - log(mut[2:t1])
# # ymt2 = log(yt[1:t2])
# t <- 3:tam
# 
# fit = glm(Mt ~ log(t) + yt1 + ymt1, family = 'poisson')
# summary(fit)

current_q <- c(0 ,0 ,0 ,0 ,0 ,0)
iteracoes <- 20000
chain <- matrix(nrow = (iteracoes + 1), ncol = length(coefs))
chain[1,] <- current_q
epsilon <- 0.001
L <- 20
c <- 0.1
s2 <- 200
medias <- c(0, 0, 0, 0, 0, 0)

for (i in 1:iteracoes) {
  chain[i+1,] <- HMC(U, grad_U, epsilon, L, chain[i,])
  print(i)
}
plot(chain[,1])
