rm(list = ls())

source("ZDP.R")
source("HMC.R")

coefs <- c(-1.8, 0.5, -0.5, -0.3, 4, -0.3)
tam <- 1000
y <- serie(coefs, tam)

hist(y[501:1000])
plot.ts(y[501:1000])
table(y[501:1000])/tam
acf(y[501:1000])