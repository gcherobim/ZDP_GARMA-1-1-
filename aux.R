graficos <- function(chain) {
  par(mfrow=c(ncol(chain)/2, 2))
  for (i in 1:ncol(chain)) {
    plot(chain[,i], main = paste("Parâmetro ", i), xlab = "", ylab = "")
  }
}

correlacao <- function(chain) {
  par(mfrow=c(ncol(chain)/2, 2))
  for (i in 1:ncol(chain)) {
    acf(chain[,i], main = paste("Parâmetro ", i), xlab = "", ylab = "")
  }
}

sumario <- function(medias, medianas, desvios, coef) {
  media <- rbind(mean(medias[,1]), mean(medias[,2]), mean(medias[,3]), 
                mean(medias[,4]), mean(medias[,5]), mean(medias[,6]))
  mediana <- rbind(mean(medianas[,1]), mean(medianas[,2]), mean(medianas[,3]),
                  mean(medianas[,4]), mean(medianas[,5]), mean(medianas[,6]))
  desvio <- rbind(mean(desvios[,1]), mean(desvios[,2]), mean(desvios[,3]), 
                 mean(desvios[,4]), mean(desvios[,5]), mean(desvios[,6]))
  
  IC1 <- c(media[1] - 1.96 * sd(medias[,1]) / sqrt(w), media[1] + 1.96 * sd(medias[,1]) / sqrt(w))
  IC2 <- c(media[2] - 1.96 * sd(medias[,2]) / sqrt(w), media[2] + 1.96 * sd(medias[,2]) / sqrt(w))
  IC3 <- c(media[3] - 1.96 * sd(medias[,3]) / sqrt(w), media[3] + 1.96 * sd(medias[,3]) / sqrt(w))
  IC4 <- c(media[4] - 1.96 * sd(medias[,4]) / sqrt(w), media[4] + 1.96 * sd(medias[,4]) / sqrt(w))
  IC5 <- c(media[5] - 1.96 * sd(medias[,5]) / sqrt(w), media[5] + 1.96 * sd(medias[,5]) / sqrt(w))
  IC6 <- c(media[6] - 1.96 * sd(medias[,6]) / sqrt(w), media[6] + 1.96 * sd(medias[,6]) / sqrt(w))
  
  IC <- rbind(IC1, IC2, IC3, IC4, IC5, IC6)
  
  CE <- rbind(1 / (w * sd(medias[,1])^2) * sum((medias[,1] - coef[1])^2),
              1 / (w * sd(medias[,2])^2) * sum((medias[,2] - coef[2])^2),
              1 / (w * sd(medias[,3])^2) * sum((medias[,3] - coef[3])^2),
              1 / (w * sd(medias[,4])^2) * sum((medias[,4] - coef[4])^2),
              1 / (w * sd(medias[,5])^2) * sum((medias[,5] - coef[5])^2),
              1 / (w * sd(medias[,6])^2) * sum((medias[,6] - coef[6])^2))
  
  CB <- rbind(1 / w * sum(abs((medias[,1] - coef[1]) / coef[1])),
              1 / w * sum(abs((medias[,2] - coef[2]) / coef[2])),
              1 / w * sum(abs((medias[,3] - coef[3]) / coef[3])),
              1 / w * sum(abs((medias[,4] - coef[4]) / coef[4])),
              1 / w * sum(abs((medias[,5] - coef[5]) / coef[5])),
              1 / w * sum(abs((medias[,6] - coef[6]) / coef[6])))
  
  linhas <- rbind("beta0", "beta1", "phi1", "theta1", "gama", "lambda")
  colunas <- cbind("média", "mediana", "desvio", "Li", "Ls", "CE", "CB")
  
  sumario <- round(data.frame(media, mediana, desvio, IC, sqrt(CE), CB), 5)
  colnames(sumario) <- c("média", "mediana", "desvio", "Li", "Ls", "CE", "CB")
  rownames(sumario) <- c("beta0", "beta1", "phi1", "theta1", "gama", "lambda")
  
  return(sumario)
}