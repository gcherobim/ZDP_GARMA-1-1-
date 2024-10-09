library("coda")

geweke <- function(chain) {
  diag1 <- geweke.diag(chain[,1], frac1 = 0.1, frac2 = 0.5)
  diag2 <- geweke.diag(chain[,2], frac1 = 0.1, frac2 = 0.5)
  diag3 <- geweke.diag(chain[,3], frac1 = 0.1, frac2 = 0.5)
  diag4 <- geweke.diag(chain[,4], frac1 = 0.1, frac2 = 0.5)
  diag5 <- geweke.diag(chain[,5], frac1 = 0.1, frac2 = 0.5)
  diag6 <- geweke.diag(chain[,6], frac1 = 0.1, frac2 = 0.5)
  
  if (abs(as.numeric(diag1[[1]])) <= 2 && abs(as.numeric(diag2[[1]])) <= 2 
      && abs(as.numeric(diag3[[1]])) <= 2 && abs(as.numeric(diag4[[1]])) <= 2 
      && abs(as.numeric(diag5[[1]])) <= 2 && abs(as.numeric(diag6[[1]])) <= 2) {
    return(1)
  } else {
    return(0)
  }
}