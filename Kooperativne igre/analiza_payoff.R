library(rgl)
library(hop)
library(invgamma)
library(matlib)
library(GameTheory)
library(ggplot2)
library(dplyr)
library(retistruct)

#funkcija ki ponovi (ponovitev)- krat igro in izracuna sporazum in povem vrne povprecje vseh ponovitev
povprecje_enofazna <- function(ponovitev, por1, por2, n, m){
  i <- 1
  vec_P1 <- c()
  vec_P2 <- c()
  while (i <= ponovitev){
    igra <- enofazno_pogajanje(matrika(por1, n,m), matrika(por2, n,m))
    vec_P1 <- append(vec_P1, igra[1])
    vec_P2 <- append(vec_P2, igra[2])
    i <- i+ 1
    
  }
  return(c(round(mean(vec_P1),3),round(mean(vec_P2),3)))
}

povprecje_dvofazne <- function(ponovitev, por1,por2, n, m){
  i <- 1
  vec_P1 <- c()
  vec_P2 <- c()
  while (i <= ponovitev){
    igra <- dvofazno_pogajanje(matrika(por1, n,m), matrika(por2, n,m))
    vec_P1 <- append(vec_P1, igra[1])
    vec_P2 <- append(vec_P2, igra[2])
    i <- i+ 1
    
  }
  return(round(c(mean(vec_P1),3), round(mean(vec_P2),3)))
}


#dobimo matriko v kateri so payoffi obeh igralcev vse do velikosti n *m, glede na izbrano porazdelitev za enofazno igro 
enofazna_enaka_por <- function(por1, por2, n, m){ 
  payoff_matrika <- as.data.frame(matrix(NA, nrow =(n-1) *(m - 1)*2, ncol = 3))
  stevec <- 1
  i <- 2
  while (i <= n){
    j <-2
    while (j <= m){
      izracun <- povprecje_enofazna(50, por1, por2, n, m)
      payoff_matrika[stevec,] <- cbind(i * j,'igralec1', izracun[1])
      payoff_matrika[stevec + 1,] <- cbind(i * j, 'igralec2', izracun[2])
      
      j <- j+1
      stevec <- stevec +2
    }
    i <- i +1
  }
  colnames(payoff_matrika)<- c('velikost_matrike', 'igralec', 'Vrednost')
  return(payoff_matrika)
}

#več kot matrika 10x10 se v zgornji funkciji ne vidi nič več, zato bom naredila drugače za vecje matrike
enofazna_enaka_por_big_n <- function(por1, por2, n, m){ 
  payoff_matrika <- as.data.frame(matrix(NA, nrow =(n-1) *(m-1), ncol = 3))
  stevec <- 1
  for (i in 2:n){
    j <-2
    while (j <= m){
      izracun <- povprecje_enofazna(50, por1, por2, i, j)
      payoff_matrika[stevec,] <- cbind(i*j, izracun[1], izracun[2])
      stevec <- stevec +1
      j <- j+1
    }
  }
  colnames(payoff_matrika)<- c('velikost_matrike', 'igralec1', 'igralec2')
  return(payoff_matrika)
  
}

#dvofazna za velikosti matrike do 10x10 
dvofazna_enaka_por <- function(por1, por2, n, m){ 
  payoff_matrika <- as.data.frame(matrix(NA, nrow =(n-1) *(m - 1)*2, ncol = 3))
  stevec <- 1
  i <- 2
  while (i <= n){
    j <-2
    while (j <= m){
      izracun <-  dvofazno_pogajanje(matrika(por1, n,m), matrika(por2, n, m))
      payoff_matrika[stevec,] <- cbind(i * j,'igralec1', izracun[1])
      payoff_matrika[stevec + 1,] <- cbind(i * j, 'igralec2', izracun[2])
      
      j <- j+1
      stevec <- stevec +2
    }
    i <- i +1
  }
  colnames(payoff_matrika)<- c('velikost_matrike', 'igralec', 'Vrednost')
  return(payoff_matrika)
}


#dobimo matriko v kateri so payoffi obeh igralcev vse do velikosti n *m, glede na izbrani porazdelitvi, za dvofazno igro 
dvofazna_enaka_por_big_n <- function(por1,por2, n, m){ 
  payoff_matrika <- as.data.frame(matrix(NA, nrow =(n-2) *(m-2), ncol = 3))
  stevec <- 1
  for (i in 3:n){
    j <-3
    while (j <= m){
      izracun <- povprecje_dvofazne(50, por1, por2, n, m)
      payoff_matrika[stevec,] <- cbind(i * j, izracun[1], izracun[2])
      j <- j+1
      stevec <- stevec +1
    }
  }
  colnames(payoff_matrika)<- c('velikost_matrike', 'igralec1', 'igralec2')
  return(payoff_matrika)
  
}


# matrika, kjer so izbrane porazdelitve za izbran n in m --> potrebovali za primerjanje

enofazna_vec_porazdelitev <- function(por0,por1=0, por2 = 0, por3=0, por4 =0, n, m ){
  vek <- c(por1, por2, por3, por4)
  payoff_mat <- as.data.frame(matrix(NA, nrow = sum(vek != 0)*2, ncol = 3))
  j <- 1
  for (i in vek){
    if (i != 0){
      v1 <- povprecje_enofazna(50,por0,  i, n, m)
      payoff_mat[j,] <- cbind(i, 'P1', v1[1])
      payoff_mat[j+1,] <- cbind(i, 'P2', v1[2])
      
      j <- j+2
    }
  }
  
  colnames(payoff_mat)<-c('Porazdelitve', 'igralec', 'vrednosti')
  
  return(payoff_mat)
  
}

dvofazna_vec_porazdelitev <- function(por0, por1 = 0, por2 = 0, por3= 0, por4= 0, n, m){
  vek <- c(por1, por2, por3, por4)
  payoff_mat <- as.data.frame(matrix(NA, nrow = sum(vek != 0)*2, ncol = 3))
  j <- 1
  for (i in vek){
    if (i != 0){
      v1 <- povprecje_dvofazne(50, por0, i, n, m)
      payoff_mat[j,] <- cbind(i,'P1', v1[1])
      payoff_mat[j+1,] <- cbind(i,'P2', v1[2])
      j <- j+2
    }
  }
  
  colnames(payoff_mat)<- c('Porazdelitve', 'igralec', 'vrednosti')
  
  return(payoff_mat)
}

#branje iz RDS za vsako porazdlitev posebaj

##za dvofazno pogajanje

izbrana_porazdelitev <- function(por1, por2, n, m){
  if (por1 == 'exp'){
    if (por2 == 'exp'){
      uvoz <- readRDS('../Kooperativne igre/Data/exp_exp_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'norm'){
      uvoz <- readRDS('../Kooperativne igre/Data/exp_norm_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'beta'){
      uvoz <- readRDS('../Kooperativne igre/Data/exp_beta_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'invgama'){
      uvoz <- readRDS('../Kooperativne igre/Data/exp_invgama_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }
  }
  
  else if (por1 == 'norm'){
    if (por2 == 'exp'){
      uvoz <- readRDS('../Kooperativne igre/Data/norm_exp_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'norm'){
      uvoz <- readRDS('../Kooperativne igre/Data/norm_norm_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'beta'){
      uvoz <- readRDS('../Kooperativne igre/Data/norm_beta_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'invgama'){
      uvoz <- readRDS('../Kooperativne igre/Data/norm_invgama_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }
  }
  
  else if (por1 == 'beta'){
    if (por2 == 'exp'){
      uvoz <- readRDS('../Kooperativne igre/Data/beta_exp_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'norm'){
      uvoz <- readRDS('../Kooperativne igre/Data/beta_norm_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'beta'){
      uvoz <- readRDS('../Kooperativne igre/Data/beta_beta_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'invgama'){
      uvoz <- readRDS('../Kooperativne igre/Data/beta_invgama_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }
  }
  
  else if (por1 == 'invgama'){
    if (por2 == 'exp'){
      uvoz <- readRDS('../Kooperativne igre/Data/invgama_exp_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'norm'){
      uvoz <- readRDS('../Kooperativne igre/Data/invgama_norm_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'beta'){
      uvoz <- readRDS('../Kooperativne igre/Data/invgama_beta_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'invgama'){
      uvoz <- readRDS('../Kooperativne igre/Data/invgama_invgama_2.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }
  }
  return(dat)
}

##za enofazno pogajanje

izbrana_porazdelitev_1 <- function(por1, por2, n, m){
  if (por1 == 'exp'){
    if (por2 == 'exp'){
      uvoz <- readRDS('../Kooperativne igre/Data/exp_exp_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'norm'){
      uvoz <- readRDS('../Kooperativne igre/Data/exp_norm_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'beta'){
      uvoz <- readRDS('../Kooperativne igre/Data/exp_beta_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'invgama'){
      uvoz <- readRDS('../Kooperativne igre/Data/exp_invgama_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }
  }
  
  else if (por1 == 'norm'){
    if (por2 == 'exp'){
      uvoz <- readRDS('../Kooperativne igre/Data/norm_exp_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'norm'){
      uvoz <- readRDS('../Kooperativne igre/Data/norm_norm_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'beta'){
      uvoz <- readRDS('../Kooperativne igre/Data/norm_beta_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'invgama'){
      uvoz <- readRDS('../Kooperativne igre/Data/norm_invgama_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }
  }
  
  else if (por1 == 'beta'){
    if (por2 == 'exp'){
      uvoz <- readRDS('../Kooperativne igre/Data/beta_exp_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'norm'){
      uvoz <- readRDS('../Kooperativne igre/Data/beta_norm_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'beta'){
      uvoz <- readRDS('../Kooperativne igre/Data/beta_beta_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'invgama'){
      uvoz <- readRDS('../Kooperativne igre/Data/beta_invgama_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }
  }
  
  else if (por1 == 'invgama'){
    if (por2 == 'exp'){
      uvoz <- readRDS('../Kooperativne igre/Data/invgama_exp_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'norm'){
      uvoz <- readRDS('../Kooperativne igre/Data/invgama_norm_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'beta'){
      uvoz <- readRDS('../Kooperativne igre/Data/invgama_beta_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }else if (por2 == 'invgama'){
      uvoz <- readRDS('../Kooperativne igre/Data/invgama_invgama_1.RDS')
      dat <- head(uvoz, (n-1) * (m-1))
    }
  }
  return(dat)
}


#graf za grafično ponazoritev pogajanja 
##enofazna 3D graf

graf_enofazna <- function(por1, por2, n, m){
  l <- matrika(por1, n,m)
  k <- matrika(por2,n,m)
  Z <- matrix(c(l, k), ncol=2)
  opt <- max(l+k)
  plot(l,k, cex = 0.5, xlim= c(min(Z[,1]-2),max(Z[,1] +2)), ylim=c(min(Z[,2])-2,max(Z[,2])+2),type="n", xlab = 'Izplacila prvega igralca'
       , ylab = 'Izplacila drugega igralca')
  hpts <- chull(Z)
  hpts <- c(hpts, hpts[1])
  lines(Z[hpts, ])
  polygon(Z[hpts, ], col='cornsilk')
  points(Z, col = "black", pch=19)
  abline(0,1, col = "grey", lty = 5)
  f <- function(x) opt - x 
  a <- seq(-20, 20, 0.1)
  points(0,0,col = "grey", pch=19)
  text(0+.2, 0, "SQ", cex=1, col='grey')
  lines(a, f(a), lwd= 1, col = 'blue')
  nbs <- enofazno_pogajanje(l,k)
  points(nbs[1], nbs[2], pch = 18, col = 'orange', cex = 1)
  text(nbs[1]+.2, nbs[2], "NBS", cex=1.1, col='orange')
  title(paste("Nashev sporazum: (",
              round(nbs[1],2),
              ",",
              round(nbs[2],2),
              ")", sep = ""
  ))
}

##dvofazna 3D graf
graf_dvofazna <- function(por1, por2, n, m){
  
  #izračun status quo tocke 
  A <- matrika(por1, n, m)
  B <- matrika(por2, n, m)
  vek_q <- minmax_q(A,B)
  q <- vek_q[1:(length(vek_q)-2)]
  vek_p <- minmax_p(A,B)
  p <- vek_p[1:(length(vek_p)-2)]
  tocka_groznje_1 <- t(p) %*% A %*% q
  tocka_groznje_2 <- t(p) %*% B %*% q
  SQ <- c(tocka_groznje_1, tocka_groznje_2)

  #graf
  nbs <- dvofazno_pogajanje(A,B)
  Z <- matrix(c(A, B), ncol=2)
  opt <- max(A+B)
  plot(A,B, cex = 0.5, xlim= c(min(Z[,1]-0.5),nbs[1]+1), ylim=c(min(Z[,2])-0.5,nbs[2] +1),type="n", xlab = 'Izplacila prvega igralca'
       , ylab = 'Izplacila drugega igralca')
  hpts <- chull(Z)
  hpts <- c(hpts, hpts[1])
  lines(Z[hpts, ])
  polygon(Z[hpts, ], col='cornsilk')
  points(Z, col = "black", pch=19)
  f <- function(x) opt - x
  h <- function(x) x - (SQ[1]-SQ[2])
  a <- seq(-50, 50, 0.1)
  points(SQ[1],SQ[2],col = "grey", pch=19)
  text(SQ[1]+.09, SQ[2], "SQ", cex=1, col='grey')
  lines(a, f(a), lwd= 1, col = 'blue')
  lines(a, h(a))

  points(nbs[1], nbs[2], pch = 18, col = 'orange', cex = 1)
  text(nbs[1]+.1, nbs[2], "NBS", cex=1.1, col='orange')
  title(paste("Nashev sporazum: (",
              round(nbs[1],2),
              ",",
              round(nbs[2],2),
              ")", sep = ""
  ))
}

