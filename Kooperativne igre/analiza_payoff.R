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
  return(c(mean(vec_P1), mean(vec_P2)))
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
  return(c(mean(vec_P1), mean(vec_P2)))
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
  #return(ggplot(data = payoff_matrika, aes(igralec1, igralec2))+ geom_line())
  return(payoff_matrika)
}

#več kot matrika 10x10 se v zgornji funkciji ne vidi nič več, zato bom naredila drugače za vecje matrike
enofazna_enaka_por_big_n <- function(por1, por2, n, m){ 
  payoff_matrika <- as.data.frame(matrix(NA, nrow =(n-1) *(m-1), ncol = 3))
  stevec <- 1
  for (i in 2:m){
      izracun <- povprecje_enofazna(50, por1, por2, n, i)
      payoff_matrika[stevec,] <- cbind(i, izracun[1], izracun[2])
      stevec <- stevec +1
  }
  colnames(payoff_matrika)<- c('velikost_matrike', 'igralec1', 'igralec2')
  return(payoff_matrika)
  
}


#dobimo matriko v kateri so payoffi obeh igralcev vse do velikosti n *m, glede na izbrani porazdelitvi, za dvofazno igro 
dvofazna_enaka_por <- function(por1,por2, n, m){ 
  payoff_matrika <- as.data.frame(matrix(NA, nrow =(n-1) *(m-1), ncol = 3))
  stevec <- 1
  for (i in 2:n){
    j <-2
    while (j <= m){
      izracun <- povprecje_dvofazne(50, por1, por2, i, j)
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




