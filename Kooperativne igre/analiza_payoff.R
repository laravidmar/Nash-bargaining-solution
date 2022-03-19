library(rgl)
library(hop)
library(invgamma)
library(matlib)
library(GameTheory)

#dobimo matriko v kateri so payoffi obeh igralcev vse do velikosti n *m, glede na izbrano porazdelitev za enofazno igro 
enofazna_enaka_por <- function(porazdelitev, n, m){ 
  payoff_matrika <- as.data.frame(matrix(NA, nrow =(n-1) *(m-1), ncol = 3))
  stevec <- 1
  for (i in 2:n){
    j <-2
    while (j <= m){
      izracun <- povprecje_enofazna(50, porazdelitev, n, m)
      payoff_matrika[stevec,] <- cbind(i * j, izracun[1], izracun[2])
      j <- j+1
      stevec <- stevec +1
    }
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


# matrika, kjer so izbrane porazdelitve za izbran n in m --> potrebvoali za primerjanje

enofazna_vec_porazdelitev <- function(por1=0, por2 = 0, por3=0, por4 =0, n, m ){
  vek <- c(por1, por2, por3, por4)
  payoff_mat <- as.data.frame(matrix(NA, nrow = sum(vek != 0), ncol = 3))
  j <- 1
  for (i in vek){
    if (i != 0){
      v1 <- povprecje_enofazna(50, i, n, m)
      payoff_mat[j,] <- cbind(i, v1[1], v1[2])
      j <- j+1
    }
    }
  
  colnames(payoff_mat)<- c('por', 'igralec1', 'igralec2')
  
  return(payoff_mat)
 
}

dvofazna_vec_porazdelitev <- function(por0, por1 = 0, por2 = 0, por3= 0, por4= 0, n, m){
  vek <- c(por1, por2, por3, por4)
  payoff_mat <- as.data.frame(matrix(NA, nrow = sum(vek != 0), ncol = 3))
  j <- 1
  for (i in vek){
    if (i != 0){
      v1 <- povprecje_dvofazne(50, por0, i, n, m)
      payoff_mat[j,] <- cbind(i, v1[1], v1[2])
      j <- j+1
    }
  }
  
  colnames(payoff_mat)<- c('por', 'igralec1', 'igralec2')
  
  return(payoff_mat)
}





