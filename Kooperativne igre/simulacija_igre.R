library(hop)
library(invgamma)
library(matlib)
library(GameTheory)


#generira matriko z razlicnimi porazdelitvai 
matrika <- function(porazdelitev, row, col){
  if(porazdelitev == 'norm'){
    return(matrix(rnorm(row * col, mean = 3, sd = 0.7), row, col))
  } 
  else if(porazdelitev == 'beta'){
    return(matrix(rbeta(row * col, 5, 1), row, col))
  }
  else if(porazdelitev == 'invgama'){
    return(matrix(rinvgamma(row * col, 2, 0.5), row, col))
  }
  else if(porazdelitev == 'exp'){
    return(matrix(rexp(row * col, 3), row, col))
  }
  
}

#izhajamo iz bimatricne igre, funkcija vrne payoff vrednosti

enofazna_igra <- function(G,length=1000,
                          xtop = 1){
  X = G # prvi igralec ima vrstice
  Y = t(G) #drugi igralec ima stoplce
  SQ <- c(gt_minimax(X)[[1]], gt_minimax(t(Y))[[1]])
  Z <- matrix(c(X, Y), ncol=2)
  Z <- t(t(Z) - SQ)
  opt <- max(rowSums(Z))
  xtop <- opt
  f <- function(x) x
  g <- function(x) opt - x
  xlim <-  c(SQ[1],xtop)
  ylim <- c(SQ[2],xtop)
  
  x 	<-  seq(0,xtop, length = length)
  x2 	<-  seq(0,max(xlim)*2, length = length)
  nsh	<-  x[sapply(x, function(i) f(i)*g(i))==max(sapply(x, function(i) f(i)*g(i)))]  # NASH Bargaining Solution
  if(length(nsh)>1) nsh <- sample(nsh,1)
  N	  <-  g(nsh)*f(nsh)
  
  payoff <- c(round(g(nsh),2), round(f(nsh),2))
  return(payoff)
}



matMax <- function(matrika)
{
  colmn <- which(matrika == max(matrika)) %/% nrow(matrika) + 1
  row <- which(matrika == max(matrika)) %% nrow(matrika)
  return( matrix(c(row, colmn), 1))
}

#dvofazna igra, zacetek v strateski, imamo 2 matriki koristnosti. Funkcija vrne payoff, obeh igralcev. gt

dvofazna_igra <- function(A, B){
  mat_igra = A-B 
  v <- gt_minimax(mat_igra)$`Minimax payoff`  
  q <- gt_minimax(mat_igra)$`Other's strategy`
  p <- gt_minimax(- t(mat_igra))$`Other's strategy`
  vsota <- A+B
  sigma <- max(vsota)
  payoff <- c( (sigma + v)/2 , (sigma - v)/2 )
  tocka_groznje_1 <- t(p) %*% A %*% q
  tocka_groznje_2 <- t(p) %*% B %*% q
  #indeks <- matMax(vsota)
  #A_i_j <- A[indeks[1]][indeks[2]]
  #P1toP2 <- A_i_j - payoff[1]
  return(c(payoff))

  
}

#funkcija ki ponovi (ponovitev)- krat igro in izracuna sporazum in povem vrne povprecje vseh ponovitev
povprecje_enofazna <- function(ponovitev, por, n, m){
  i <- 1
  vec_P1 <- c()
  vec_P2 <- c()
  while (i <= ponovitev){
    igra <- enofazna_igra(matrika(por, n,m))
    print(igra)
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
    igra <- dvofazna_igra(matrika(por1, n,m), matrika(por2, n,m))
    vec_P1 <- append(vec_P1, igra[1])
    vec_P2 <- append(vec_P2, igra[2])
    i <- i+ 1
    
  }
  return(c(mean(vec_P1), mean(vec_P2)))
}


