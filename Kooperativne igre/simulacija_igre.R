library(hop)
library(invgamma)
library(matlib)
library(GameTheory)
library(lpSolve)
library(retistruct)


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

#Linearni program za maxmin strategijo za prvega in drugega igralca

minmax_p <- function(A, B){ 
  raz <- A-B
  min_element <- min(raz) + 1
  if (min_element <= 0){
    org <- min_element + raz
    
  }else {
  org <- raz
  }
  #original
  vrstice <- nrow(org)
  stolpci <- ncol(org)
  modmat <- rbind(org, rep(1, stolpci))
  f.con <- cbind(modmat, c(rep(1, vrstice), 0))
  f.obj <- c(rep(0, stolpci), 1)
  f.dir <- c(rep('<=', vrstice), '=')
  f.rhs <- c(rep(0, vrstice), 1)
  lin_prog <- lp ("max", f.obj, f.con, f.dir, f.rhs)
  
  return(lin_prog$solution)
  
}

minmax_q <- function(A, B){
  raz <- t(A-B) 
  min_element <- min(raz) + 1
  if (min_element <= 0){
    dual <- min_element + raz
  }else {
    dual <- raz
  }
  #dual 
  vrsticeD <- nrow(dual)
  stolpciD <- ncol(dual)
  modmatD <- rbind(dual, rep(1, stolpciD))
  d.con <- cbind(modmatD, c(rep(1, vrsticeD ), 0))
  d.obj <- c(rep(0, stolpciD), 1)
  d.dir <- c(rep('>=', vrsticeD), '=')
  d.rhs <- c(rep(0, vrsticeD), 1)
  lin_prog_d <- lp ("min", d.obj, d.con, d.dir, d.rhs)
  return(lin_prog_d$solution)

}

#enofazno pogajanje status quo je vedno tocka (0,0) --> TO NI PRAVA FORMULA!!
enofazno_pogajanje <- function(A, B){
  SQ <- c(0, 0)
  Z <- A-B
  opt <- max(A+B)
  pos <- which(A+B == opt, arr.ind = TRUE)
  max_tocka <- c(A[pos[1],pos[2]], B[pos[1], pos[2]])
  xtop <- opt
  f <- function(x) x
  g <- function(x) opt - x
  sporazum <- line.line.intersection(c(0, f(0)), c(xtop, f(xtop)), max_tocka, c( 0, opt))
  return(round(sporazum,3))
}


#dvofazno pogajanje , status quo je tocka groznje, ki jo določimo s pomočjo maxmin strategije

dvofazno_pogajanje <- function(A, B){
  vek_q <- minmax_q(A,B)
  p <- vek_q[1:length(vek_q)-1]
  vek_p <- minmax_p(A,B)
  q <- vek_p[1:length(vek_p)-1]
  v_igre <- vek_p[length(vek_p)]
  print(p)
  print(q)
  tocka_groznje_1 <- t(p) %*% A %*% q
  tocka_groznje_2 <- t(p) %*% B %*% q
  SQ <- c(tocka_groznje_1, tocka_groznje_2)
  print(SQ)
 
  Z <- A-B
  opt <- max (A+B)
  g <- function(x) opt - x
  if (SQ[1] < SQ[2]){
    tocka2 <- c(0, SQ[2]-SQ[1])
  }else{
    tocka2 <- c(SQ[1]- SQ[2], 0)
  }
  sporazum <- line.line.intersection(c(0, g(0)), c(opt, g(opt)), c(SQ[1], SQ[2]), tocka2)
  print(sporazum)
  return(sporazum)
  
}


