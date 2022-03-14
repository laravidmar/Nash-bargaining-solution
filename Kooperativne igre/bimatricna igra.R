library(hop)
library(invgamma)
library(matlib)
#strateska igra s koristnostmi v matirki A in B 

matrika <- function(velikost, porazdelitev){
  if(porazdelitev == 'norm'){
    return(matrix(rnorm(velikost^2, mean = 3, sd = 0.7), velikost, velikost))
  } 
  else if(porazdelitev == 'beta'){
    return(matrix(rbeta(velikost^2, 5, 1), velikost, velikost))
  }
  else if(porazdelitev == 'invgama'){
    return(matrix(rinvgamma(velikost^2, 2, 0.5), velikost, velikost))
  }
  else if(porazdelitev == 'exp'){
    return(matrix(rexp(velikost^2, 3), velikost, velikost))
  }
  
}

#spodej nisi upoštevala čistih nashevih ravnovesij
dvofazna_igra <- function(A, B){
  razlika = A-B 
  #mesana nashova ravnovesja
  Id <- matrix(rep(1, nrow(razlika)), nrow(razlika),1)
  inverz <- inv(razlika)
  if (sum(t(Id) %*% inverz > 0) == nrow(razlika) ){
    if (sum(inverz %*% Id > 0) == nrow(razlika)){
      v <-as.vector( 1 /(t(Id) %*% inverz %*% Id))
      pT <- t(Id) %*% inverz * v 
      q <- v * inverz %*% Id
    }
  }else (return('Pogoji niso izpolnjeni'))
  
  tocka_groznje_1 <- pT %*% A %*% q
  tocka_groznje_2 <- pT %*% B %*% q
  vsota <- A+B
  sigma <- max(vsota)
  payoff <- c( (sigma + tocka_groznje_1 - tocka_groznje_2)/2 , (sigma - tocka_groznje_1 + tocka_groznje_2)/2 )
  return(c(v, payoff))
}
