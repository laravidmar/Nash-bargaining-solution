#plot za točno en n in m za vse 4 porazdelitve
#dvofazna vec porazdlitev 
#ggplot(data = k, aes(igralec1, igralec2)) + geom_point(size= 3)+geom_text(label = k$por, hjust = 0.5, vjust = -1)
ggplot(data = p, aes(igralec1, igralec2))+ geom_line()
ggplot(t) + 
  geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                color = igralec, fill = igralec ), 
            position = "dodge2", stat = "identity")+
  labs(y ='bruke')+
  ggtitle("Pristranskost ocenjenih parametrov", 
          subtitle = "Stevilo simulacij: 1000")


ggplot(data = o) +
  geom_point(aes(x = velikost_matrike, y = Vrednost, 
                 color = igralec ), cex = 2, 
             position = position_dodge(width = 0.3),
             stat = "identity")+

  labs(y = "Koren srednje kvadaratne napake")+
  ggtitle("Koren srednje kvadaratne napake ocenjenih parametrov",
          subtitle = "Stevilo simulacij: 1000")

o <- enofazna_enaka_por('exp', 'invgama', 4,5)
p <- dvofazna_enaka_por('exp', 'invgama', 5,4)

A <- matrix(c(0,6,-1,4,3,5),2,byrow = TRUE)

B <- matrix(c(0,-1,2,6,2,5),2,3)

A<- matrix(c(6,7,1,5),2,2)
B <- matrix(c(2,4,1,3),2,2)

A <- matrika('exp', 7,4)
B <- matrika('exp', 7,4)

r <- dvofazna_vec_porazdelitev(por0 = 'exp', por1 = 'norm',por2 = 'exp', por3 = 'invgama', por4 = 0, 5,5)
enofazno_pogajanje(matrika('norm', 7,9), matrika('beta', 7,9))
povprecje_enofazna(50, 'exp', 'norm', 9,7)


# To display in an R Markdown document:
# rglwidget()

library(rgl)
j <-enofazna_enaka_por('exp', 'beta', 7,9)
mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
j$color <- mycolors[ j$velikost_matrike ]

plot3d( 
  j$velikost_matrike, j$igralec1, j$igralec2, 
 
  type = 's', 
  radius = .1,
  xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")


t <- dvofazna_vec_porazdelitev(por0 = 'exp', por1 = 'norm',por2 = 'exp', por3 = 0, por4 = 0, 7,9)
                   
z <- enofazna_enaka_por_big_n('exp', 'norm', 10, 7)

#ce bos rabla 
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
