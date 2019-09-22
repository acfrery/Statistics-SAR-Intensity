
################ dgamma ################ Densidade
#Chama as densidades Gamma com os paramentros a parametrização do Modelo SAR de intensidade.
#Função para Densidade DgammaSAR.
#Entradas: x, Looks, mean

dgammaSAR <- function(x, Looks, mean) {
  
  dgamma(x, shape=Looks, rate=Looks/mean)
  
}

################ pgamma ################ Acumulada
#Chama as densidades Gamma com os paramentros a parametrização do Modelo SAR de intensidade.
#Função de distribuição Acumulada pgammaSAR.
## Visualizar e intepretar as funções  **********
#Entradas: q, Looks, mean

pgammaSAR <- function(q, Looks, mean) {
  
  pgamma(q, shape=Looks, rate=Looks/mean)
  
}

################ qgamma ################ Quantis
#Chama as densidades Gamma com os paramentros a parametrização do Modelo SAR de intensidade.
#Função os Quantis qgammaSAR.
## Visualizar e intepretar os Quantis **********
#Entradas: p, Looks, mean

qgammaSAR <- function(p, Looks, mean) {
  
  qgamma(p, shape=Looks, rate=Looks/mean)
  
}

################ rgamma ################ Distribuição
#Chama as densidades Gamma com os paramentros da parametrização do Modelo SAR de intensidade.
#Função para rgammaSAR.
#Entradas: n, Looks, mean

rgammaSAR <- function(n, Looks, mean) {
  
  rgamma(n, shape=Looks, rate=Looks/mean)
  
}


#### Função cuja raíz é o estimador de MV de L baseado na amostra z ##### 


func_to_zero_Looks <- function(L, z) {
  
  lambdahat <- mean(z)
  meanlog <- sum(log(z)) / length(z)
  
  return(log(L)-digamma(L)-log(lambdahat)+meanlog)
  
}

###ExplicaçÃo de como usar###
###LH0 <- uniroot(func_to_zero_Looks, c(1e-3, 100), z)$root 

####################

### Transformação para deixar observações gamma mais parecidas com gaussianas
### Kimber (1983), Sec. 2

phi3 <- function(observ) {
  
  u <- observ / mean(observ)
 nm1 <- length(observ) - 1
  d <- -log(u) - nm1 * log((nm1+1 - u) / nm1)
  
}

### Test Z*_{01} de Kimber para detectar upper ouliers en v.a. Gamma
kimber.test<-function(obs){ 
  tn <- length(obs)
  n1 = tn-1
  u <- obs/ mean(obs)  
  D <-phi3(obs)
  Z <-sqrt(n1*D[tn]/sum(D))
  # Y<-log(mean(obs)/geometric.mean(obs), base = 10)
  # statistic <- nm1/ene*sqrt(Dn/Y) #test Z_{01} de Kimber(1983)
  Pval<- qgrubbs(Z,tn,type = 20,rev = FALSE)
  rk<-data.frame(Z,Pval)
  colnames(rk)=c("Z","Pval")
  return(rk)
}