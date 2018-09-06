setwd("~/Área de trabalho")

# Criando o objeto x
x = 10


# só executa se a condição lógica for atendida
if(x < 20){
  y = 1
}

# note que x foi criado
exists("x")

# só executa se a condição lógica for atendida
if(x > 20){
  z = 10
}

# note que z não foi criado
exists("z")

######################
######################

# Criando o objeto x
x = 10



if(x < 20){
  y = 1
  cat("O número y criado é igual a", y)
} else{
  y = 10
  cat("O número y criado é igual a", y)
}


##################

# Criando o objeto x
x = 10

if(x < 10){
  y = 1
  cat("O número y criado é igual a", y)
} else if(x >=10 & x<20){
  y = 10
  cat("O número y criado é igual a", y)
} else{
  y = 100
  cat("O número y criado é igual a", y)
}
################

x = c(310,20,230,40)

if(x < 10){
  y = 1
  cat("O número y criado é igual a", y)
} else if(x >=10 & x<20){
  y = 10
  cat("O número y criado é igual a", y)
} else{
  y = 100
  cat("O número y criado é igual a", y)
}
############

#Criando o objeto x
x = c(310,20,230,40)

ifelse(x >130, 1, 0)


###### LOOP ###########

x = 50
aux = 0

for(j in 1:10){
  aux = aux + x/j
}

aux


######### FUNÇÕES


mensagem = function(){
  print("Adoro Métodos Computacionais II")
}

#evocando a função
mensagem()

soma.minha = function(x,y){
  valor = x + y
  return(valor)
}

soma.minha(2,4)
soma.minha(2,"h")

#Melhorando a função

soma.minha = function(x,y){
  if(class(x) != "numeric" | !is.numeric(y)){
    print("Você deve inserir dois números para que a função possa ser executada!")
  }else{
    valor = x + y
    return(valor)    
  }
}

soma.minha(2,"h")

###argumentos com padrão

potencia = function(x,base = 10){
  valor = base^x
  return(valor)
}

potencia(3)

funcao.estranha <- function(y) {
  
  if(y > 0){
    return(3*exp(-3*y))
  }
    {0}
    }

funcao.estranha(2)  
funcao.estranha(-5)

################ DISTRIBUIÇÕES DE PROBABILIDADE

pnorm(28,mean = 25, sd = sqrt(32))

rnorm(10)

rnorm(10,mean = 25, sd = sqrt(32))
qnorm(0.3,mean = 25, sd = sqrt(32))

dnorm(10,mean = 25, sd = sqrt(32))
dpois(10,lambda = 8)

ppois(3,lambda = 8)

##ppois(3,lambda = 8) é equivalente à:
dpois(0,lambda = 8) + dpois(1,lambda = 8) + dpois(2,lambda = 8) + dpois(3,lambda = 8)

pgamma(0.5,shape = 1, rate = 5)


##P(23 < X < 30)

p = pnorm(30,mean = 25, sd = sqrt(32)) - pnorm(23,mean = 25, sd = sqrt(32))
p


## P(3 < Y < 30) ; Y ~ Poisson(8)

p2 = ppois(30,lambda = 8) - ppois(3,lambda = 8)
p2

# Gere uma F de Fisher com 2 e 4 graus de liberdade
F = rf(n = 1000, 2,4)
F

# c = ? se P(Z>c) = 0.7 ; P ~ Gamma(1,5)
p3 = 1 - qgamma(0.3 , 1, 5)
p3


## grafando distribuições
library(ggplot2)
ggplot(data = data.frame(x = c(-10,30)), aes(x=x)) + 
  stat_function(fun = dnorm, args = list(mean = 10, sd = 5)) + 
  ##stat_function() para funções
  ##importante manter essa sintaxe, passar uma lista com argumentos
  ylab("f(x)") + 
  ggtitle("Gráfico da densidade de uma N(10,25)")



## acumulada da normal

ggplot(data = data.frame(x = c(-10,30)), aes(x=x)) + 
  stat_function(fun = pnorm, args = list(mean = 10, sd = 5)) + 
  ylab("F(x)") + 
  ggtitle("Gráfico da distribuição acumulada de uma N(10,25)")


#gerando amostra
base = data.frame(amostra = rnorm(300,10,5))

#Fazendo o histograma da amosytra gerada acima
ggplot(data = base, aes(x = amostra)) + 
  geom_histogram(aes(y = ..density..), bins = 10) +
  stat_function(fun = dnorm, args = list(mean = 10, sd = 5))

##### MONTANDO GRID

library(gridExtra)

#Gráfico da densidade de X
graf1 = ggplot(data = data.frame(x = c(-10,30)), aes(x=x)) + stat_function(fun = dnorm, args = list(mean = 10, sd = 5)) + ylab("f(x)") 

#Gráfico de F(x)
graf2 = ggplot(data = data.frame(x = c(-10,30)), aes(x=x)) + stat_function(fun = pnorm, args = list(mean = 10, sd = 5)) + ylab("F(x)")

#Histograma da amostra
graf3 = ggplot(data = base, aes(x = amostra)) + geom_histogram(aes(y = ..density..), bins = 10) + stat_function(fun = dnorm, args = list(mean = 10, sd = 5), col = "red") + xlab(x)

grid.arrange(graf1,graf2,graf3, ncol = 2)
