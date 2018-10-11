funcao = function(x){
  valor = 5*exp(-5*x)*I(x>0)
  return(valor)
}

#integrando numericamente
integrate(funcao, lower = 0, upper = 0.1)
#conferindo se bate
pexp(0.1, rate = 5)

#bate!

integrate(funcao, lower = 0, upper = Inf)


#integrais multiplas
library(cubature)

funcao.mult = function(x){
  valor = 2/3 * (x[1] + x[2] + x[3])
  return(valor)
}

adaptIntegrate(funcao.mult, lowerLimit = c(0, 0, 0), upperLimit = c(0.5, 0.5, 0.5))


#Primeiro usamos o quote para criar a função
f = quote(exp(-x)*cos(x)*y + 3*y^2*x)

#Visualizando f
f

#Primeiro usamos o quote para criar a função
f = quote(exp(-x)*cos(x)*y + 3*y^2*x)

#Visualizando f
f

D(df.dx, "x")

##OTIMIZAÇâo

#Criando a função
fun.max = function(x){
  valor = 2 - (x - 3)^2
  return(valor)
}

#Plotando a função
library(ggplot2)

ggplot(data = data.frame(x = c(-3,5)), aes(x = x)) + 
  stat_function(fun = fun.max) 

#Obtendo o máximo
maximo = optimise(fun.max,c(-3,5), maximum = TRUE)

#Visualizando o máximo
maximo

ggplot(data = data.frame(x = c(-3,5)), aes(x = x)) + 
  stat_function(fun = fun.max) + 
  geom_vline(aes(xintercept = maximo$maximum), 
             linetype = "dashed", color = "red" )  

#APLICANDO ISSO A MAXIMA VEROSSIMILHANÇA

#Criando a função
ver = function(l,z){
  valor = exp(-l*length(z))*l^{sum(z)}*prod(1/factorial(z))
}

#Obtendo o gráfico da função de verossimilhança
ggplot(data = data.frame(x = c(0,10)), aes(x = x)) + 
  stat_function(fun = ver, args = list(z = c(2,3,1,2,3,4,5,3,3,1))) + xlab("z")

#Definindo a nossa população
x = c(1,3,5,5,7)

#Vamos definir o tamanho da amosra que iremos trabalhar
n = 2

#Vamos definir o número de amostras da dstribuição de x_barra
num.amostra = 1000

#Vamos retirar 1000 amostras de tamanho 2 e armazenar em uma matriz
mat.amostras = matrix(NA, ncol=num.amostra, nrow=n)

for(i in 1:num.amostra){
  mat.amostras[,i] = sample(c(1,3,5,5,7),n,replace = TRUE)
}

#Calcular a média para todas as amostras obtidas
base3 = data.frame(media.amostral = apply(mat.amostras,2,mean))

#Visualizar o histograma das médias amsotrais

ggplot(data = base3, aes(x = media.amostral)) + 
  geom_histogram(aes(y = ..density..), bins = 15) 

media = mean(x)

media

vari =((5-1)/5 * var(x)) / n

vari

ggplot(data = base3, aes(x = media.amostral)) + 
  geom_histogram(aes(y = ..density..), bins = 15) + 
  stat_function(fun = dnorm, args = list(mean = media, 
                                         sd = sqrt(vari)), col = "red")

#Acrescentando a média amostral (azul) e a média populacional(vermelho)
ggplot(data = base3, aes(x = media.amostral)) + 
  geom_histogram(aes(y = ..density..), bins = 15) + 
  stat_function(fun = dnorm, args = list(mean = media,
                                         sd = sqrt(vari)), col = "red") + geom_vline(aes(xintercept = mean(media.amostral)), linetype = "dashed", color = "red" ) + 
  geom_vline(aes(xintercept = media), linetype = "dashed", color = "blue" )  
