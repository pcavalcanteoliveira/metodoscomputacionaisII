###lista aula 8

##1
n = 10000

X = rnorm(n = n, mean = 10, sd = 5)
Z = (X - 10)/5

dados = data.frame(X,Z)

library(ggplot2)
library(dplyr)

dados %>%
  ggplot(aes(x = Z))+
  geom_density(color = "blue", size = 2)+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                geom = "line", size = 2, color = "red" )

## converge


## 2
n = 3

S = vector()
for(i in 1:25000) {

  x = rnorm(n = n, mean = 0, sd = 5)
  S[i] =  var(x)*(n/(n-1))/25
}

data = data.frame(coisa = S)

data %>%
  ggplot(aes(x = coisa))+
  geom_density(color = "blue", size = 2)+
  stat_function(fun = dchisq, args = list(df = n - 1), 
                geom = "line", size = 2, color = "red" )



### poder do teste para variância

poder = function(x, sigmazero, alpha = .05) 
  {
  df = length(x) - 1
  sigma = var(x)
  termo1 = sigma*qchisq(p = alpha/2, df = df)/sigmazero
  termo2 = sigma*qchisq(p = 1 - alpha/2, df = df)/sigmazero
  
  P1 = pchisq(termo1, df = df)
  P2 = pchisq(termo2, df = df, lower.tail = FALSE)
  
  pi = P1 + P2
  
  return(pi)
  
  }
  
poder(glicemia, sigmazero = 30 )

ggplot(data.frame(x = c(0,120)), aes(x))+ 
  stat_function(fun = poder, args = list(x = glicemia), 
                geom = "line", size = 2)
