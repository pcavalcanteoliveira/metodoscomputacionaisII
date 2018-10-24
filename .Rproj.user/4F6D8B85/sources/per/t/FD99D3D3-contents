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

S = ((rnorm(n = n, mean = 0, sd = 5) - 0)**2)/(n-1)
coisa = (n-1)*S/25

data = data.frame(coisa)

data %>%
  ggplot(aes(x = coisa))+
  geom_density(color = "blue", size = 2)+
  stat_function(fun = dchisq, args = list(df = n - 1), 
                geom = "line", size = 2, color = "red" )

