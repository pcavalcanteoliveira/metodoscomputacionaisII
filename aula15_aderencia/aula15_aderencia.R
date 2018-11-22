### TESTES DE ADERENCIA


### UMA PRIMEIRA APLICAÇÃO EM EQUILIBRIO DE HARDY-WEINBERG
library(readr)

dados = read_csv("Genotipo.csv")

## vetor de probabilidades compativeis com equilibrio HW
p = c(0.25, 0.50, 0.25)

## frequencias observadas
table(dados$genotipo)/nrow(dados)

## inspecionar as probabilidades

library(dplyr)

tabela_dif = dados %>%
  group_by(genotipo) %>%
  summarise(observada = n()) %>%
  mutate(esperada = p*100)

## teste mais formal chi-quadrado para probabilidades dadas

chisq.test(table(dados$genotipo), 
           p = p,
           correct = FALSE)


################

amostra = read_table2("amostras.txt")

chisq.test(table(amostra$amost1), p = c(.5, .5), correct = FALSE)

teste2 = chisq.test(table(amostra$amost2), p = dbinom(0:3, 3, .8), correct = FALSE)

teste2$expected

teste3 = chisq.test(table(amostra$amost3), 
           p = dpois(x = 0:15, lambda = 10), 
           correct = FALSE)

plot(dpois(0:25, 10))


