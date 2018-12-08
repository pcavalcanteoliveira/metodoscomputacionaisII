## AULA EXTRA PARA P3

library(tidyverse)

alunos = read_csv("alunos.csv")

## 1_a


### HISTOGRAMA COM DENSIDADE
alunos %>%
  ggplot(aes(x = tempo)) +
  geom_histogram(aes(x = tempo, y = ..density..), 
                 binwidth = 40) +
  stat_function(fun = dexp, args = list(rate = 1/50))


## QQ PLOT
alunos %>%
  ggplot(aes(sample = tempo)) +
  stat_qq(distribution = qexp, dparams = list(rate = 1/50)) +
  stat_qq_line(distribution = qexp, dparams = list(rate = 1/50))

# TESTE KS

ks.test(alunos$tempo,
        pexp,
        rate = 1/50)

###### TESTE COM CHI QUADRADO DE ADERENCIA

a = qexp(rate = 1/50, p = .25)
b = qexp(rate = 1/50, p = .5)
c = qexp(rate = 1/50, p = .75)

# agora classificamos as obs
alunos$gp = ifelse(alunos$tempo > c, "d", 
                   ifelse(alunos$tempo > b, "c",
                          ifelse(alunos$tempo > a, "b", "a"))) 

p = c(.25,.25,.25,.25) # cada quartil tem 25% de prob

chisq.test(table(alunos$gp), 
           p = p,
           correct = TRUE)

## 1_b

# atribuímos labels aos grupos de alunos
alunos$gpnotas = ifelse(alunos$notas < 3, "A",
                        ifelse(alunos$notas < 6, "B",
                               ifelse(alunos$notas < 8, "C", "D")))

## teste de homogeneidade
chisq.test(table(alunos$gpnotas),
           p = c(.3,.3,.2,.2),
           correct = FALSE)


## questão 2

glicemia = read_csv("Glicemia.csv")

anova1 = aov(glicemia ~ grupo, data = glicemia)
summary(anova1)


# testes post hoc
library(DescTools)

PostHocTest(anova1, method = "bonferroni")

TukeyHSD(anova1)

## questão 3

uni = read_csv("universidade.csv")
uni$satisfacao = ifelse(uni$satisfacao == "Sim", 1, 0)
anova2 = aov(satisfacao ~ curso, data = uni)
summary(anova2)

table(uni$satisfacao, uni$curso)


