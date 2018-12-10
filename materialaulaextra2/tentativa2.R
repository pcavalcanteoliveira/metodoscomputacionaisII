## tentando de novo independência censurando grupos
library(tidyverse)
alunos  = read_csv("alunos.csv")

alunos %>%
  ggplot(aes(x = tempo)) +
  geom_histogram(aes(y = ..density..), binwidth = 25,
                 fill = "light blue") +
  stat_function(fun = dexp, args = list(rate = 1/50),
                color = "green", size = 2)

alunos %>%
  ggplot(aes(sample = tempo)) +
  stat_qq(distribution = qexp, dparams = list(rate = 1/50)) +
  stat_qq_line(distribution = qexp, dparams = list(rate = 1/50))

a = quantile(alunos$tempo, .25)
b = quantile(alunos$tempo, .5)
c = quantile(alunos$tempo, .75)

alunos$grupo = factor(ifelse(alunos$tempo > c, "d", 
                      ifelse(alunos$tempo > b, "c", 
                             ifelse(alunos$tempo  > a, "b", "a"))))


chisq.test(table(alunos$grupo),
           p = c(.25,.25,.25,.25),
           correct = FALSE)
### 3

uni = read_csv("universidade.csv")


table(uni)

chisq.test(table(uni), correct = FALSE) # rejeitamos independência

# para aplicar teste de homogeneidade teríamos que a priori 
#definir o número de alunos de cada curso
