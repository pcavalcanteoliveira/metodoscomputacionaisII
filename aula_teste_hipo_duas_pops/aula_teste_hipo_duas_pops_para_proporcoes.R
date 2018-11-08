library(readr)

dados = read_table2("Maquinas.txt")

library(dplyr)

dados %>%
  group_by(Maquina)%>%
  summarise(mean(Peso), sd(Peso), mean(Defeito))



#### testes para igualdade de proporções

resultados2 = dados %>% 
  group_by(Maquina) %>% 
  summarise(N = n(), favoraveis = sum(Defeito), 
            proporcao = favoraveis/N)

resultados2

prop.test(x = c(resultados2$favoraveis[1],resultados2$favoraveis[2]),
          n = c(resultados2$N[1],resultados2$N[2]), 
          alternative = "two.sided",
          conf.level = 0.95, correct = FALSE)


### duas populações normais dependentes

glicemia = read_csv2("Glicemia.csv")

#procurando normalidade
glicemia %>%
  ggplot(aes(sample = Glicemia1)) + 
  stat_qq() + 
  stat_qq_line()

glicemia %>%
  ggplot(aes(sample = Glicemia2)) + 
  stat_qq() + 
  stat_qq_line()


