library(readr)

dados = read_table2("Maquinas.txt")

library(dplyr)

dados %>%
  group_by(Maquina)%>%
  summarise(mean(Peso), sd(Peso), mean(Defeito))

summary(dados$Peso)

library(ggplot2)

dados %>%
  ggplot(aes(x=Peso, fill = Maquina))+
  geom_histogram(binwidth = 2.5, aes(y = ..density..))

## verificando normalidade

baseA = dados %>% 
  filter(Maquina == "A")

baseB = dados %>%
  filter(Maquina == "B")

baseA %>%
  ggplot(aes(sample = Peso)) + 
  stat_qq() + 
  stat_qq_line()

baseB %>% 
  ggplot(aes(sample = Peso)) + 
  stat_qq() + 
  stat_qq_line()

## testes KS

ks.test(baseA$Peso, "pnorm", 
        mean(baseA$Peso), 
        sd(baseA$Peso))
  
ks.test(baseB$Peso, "pnorm",
        mean(baseB$Peso), 
        sd(baseB$Peso))

## testes SW

shapiro.test(baseA$Peso)
shapiro.test(baseB$Peso)


## testando diferença das variâncias
## razão = 1 implica vars iguais

library(DescTools)

VarTest(baseA$Peso, baseB$Peso, 
        alternative = "two.sided",
        ratio = 1, 
        conf.level = 0.95)

## testando diferença de medias

dados %>% 
  group_by(Maquina) %>% 
  summarise(N = n(), media = mean(Peso), 
            desvio.padrao = sd(Peso), 
            LI = media - qt(p = 0.975, df = N-1)*desvio.padrao/sqrt(N), 
            LS = media + qt(p = 0.975, df = N-1)*desvio.padrao/sqrt(N))
