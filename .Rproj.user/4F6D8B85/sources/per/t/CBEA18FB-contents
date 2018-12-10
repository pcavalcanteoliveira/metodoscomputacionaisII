## LISTA 6

library(tidyverse)
library(DescTools)

##1

numeros = matrix(c(20,10,10,15,30,20,35), nrow = 1)

chisq.test(numeros, correct = FALSE)

## 2


quimio = matrix(c(51,33,16,58,29,13,48,42,30,26,38,16), 
                nrow = 4, ncol = 3)

colnames(quimio) = c("Pouca", "Média", "Alta")
rownames(quimio) = c("Tipo I", "Tipo II", "Tipo III", "Tipo IV")
tipo = c("Tipo I", "Tipo II", "Tipo III", "Tipo IV")

chisq.test(quimio, correct = FALSE) # evidencias de que são dependentes

## para fazer a descritiva
tipos = rep(tipo, 3)
quimiotb = as.tibble(quimio)
quimiotb = gather(quimiotb)
quimiotb$tipo = tipos

quimiotb = rename(quimiotb, reacao = key)
quimiotb$reacao = factor(quimiotb$reacao)
quimiotb$tipo = factor(quimiotb$tipo)

quimiotb %>%
  ggplot(aes(x = reacao, fill = tipo)) +
  geom_bar(aes(y = value), stat = "identity")

## 3

psico = readxl::read_xls("Banco Escalas Psicologia.xls",
                         na = "999")
psico = psico %>%
  dplyr::filter(Grupo < 3)

psico$Grupo = factor(psico$Grupo)
psico$Sexo = psico$Sexo - 1


anova3 = aov(Idade ~ Grupo,
    data = psico)
summary(anova3) #diferença significante a 3%

# rodar aqui teste de independencia entre grupo e sexo



anova5 = aov(Anos_escolaridade ~ Grupo,
             data = psico)
summary(anova5) # significante a 5%

anova6 = aov(QI ~ Grupo,
             data = psico)
summary(anova6) # significante a 5%

anovaRAVLT = aov(RAVLT ~ Grupo,
                 data = psico)
summary(anovaRAVLT) # significante

anovaWCST = aov(WCST ~ Grupo,
                data = psico)
summary(anovaWCST) # significante

anovaStroop = aov(Stroop ~ Grupo,
                  data = psico)
summary(anovaStroop) #significante

anovaDigitos = aov(Digitos ~ Grupo,
                   data = psico)
summary(anovaDigitos) #significante

anovaRep = aov(psico$`Reproduçao Visual` ~ Grupo,
               data = psico)
summary(anovaRep) #significante

# Questão 4


psico$novo = ifelse(psico$Grupo == 0, "Controle", "Trauma")


novoanova3 = aov(Idade ~ novo,
             data = psico)
summary(novoanova3) #diferença significante a 3%

# rodar aqui teste de independencia entre grupo e sexo
novoanova



novoanova5 = aov(Anos_escolaridade ~ novo,
             data = psico)
summary(novoanova5) # significante a 5%

novoanova6 = aov(QI ~ novo,
             data = psico)
summary(novoanova6) # significante a 5%

novoanovaRAVLT = aov(RAVLT ~ novo,
                 data = psico)
summary(novoanovaRAVLT) # significante

novoanovaWCST = aov(WCST ~ novo,
                data = psico)
summary(novoanovaWCST) # não é mais significante

novoanovaStroop = aov(Stroop ~ novo,
                  data = psico)
summary(novoanovaStroop) #significante

novoanovaDigitos = aov(Digitos ~ novo,
                   data = psico)
summary(novoanovaDigitos) #significante

novoanovaRep = aov(psico$`Reproduçao Visual` ~ novo,
               data = psico)
summary(novoanovaRep) #significante

# questão 5

embrapa = readxl::read_xls("EMBRAPA.xls", 
                           na = "NA")
embrapa$Tratamento = factor(embrapa$Tratamento)

anovapeso = aov(Peso26 ~ Tratamento,
                data = embrapa)
summary(anovapeso) #diferenças significantes a 5%

PostHocTest(anovapeso, method = "bonferroni") 
# a diferença par-a-par no método de Bonferroni não indica nenhuma diferença nos pesos

embrapa$difpeso = embrapa$Peso26 - embrapa$Peso0

t.test(embrapa$difpeso,
       alternative = "greater",
       mu = 88)
# rejeitamos a hipotese nula de que a diferença é menor que 88 a 4% de significância


# questão 6

vulnerabilidade = read_csv2("Basevulnerabilidade.csv")
vulnerabilidade$situacao = factor(ifelse(vulnerabilidade$situacao == 1, 
                                         "Alugado", "Próprio"))

vulnerabilidade$escmae = factor(ifelse(vulnerabilidade$escmae == 1, "Baixa",
                                       ifelse(vulnerabilidade$escmae == 2, "Média", "Alta")))

anovavulneravel = aov(vulnerabilidade ~ situacao + escmae,
                      data = vulnerabilidade)
summary(anovavulneravel) ## estão fortemente associadas

# não tem variável idade na base

# questão 7

cancer = readxl::read_xls("Banco Cancer Mama.xls")

barra = cancer %>%
 group_by(menstruacao) %>%
  summarise(Media = mean(cancer)) # parece ser um fator de risco considerável

barra %>%
  ggplot(aes(x = menstruacao)) +
  geom_bar(aes(y = Media), stat = "identity", fill = "light blue")

anovamenstru = aov(cancer ~ menstruacao,
                   data = cancer)
summary(anovamenstru) # o modelo também sugere

modelomenstru = lm(cancer ~ menstruacao + peso + grauparentesco,
                   data = cancer)
summary(modelomenstru) # estimativas por OLS sugerem isso também


# questão 8

cancer$depois = as.numeric(cancer$depois)
cancer$antes = as.numeric(cancer$antes)
cancer$efeitotratamento = cancer$antes - cancer$depois

t.test(cancer$efeitotratamento, mu = 0, alternative = "less")
# a 5% de significância encontramos que o tratamento teve sim um efeito

cancer %>%
  ggplot(aes(x = efeitotratamento)) +
  geom_histogram() #inspeção visual


## questão 9

cancer %>%
  ggplot(aes(x = as.factor(cancer), y = peso)) +
         geom_boxplot()

cancer %>%
  group_by(cancer) %>%
  summarise(media = mean(peso))

# pessoas com cancer parecem serem levemente mais pesadas

anovapeso = aov(peso ~ cancer, 
                data = cancer)

summary(anovapeso)

## Questão 10

barra10 = cancer %>%
  group_by(grauparentesco) %>%
  summarise(media = mean(cancer))

barra10 %>%
  ggplot(aes(x = grauparentesco)) +
  geom_bar(aes(y = media), stat = "identity") 
#sugere que existem diferentes taxas de ocorrência de câncer entre os grupos

chisq.test(table(cancer$grauparentesco, cancer$cancer),
           correct = FALSE)
# a 5% de significância rejeitamos a hipótese nula de independência
# a evidência aponta que de fato é um fator de risco




  


