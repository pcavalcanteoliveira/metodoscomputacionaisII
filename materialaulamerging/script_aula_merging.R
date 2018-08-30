setwd("~/Área de trabalho")
library(readr)
library(readxl)

base_obitos <- read_csv2("Num de obitos 2010.csv" )
head(base)

base_municipios <- read_excel("Municipios_2010.xlsx")
head(base_municipios)

############################
library(dplyr)

b1 <- tibble(ID = c(1010, 2010, 3010, 4010, 5010),
             W = c('a', 'b', 'c', 'd', 'e'),
             X = c(1, 1, 0, 0, 1),
             Y=rnorm(5))

b2 <- tibble(ID = c(1010, 7010, 3010, 6010, 8010),
             A = c('z', 'b', 'k', 'd', 'l'),
             B = c(1, 2, 3, 0, 4),
             C =rnorm(5))

b3 <- tibble(Identificacao = c(1010, 5010,2541),
             Z =c(123453,221323,123552),
             W =c("Rio","São Paulo","Niteroi"))

################################################

merge1 = inner_join(b1,b2,by="ID")
merge1

### inner_join preserva informação nas duas bases, observe que cada uma
### tem vetores _diferentes_

merge2 = inner_join(b1,b3,by=c("ID"="Identificacao"))
merge2

#Função left_join: Combina as duas bases incluindo todas as variáveis 
#de ambas as bases e todas as linhas da base a esquerda

merge3 = left_join(b1,b2,by="ID")
merge3

# Função right_join: Combina as duas bases incluindo todas as variáveis 
#de ambas as bases e todas as linhas da base a direita

merge4 = right_join(b1,b2,by="ID")
merge4

# Função full_join: Combina as duas bases incluindo todas as variáveis 
# de ambas as bases e todas as linhas de ambas as bases
merge5 = full_join(b1,b2,by="ID")
merge5

# Função semi_join: Combina as duas bases incluindo as variáveis 
#da base a esquerda e todas as linhas comuns as duas bases
merge6 = semi_join(b1,b2,by="ID")
merge6

# Função anti_join: Combina as duas bases incluindo as variáveis da 
#base a esquerda e todas as linhas que não são comuns as duas bases
merge7 = anti_join(b1,b2,by="ID")
merge7


########################################

merge_municipios = full_join(base_obitos, base_municipios, 
                             by = c("Codibge7" = "codmun"))

merge_municipios
str(merge_municipios)

microreg = base_municipios %>%
  select(Microrregiao, Nome_Micro) %>%
  distinct()

str(microreg)


base.tot = merge_municipios %>%
  group_by(Microrregiao) %>%
  summarise(tot.acidente = sum(acidente),
            tot.homicidio = sum(homicidio),
            tot.suicidio = sum(suicidio),
            tot.indeterminado = sum(indeterminado),
            tot.veiculos = sum(veiculos),
            tot.h_legal = sum(h_legal)) %>%
  distinct()




###################################

#Criando a base d1
d1 <- tibble(ID = c(1010, 2010, 3010, 4010, 5010),
                  W = c('a', 'b', 'a', 'b', 'e'),
                  X = c(1, 1, 0, 0, 1),
                  Y=c(3,6,3,5,7))

d1

#Criando a base d2

d2 <- tibble(ID = c(1010, 2010, 5010),
                  W = c('a', 'b', 'e'),
                  X = c(1, 1, 1),
                  Y=c(3,6,7))

d2

#Criando a base d3

d3 <- tibble(ID = c(3210, 2011, 1017),
                  W = c('b', 'e', "a"),
                  X = c(1,0,1),
                  Y=c(3,5,4))

d3

#Criando a base d4

d4 <- tibble(ID = c(3210, 2011, 1017),
                  idade = c(10,20,32),
                  Sexo = c(1,0,1))


#Criando uma base com as linhas comus as duas bases
intersect(d1,d2)

#Criando uma base unindo todas as linhas das duas bases
union(d1,d3)

#Criando uma base com as linhas distintas nas duas bases
setdiff(d1,d3)

#Empilhando duas bases, uma em cima da outra
rbind(d1,d3)

#Empilhando duas bases, uma ao lado da outra
cbind(d3,d4)


