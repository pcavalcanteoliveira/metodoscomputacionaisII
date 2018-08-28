setwd("~/Área de trabalho")

library(readr)

base <- read_csv2("suicidios.csv")

head(base)


####

#Transformando a variável suicidio por arma de fogo em um factor
base$suicidio_paf = factor(base$suicidio_paf, 
                           labels=c("Não","Sim"))

#Transformando a variável Microrregiao em um factor
base$Microrregiao = factor(base$Microrregiao)

#Transformando a variável Escolaridade em um ordered
base$escolaridade = ordered(base$escolaridade,levels=c(1,2,3,4,5,6), 
                            labels=c("Analfabeto","1 a 3","4 a 7","8 a 11",
                                     "12 ou mais","Desconhecido"))

#Transformando a variável Escolaridade do pai em um ordered
base$escolaridade_pai = ordered(base$escolaridade_pai,levels=c(4,3,5,7,6,8),
                                labels=c("Analfabeto","1 a 3","4 a 7","8 a 11", 
                                         "12 ou mais","Desconhecido"))


#Transformando a variável estado civil em um factor
base$estado_civil = factor(base$estado_civil, 
                           labels=c("Solteiro",
                                    "União estavel", "Casado","Viuvo",
                                    "Divorciado","Desconhecido"))

#Transformando a variável raca em um factor
base$raca = factor(base$raca, 
                   labels=c("Branco","Negro","Amarelo",
                            "Pardo", "Indigena",
                            "Desconhecido"))

#Transformando a variável sexo em um factor
base$sexo = factor(base$sexo, labels=c("Masculino",
                                       "Feminino"))


saveRDS(base, file="base.Rds")

readRDS("base.Rds")

system.time(readRDS("base.Rds"))
system.time(read_csv2("suicidios.csv"))

library(dplyr)

base2 = distinct(base)
dim(base)
dim(base2) ### várias obs dropadas


base3 = base %>%
  distinct(Microrregiao)

dim(base3)

base4 = base %>%
  distinct(idade, escolaridade)

dim(base4)

base7 = base %>%
  select(starts_with("e"))

# reorganiza o data frame, iniciando com a variável Microrregiao e depois as demais
base8 = base %>%
  select(base, Microrregiao, everything())

base9 = rename(base, micro = Microrregiao)

base9 = base %>%
  filter(sexo == "Masculino" & trab_armado == "Sim")

base11 = base %>%
  filter(escolaridade %in% c("1 a 3", "4 a 7"))

base12 = base %>%
  filter((sexo == "Masculino" & estado_civil == "Solteiro") |
           (sexo == "Feminino" & estado_civil == "Casado"))

base13 = base %>%
  arrange(idade)

base14 = base %>%
  arrange(idade, escolaridade)

base15 = base %>%
  arrange(desc(idade))

base16 = base %>%
  mutate(idade2 = idade**2)

dim(base)
dim(base16)

base17 = base %>%
  mutate(UF = substring(Microrregiao,1,2), 
         Grandes.regioes = substring(UF, 1,1))

## se quiser manter somente as variaveis criadas
## trocar mutate por transmute

base18 = base %>%
  transmute(UF = substring(Microrregiao,1,2), 
         Grandes.regioes = substring(UF, 1,1))

# Calculando a media e a mediana da variável idade
base.descritiva = base %>%
  summarise(media.idade = mean(idade, na.rm=TRUE),
            mediana.idade = median(idade, na.rm=TRUE))

## descritivas por grupo
grupo = base %>%
  group_by(sexo, escolaridade, estado_civil) %>%
  summarise(media.idade = mean(idade, na.rm=TRUE),
            mediana.idade = median(idade, na.rm=TRUE))

grupo

base19 = base %>% 
  select(-estado_civil) %>%
  filter(sexo=="Masculino") %>%
  group_by(raca,escolaridade) %>%
  summarise(maximo=max(idade, na.rm = TRUE),
            media=mean(idade, na.rm = TRUE))

base19

