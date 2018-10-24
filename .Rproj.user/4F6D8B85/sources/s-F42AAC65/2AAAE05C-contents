### aula Intervalos de Confiança

#Criando uma função que calcula o IC para a média de uma população normal com variância conhecida

IC.media = function(x, alpha = .05, sigma = var(x, na.rm = TRUE))
  
{
  
  raizn = sqrt(length(x))
  termo = qnorm(1 - alpha/2, sd = sqrt(sigma))*sqrt(sigma)/raizn

  Infimo = round(mean(x) - termo, digits = 3)
  Supremo = round(mean(x) + termo, digits = 3)
  
print(paste("[", Infimo, "; ", Supremo, "]"), sep = "")
print(termo)
print(qnorm(1 - alpha/2, sd = sqrt(sigma)))
    
}
## testando
amostra=c(10,12,13.4,15.2,9.7,10.5,12,15.7,18,17.6)


IC.media(x = x)
IC.media(x = amostra, sigma = 5)

#bateu

dados = read.table("Base saude.txt", header = TRUE, na = 9)
dados

#IC da variável idade, tendo variância populacional = 16

IC.media(dados$Idade, sigma = 16)

### IC para proporções, aproximação normal de uma Bernoulli

IC.prop = function(p, alpha = .05)
  
{
  
  p = mean(x)
  termo = qnorm(1 - alpha/2, sd = sd(x))*sqrt((p*(1-p)/n))
  Infimo = p - termo
  Supremo = p + termo
  
  print(paste("[", Infimo, "; ", Supremo, "]"), sep = "")
  print(termo)
}

### IC para variância

IC.vari = function(x, alpha = .05) 
      
  {
  
  nmenosum = length(x) - 1
  sdois = var(x, na.rm = TRUE)
  
  Infimo = sdois*nmenosum/qchisq(1 - alpha/2, df = nmenosum)
  Supremo = sdois*nmenosum/qchisq(alpha/2, df = nmenosum)
  
  print(paste("[", Infimo, "; ", Supremo, "]"), sep = "")
}

dados$Estatura = as.double(dados$Estatura)
dados$Estatura
IC.vari(dados$Estatura)

data = readRDS("cafe.rds")
IC.media(data$Peso, sigma = 12**2)

library(ggplot2)
library(dplyr)

data %>%
  ggplot(aes(x = Peso))+
  geom_histogram(binwidth = 5)

data %>%
  ggplot(aes(sample = Peso))+
  geom_qq()+
  geom_qq_line()

shapiro.test(data$Peso)
qqnorm(data$Peso)

## teste de Shapiro-Wilk e Inspeção visual sugerem que Peso é normalmente distribuído
