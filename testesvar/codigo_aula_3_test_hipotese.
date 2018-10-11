library(readr)

dados = read_csv2("Glicemia.csv")
glicemia = dados$glicemia

hipo.test = function(amostra, alpha = .05,
                     mu, variancia = sd(amostra))
{
  media = mean(amostra)
  diferencial = media - mu
  raizn = sqrt(length(amostra))
  desvpad = sd(amostra)
  erropad = desvpad/raizn
  
  t = diferencial/erropad
  
  critico = qnorm(p = 1 - alpha, mean = mu, sd = erropad)
  
  result = list(result = ifelse(critico < t, 
                                paste("Rejeitamos a Hipótese Nula com nível de significância de ",
                                      alpha, "%", sep = ""),
                                paste("Não Rejeitamos a Hipótese Nula com nível de significância de ",
                                      alpha, "%", sep = "")),
                t = t,
                "valor critico" = critico,
                RC = paste("Região Crítica: [",round(critico, digits = 2),
                           ", Inf]", sep = "")
  )
  print(result)
  
}

shapiro.test(glicemia)

## amostra é aproximadamenet normal, porque não conseguimos rejeitar a hipótese nula de que é normal

mu = 100
hipo.test(amostra = glicemia, mu = mu)
mean(glicemia)
t.test(x = glicemia, alternative = "greater", mu = 100)

library(ggplot2)
library(dplyr)

dados %>%
  ggplot(aes(sample = glicemia))+
  stat_qq()+
  stat_qq_line()


poder.teste.chisq.media = function(amostra, alpha = .05, sigma_zero)
  
  {
  vari = var(amostra)
  df = length(amostra - 1)
  k1 = qchisq(p = (alpha / 2), df = df)
  k2 = qchisq(p = 1 -(alpha / 2), df = df)
  beta = pchisq(q = (sigma_zero*k1/vari), 
                df = df) + pchisq(q = (sigma_zero*k2/vari), 
                                             df = df, lower.tail = FALSE)
  
  poder = 1 - beta
  results = list(poder = poder, beta = beta)
  print(results)
  }

glicemia2 = glicemia*glicemia
var(glicemia2)
poder.teste.chisq.media(amostra = glicemia2, sigma_zero = var(glicemia2))

