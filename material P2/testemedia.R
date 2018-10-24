## teste media

RC = function(x, alpha = .05, muzero = mean(x, na.rm = TRUE), sd) {
  
  termo = qnorm(1 - alpha)*sd/sqrt(length(x))
  critico = muzero + termo
  
  results = list(Resultado = ifelse(mean(x) > critico, paste("Rejeitamos a hipótese nula de que a média populacional é ", muzero, " a ", alpha, " % de confiança", sep = ""),
         paste("Não rejeitamos a hipótese nula de que a média populacional é ", muzero, " a ", alpha, " % de confiança", sep = "")),
         crítico = critico,
         Média = mean(x))
  
  print(results)
  
}

amostra = c(520, 492, 512, 534, 500, 490, 512, 515, 495, 510, 511, 502, 501, 498)

RC(x = amostra, muzero = 500, sd = 12)


## agora com variância desconhecida

library(ggplot2)
library(dplyr)
amostra = c(507, 510, 503, 505, 506, 505, 506, 500, 506, 518, 512, 502, 508, 508, 500, 502, 509, 500, 504, 496, 498, 507, 502, 501)

data.frame(amostra = amostra) %>%
  ggplot(aes(sample = amostra))+
  stat_qq()+
  stat_qq_line()

#o grafico QQ sugere normalidade

## Hipo nula de que a media populacional = 500
t.test(amostra, alternative = "greater", mu = 500)


## agora testes para variância

var(amostra)
# 23.47

library(DescTools)

## testando se é estatísticamente diferente de 25, hipo nula de que é = 25
VarTest(amostra, alternative = "two.sided", sigma.squared = 25, conf.level = 0.95)


## teste de hipótese para proporção
#testando se menos de 25% da população tem peso menor que 500

proporcao = ifelse(amostra < 500, 1, 0)
proporcao

prop.test(x = sum(proporcao), n = length(proporcao), p = .25, 
          alternative = "less", conf.level = .95)


