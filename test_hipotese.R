amostra = c(520, 492, 512, 534, 500, 490, 512,
            515, 495, 510, 511, 502, 501, 498)

x.barra = mean(amostra)

n = length(amostra)

mu = 500

z = qnorm(0.95)

x.c = mu + z*12/sqrt(n)

x.c


hipo.test = function(amostra, alpha = .05,
                   mu, variancia)
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


hipo.test2 = function(amostra, alpha = .05,
                   mu, variancia)
  {
  media = mean(amostra)
  diferencial = media - mu
  raizn = sqrt(length(amostra))
  desvpad = sd(amostra)
  erropad = desvpad/raizn
  
  t = diferencial/erropad
  critico = qnorm(p = alpha, mean = mu, sd = erropad)
  
  result = list(result = ifelse(critico > t, 
           paste("Rejeitamos a Hipótese Nula com nível de significância de ",
               alpha, "%", sep = ""),
          paste("Não Rejeitamos a Hipótese Nula com nível de significância de ",
               alpha, "%", sep = "")),
         t = t,
         "valor critico" = critico,
         RC = paste("Região Crítica: ( -Inf, ",
                    round(critico, digits = 2),"]",
                     sep = "")
)
    print(result)
  
}
hipo.test(amostra = amostra, mu = mu,
        variancia = sd(amostra))


hipo.test2(amostra = amostra, mu = mu,
        variancia = sd(amostra))
