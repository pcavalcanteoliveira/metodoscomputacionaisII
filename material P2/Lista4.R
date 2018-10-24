

require(tidyverse)
require(readr)
require(readxl)

# -------------------

# 1)

## c)

intervalo.media = function(x, sigma2, alpha){
  
  # Tratando o vetor para ficar sem NA:
  amostra = na.omit(x)
  
  
  n = length(amostra)
  
  # Obtendo estimativa pontual:
  x.barra = mean(amostra)
  
  # Obtendo o valor critico da distribuição normal PADRAO!:
  z.c = qnorm(alpha/2, lower.tail = FALSE)
  
  # Obtendo os limites do intervalo:
  LI = x.barra - z.c * sqrt(sigma2/n)
  LS = x.barra + z.c * sqrt(sigma2/n)
  
  # Retornando a função obdecendo o modelo pedido:
  return( cat("IC(m,", (1 - alpha)*100, "%)=", "[", round(LI, 2), round(LS, 2), "]", sep = "") )
  
  
}







# 4)



funcao.IC.cat = function(x, y, conf){
  
  # Juntando os vetores:
  juntos = cbind(x, as.numeric(as.vector(y)))
  
  # Calculando por categoria:
  IC.cat = matrix(NA, nrow = nlevels(y), ncol = 2,       # nlevel: numero de niveis/categorias (levels) do fator
                  dimnames = list(levels(y), c("LI", "LS")))
  
  for( i in 1:nlevels(y) ){
    
    aux = juntos[which(juntos[,2] == levels(y)[i]), 1]   # levels: categoria da variavel y como fator
    
    # Retirando NA do vetor x:
    x.novo  = na.omit(aux)
    
    # Calculando o tamanho do novo vetor:
    n = length(x.novo)
    
    # Encontrando IC para media com var desconhecida:
    
    s2        = var(x.novo)
    x.barra   = mean(x.novo)
    
    x.critico = qt( (1 - conf)/2, lower.tail = FALSE, df = n - 1)
    
    LI = x.barra - x.critico * sqrt(s2/n)
    LS = x.barra + x.critico * sqrt(s2/n)
    
    IC.cat[i, 1] = round(LI, 3)
    IC.cat[i, 2] = round(LS, 3)
    
  }
  
  return(IC.cat)
  
}


# 5)

  dados = read_table2("Base saude.txt")
  
  x = dados$Estatura
  y = as.factor(dados$DST)   # Minha função não aceita y com labels do tipo character!! Por isso transformar em fator sem mudar label!
  
  conf = 0.95

  funcao.IC.cat(x = x, y = y, conf = conf)
  
  # Se quiser identificar corretamente o que significa as linhas da saida da função criada ...
  
  resultado = funcao.IC.cat(x = x, y = y, conf = conf)
  dimnames(resultado) = list(c("Sem DST", "Com DST"), c("LI", "LS") )
  
  resultado
  
  
 # 7) 
  
    exames = readRDS("exames medicos.rds")
    
    ## a)
    
    data.frame(exames$HDL) %>% ggplot( aes(sample = exames.HDL) ) + stat_qq() + stat_qq_line( aes(col = "red") ) +
         xlab("Quantis Teóricos") + ylab( "Quantis Amostrais") # Nao segue distribuiçao normal
    
    
    
    
    # Calculo da curtose (medida numérica):
    
    momento_4 = mean( (exames$HDL - mean(exames$HDL))^4 )
    denom     = sqrt( var(exames$HDL)^4 )
    
      # Pelo valor da curtose abaixo nao segue distribuiçao normal
    curtose = momento_4/denom # coeficiente momento de curtose

      # Classificaçao da curtose sob formula acima: se curtose = 3, entao a curva é mesocurtica "==" normal
    
    
    ## e)
    glicose.s45 = iflese(exames$glicose > 45, 1, 0)
    
    prop.test(sum(glicose.s45), length(glicose.s45), conf.level = 0.95)
    