## Aula 16 - ANOVA
library(tidyverse)
tempo = read_csv2("Tempo Vida.csv")

tempo %>%
  ggplot(aes(x = Marca, y = tempovida)) +
  geom_boxplot()

tempo %>%
  group_by(Marca) %>%
  summarise(Media = mean(tempovida), sd = sd(tempovida), N = n())

tempo %>%
  ggplot(aes(sample = tempovida)) + 
  geom_qq() + 
  stat_qq_line()+
  facet_wrap(~ Marca)

ks.test(x = tempo$tempovida[tempo$Marca == "A"],
        y = pnorm, mean = mean(tempo$tempovida[tempo$Marca == "A"]),
        sd = sd(tempo$tempovida[tempo$Marca == "A"]))

# ou alternativamente e de maneira mais simples

baseA = tempo %>%
  filter(Marca == "A")
baseB = tempo %>%
  filter(Marca == "B")
baseC = tempo %>%
  filter(Marca == "C")
baseD = tempo %>%
  filter(Marca == "D")

ks.test(x = baseA$tempovida,
        y = pnorm,
        mean = mean(baseA$tempovida),
        sd = sd(baseA$tempovida))

ks.test(x = baseB$tempovida,
        y = pnorm,
        mean = mean(baseB$tempovida),
        sd = sd(baseB$tempovida))

ks.test(x = baseC$tempovida,
        y = pnorm,
        mean = mean(baseC$tempovida),
        sd = sd(baseC$tempovida))

ks.test(x = baseD$tempovida,
        y = pnorm,
        mean = mean(baseD$tempovida),
        sd = sd(baseD$tempovida))


## testando homocedasticidade
lawstat::levene.test(y = tempo$tempovida, 
                     group = tempo$Marca,
                     location = "mean")

# como é homocedástico, podemos usar ANOVA

#### ANOVA
ANOVA1 = aov(tempovida ~ Marca,
             data = tempo)

summary(ANOVA1)

### se existe diferença entre marca, quais? usamos o teste de Bonferroni

DescTools::PostHocTest(ANOVA1, 
            method = "bonferroni")

## diferenças entre C e A não são estatisticamente significantes

## podemos usar a especificação de Tukey ou de Duncan

DescTools::PostHocTest(ANOVA1, 
                       method = "duncan")
TukeyHSD(ANOVA1)


