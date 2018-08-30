setwd("C:/Users/e270860661/Downloads/computacionaisiimerging")
library(readxl)

municipios = read_xlsx("Municipios_2010.xlsx")
saveRDS(municipios, "municipios.Rds")

obitos = read.csv2("Num de obitos 2010.csv")
saveRDS(obitos, "obitos.Rds")
