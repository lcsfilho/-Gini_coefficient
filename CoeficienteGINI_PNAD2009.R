setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Antiga/2009/dados")

require("foreign")
library('dplyr') # data manipulation
library(tidyr)
library(haven)
# carregando a base como uma lista
#UF=Código da UF
#V4105=Situação censitária;
#V4107=Área Censitária;
#V4729=Peso da pessoa;
#RDPC=Rendimento mensal domiciliar percapita
PNAD2009 <- read_sav("PES_DOM2009.sav",col_select = c("UF","V4105","V4107","V4729","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD2009)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD2009))
colSums(PNAD2009=="")

#Removendo dados missing:
PNAD2009 <- PNAD2009 %>% drop_na(RDPC)

summary(PNAD2009)

#Contar dados missing
colSums(is.na(PNAD2009))
colSums(PNAD2009=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD2009_BR <- PNAD2009[order(PNAD2009$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2009_BR$V4729*PNAD2009_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2009_BR <-cbind.data.frame(PNAD2009_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2009_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2009_BR <-cbind.data.frame(PNAD2009_BR,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2009_BR$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2009_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2009_BR <-cbind.data.frame(PNAD2009_BR,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2009_BR$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2009_BR <-cbind.data.frame(PNAD2009_BR,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2009_BR$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2009_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2009_BR <-cbind.data.frame(PNAD2009_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD2009_BR))
for (k in 0:n-1)
  {
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
  }
Gini_BR <- 1-sum(Soma)
print(Gini_BR)



###############################################
##                NORDESTE                   ##
###############################################

######&&&&&&&&&#################&&&&&&&&&######
###########FILTRANDO REGIÃO NORDESTE###########
PNAD2009_NE <-filter(PNAD2009, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2009_NE <- PNAD2009_NE[order(PNAD2009_NE$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2009_NE$V4729*PNAD2009_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2009_NE <-cbind.data.frame(PNAD2009_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2009_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2009_NE <-cbind.data.frame(PNAD2009_NE,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2009_NE$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2009_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2009_NE <-cbind.data.frame(PNAD2009_NE,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2009_NE$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2009_NE <-cbind.data.frame(PNAD2009_NE,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2009_NE$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2009_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2009_NE <-cbind.data.frame(PNAD2009_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD2009_NE))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_NE <- 1-sum(Soma)
print(Gini_NE)


###############################################
##                  BAHIA                    ##
###############################################

######&&&&&&&&&#################&&&&&&&&&######
#####FILTRANDO O ESTADO DA BAHIA NORDESTE######
PNAD2009_BA <- filter(PNAD2009, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2009_BA <- PNAD2009_BA[order(PNAD2009_BA$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2009_BA$V4729*PNAD2009_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2009_BA <-cbind.data.frame(PNAD2009_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2009_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2009_BA <-cbind.data.frame(PNAD2009_BA,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2009_BA$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2009_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2009_BA <-cbind.data.frame(PNAD2009_BA,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2009_BA$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2009_BA <-cbind.data.frame(PNAD2009_BA,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2009_BA$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2009_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2009_BA <-cbind.data.frame(PNAD2009_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD2009_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2009)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI

