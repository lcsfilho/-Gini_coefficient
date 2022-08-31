setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Antiga/2015/dados")

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
PNAD2015 <- read_sav("PES_DOM2015.sav",col_select = c("UF","V4105","V4107","V4729","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD2015)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD2015))
colSums(PNAD2015=="")

#Removendo dados missing:
PNAD2015 <- PNAD2015 %>% drop_na(RDPC)

summary(PNAD2015)

#Contar dados missing
colSums(is.na(PNAD2015))
colSums(PNAD2015=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD2015_BR <- PNAD2015[order(PNAD2015$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2015_BR$V4729*PNAD2015_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2015_BR <-cbind.data.frame(PNAD2015_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2015_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2015_BR <-cbind.data.frame(PNAD2015_BR,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2015_BR$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2015_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2015_BR <-cbind.data.frame(PNAD2015_BR,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2015_BR$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2015_BR <-cbind.data.frame(PNAD2015_BR,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2015_BR$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2015_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2015_BR <-cbind.data.frame(PNAD2015_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD2015_BR))
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
PNAD2015_NE <-filter(PNAD2015, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2015_NE <- PNAD2015_NE[order(PNAD2015_NE$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2015_NE$V4729*PNAD2015_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2015_NE <-cbind.data.frame(PNAD2015_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2015_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2015_NE <-cbind.data.frame(PNAD2015_NE,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2015_NE$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2015_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2015_NE <-cbind.data.frame(PNAD2015_NE,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2015_NE$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2015_NE <-cbind.data.frame(PNAD2015_NE,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2015_NE$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2015_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2015_NE <-cbind.data.frame(PNAD2015_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD2015_NE))
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
PNAD2015_BA <- filter(PNAD2015, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2015_BA <- PNAD2015_BA[order(PNAD2015_BA$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2015_BA$V4729*PNAD2015_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2015_BA <-cbind.data.frame(PNAD2015_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2015_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2015_BA <-cbind.data.frame(PNAD2015_BA,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2015_BA$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2015_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2015_BA <-cbind.data.frame(PNAD2015_BA,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2015_BA$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2015_BA <-cbind.data.frame(PNAD2015_BA,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2015_BA$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2015_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2015_BA <-cbind.data.frame(PNAD2015_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD2015_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2015)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI

