setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Antiga/2006/dados")

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
PNAD2006 <- read_sav("PES_DOM2006.sav",col_select = c("UF","V4105","V4107","V4729","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD2006)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD2006))
colSums(PNAD2006=="")

#Removendo dados missing:
PNAD2006 <- PNAD2006 %>% drop_na(RDPC)

summary(PNAD2006)

#Contar dados missing
colSums(is.na(PNAD2006))
colSums(PNAD2006=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD2006_BR <- PNAD2006[order(PNAD2006$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2006_BR$V4729*PNAD2006_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2006_BR <-cbind.data.frame(PNAD2006_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2006_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2006_BR <-cbind.data.frame(PNAD2006_BR,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2006_BR$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2006_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2006_BR <-cbind.data.frame(PNAD2006_BR,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2006_BR$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2006_BR <-cbind.data.frame(PNAD2006_BR,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2006_BR$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2006_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2006_BR <-cbind.data.frame(PNAD2006_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD2006_BR))
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
PNAD2006_NE <-filter(PNAD2006, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2006_NE <- PNAD2006_NE[order(PNAD2006_NE$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2006_NE$V4729*PNAD2006_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2006_NE <-cbind.data.frame(PNAD2006_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2006_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2006_NE <-cbind.data.frame(PNAD2006_NE,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2006_NE$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2006_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2006_NE <-cbind.data.frame(PNAD2006_NE,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2006_NE$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2006_NE <-cbind.data.frame(PNAD2006_NE,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2006_NE$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2006_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2006_NE <-cbind.data.frame(PNAD2006_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD2006_NE))
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
PNAD2006_BA <- filter(PNAD2006, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2006_BA <- PNAD2006_BA[order(PNAD2006_BA$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2006_BA$V4729*PNAD2006_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2006_BA <-cbind.data.frame(PNAD2006_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2006_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2006_BA <-cbind.data.frame(PNAD2006_BA,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2006_BA$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2006_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2006_BA <-cbind.data.frame(PNAD2006_BA,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2006_BA$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2006_BA <-cbind.data.frame(PNAD2006_BA,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2006_BA$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2006_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2006_BA <-cbind.data.frame(PNAD2006_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD2006_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2006)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI
