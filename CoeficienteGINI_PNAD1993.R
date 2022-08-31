setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Antiga/1993/DADOS")

require("foreign")
library('dplyr') # data manipulation
library(tidyr)
library(haven)
# carregando a base como uma lista
#V4105=Código de situação censitária;
#V4107=Código de área censitária;
#V4729=V4729 da pessoa;
#RDPC=Rendimento mensal domiciliar percapita
PNAD1993 <- read_sav("PES_DOM93.sav",col_select = c("UF","V4105","V4107","V4729","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD1993)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD1993))
colSums(PNAD1993=="")

#Removendo dados missing:
PNAD1993 <- PNAD1993 %>% drop_na(RDPC)

summary(PNAD1993)

#Contar dados missing
colSums(is.na(PNAD1993))
colSums(PNAD1993=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD1993_BR <- PNAD1993[order(PNAD1993$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD1993_BR$V4729*PNAD1993_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD1993_BR <-cbind.data.frame(PNAD1993_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD1993_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD1993_BR <-cbind.data.frame(PNAD1993_BR,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD1993_BR$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD1993_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD1993_BR <-cbind.data.frame(PNAD1993_BR,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD1993_BR$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD1993_BR <-cbind.data.frame(PNAD1993_BR,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD1993_BR$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD1993_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD1993_BR <-cbind.data.frame(PNAD1993_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD1993_BR))
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
PNAD1993_NE <-filter(PNAD1993, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD1993_NE <- PNAD1993_NE[order(PNAD1993_NE$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD1993_NE$V4729*PNAD1993_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD1993_NE <-cbind.data.frame(PNAD1993_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD1993_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD1993_NE <-cbind.data.frame(PNAD1993_NE,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD1993_NE$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD1993_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD1993_NE <-cbind.data.frame(PNAD1993_NE,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD1993_NE$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD1993_NE <-cbind.data.frame(PNAD1993_NE,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD1993_NE$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD1993_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD1993_NE <-cbind.data.frame(PNAD1993_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD1993_NE))
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
########  FILTRANDO O ESTADO DA BAHIA   #######
PNAD1993_BA <- filter(PNAD1993, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD1993_BA <- PNAD1993_BA[order(PNAD1993_BA$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD1993_BA$V4729*PNAD1993_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD1993_BA <-cbind.data.frame(PNAD1993_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD1993_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD1993_BA <-cbind.data.frame(PNAD1993_BA,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD1993_BA$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD1993_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD1993_BA <-cbind.data.frame(PNAD1993_BA,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V4729)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD1993_BA$V4729)

#Incluindo a coluna da soma acumulada da frequência:
PNAD1993_BA <-cbind.data.frame(PNAD1993_BA,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD1993_BA$V4729)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD1993_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD1993_BA <-cbind.data.frame(PNAD1993_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD1993_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(1993)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI
