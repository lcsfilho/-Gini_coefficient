setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Antiga/2002/dados")

require("foreign")
library('dplyr') # data manipulation
library(tidyr)
library(haven)
# carregando a base como uma lista
#UF=C?digo da UF
#V4105=Situa??o censit?ria;
#V4107=?rea Censit?ria;
#V4729=Peso da pessoa;
#RDPC=Rendimento mensal domiciliar percapita
PNAD2002 <- read_sav("PES_DOM2002.sav",col_select = c("UF","V4105","V4107","V4729","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD2002)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD2002))
colSums(PNAD2002=="")

#Removendo dados missing:
PNAD2002 <- PNAD2002 %>% drop_na(RDPC)

summary(PNAD2002)

#Contar dados missing
colSums(is.na(PNAD2002))
colSums(PNAD2002=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD2002_BR <- PNAD2002[order(PNAD2002$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2002_BR$V4729*PNAD2002_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2002_BR <-cbind.data.frame(PNAD2002_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2002_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2002_BR <-cbind.data.frame(PNAD2002_BR,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2002_BR$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2002_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2002_BR <-cbind.data.frame(PNAD2002_BR,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V4729)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD2002_BR$V4729)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD2002_BR <-cbind.data.frame(PNAD2002_BR,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD2002_BR$V4729)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD2002_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD2002_BR <-cbind.data.frame(PNAD2002_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD2002_BR))
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
###########FILTRANDO REGI?O NORDESTE###########
PNAD2002_NE <-filter(PNAD2002, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2002_NE <- PNAD2002_NE[order(PNAD2002_NE$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2002_NE$V4729*PNAD2002_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2002_NE <-cbind.data.frame(PNAD2002_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2002_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2002_NE <-cbind.data.frame(PNAD2002_NE,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2002_NE$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2002_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2002_NE <-cbind.data.frame(PNAD2002_NE,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V4729)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD2002_NE$V4729)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD2002_NE <-cbind.data.frame(PNAD2002_NE,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD2002_NE$V4729)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD2002_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD2002_NE <-cbind.data.frame(PNAD2002_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD2002_NE))
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
PNAD2002_BA <- filter(PNAD2002, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2002_BA <- PNAD2002_BA[order(PNAD2002_BA$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2002_BA$V4729*PNAD2002_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2002_BA <-cbind.data.frame(PNAD2002_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2002_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2002_BA <-cbind.data.frame(PNAD2002_BA,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2002_BA$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2002_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2002_BA <-cbind.data.frame(PNAD2002_BA,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V4729)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD2002_BA$V4729)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD2002_BA <-cbind.data.frame(PNAD2002_BA,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD2002_BA$V4729)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD2002_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD2002_BA <-cbind.data.frame(PNAD2002_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD2002_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2002)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI
