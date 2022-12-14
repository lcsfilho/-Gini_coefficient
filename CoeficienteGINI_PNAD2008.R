setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Antiga/2008/dados")

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
PNAD2008 <- read_sav("PES_DOM2008.sav",col_select = c("UF","V4105","V4107","V4729","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD2008)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD2008))
colSums(PNAD2008=="")

#Removendo dados missing:
PNAD2008 <- PNAD2008 %>% drop_na(RDPC)

summary(PNAD2008)

#Contar dados missing
colSums(is.na(PNAD2008))
colSums(PNAD2008=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD2008_BR <- PNAD2008[order(PNAD2008$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2008_BR$V4729*PNAD2008_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2008_BR <-cbind.data.frame(PNAD2008_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2008_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2008_BR <-cbind.data.frame(PNAD2008_BR,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2008_BR$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2008_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2008_BR <-cbind.data.frame(PNAD2008_BR,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V4729)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD2008_BR$V4729)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD2008_BR <-cbind.data.frame(PNAD2008_BR,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD2008_BR$V4729)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD2008_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD2008_BR <-cbind.data.frame(PNAD2008_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD2008_BR))
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
PNAD2008_NE <-filter(PNAD2008, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2008_NE <- PNAD2008_NE[order(PNAD2008_NE$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2008_NE$V4729*PNAD2008_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2008_NE <-cbind.data.frame(PNAD2008_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2008_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2008_NE <-cbind.data.frame(PNAD2008_NE,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2008_NE$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2008_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2008_NE <-cbind.data.frame(PNAD2008_NE,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V4729)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD2008_NE$V4729)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD2008_NE <-cbind.data.frame(PNAD2008_NE,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD2008_NE$V4729)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD2008_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD2008_NE <-cbind.data.frame(PNAD2008_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD2008_NE))
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
PNAD2008_BA <- filter(PNAD2008, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2008_BA <- PNAD2008_BA[order(PNAD2008_BA$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2008_BA$V4729*PNAD2008_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2008_BA <-cbind.data.frame(PNAD2008_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2008_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2008_BA <-cbind.data.frame(PNAD2008_BA,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2008_BA$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2008_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2008_BA <-cbind.data.frame(PNAD2008_BA,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V4729)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD2008_BA$V4729)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD2008_BA <-cbind.data.frame(PNAD2008_BA,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD2008_BA$V4729)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD2008_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD2008_BA <-cbind.data.frame(PNAD2008_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD2008_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2008)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI
