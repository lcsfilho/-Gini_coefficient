setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Antiga/1998/DADOS")

require("foreign")
library('dplyr') # data manipulation
library(tidyr)
library(haven)
# carregando a base como uma lista
#V4105=C?digo de situa??o censit?ria;
#V4107=C?digo de ?rea censit?ria;
#V4729=V4729 da pessoa;
#RDPC=Rendimento mensal domiciliar percapita
PNAD1998 <- read_sav("PES_DOM98.sav",col_select = c("UF","V4105","V4107","V4729","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD1998)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD1998))
colSums(PNAD1998=="")

#Removendo dados missing:
PNAD1998 <- PNAD1998 %>% drop_na(RDPC)

summary(PNAD1998)

#Contar dados missing
colSums(is.na(PNAD1998))
colSums(PNAD1998=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD1998_BR <- PNAD1998[order(PNAD1998$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD1998_BR$V4729*PNAD1998_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD1998_BR <-cbind.data.frame(PNAD1998_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD1998_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD1998_BR <-cbind.data.frame(PNAD1998_BR,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD1998_BR$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD1998_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD1998_BR <-cbind.data.frame(PNAD1998_BR,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V4729)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD1998_BR$V4729)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD1998_BR <-cbind.data.frame(PNAD1998_BR,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD1998_BR$V4729)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD1998_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD1998_BR <-cbind.data.frame(PNAD1998_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD1998_BR))
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
PNAD1998_NE <-filter(PNAD1998, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD1998_NE <- PNAD1998_NE[order(PNAD1998_NE$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD1998_NE$V4729*PNAD1998_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD1998_NE <-cbind.data.frame(PNAD1998_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD1998_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD1998_NE <-cbind.data.frame(PNAD1998_NE,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD1998_NE$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD1998_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD1998_NE <-cbind.data.frame(PNAD1998_NE,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V4729)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD1998_NE$V4729)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD1998_NE <-cbind.data.frame(PNAD1998_NE,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD1998_NE$V4729)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD1998_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD1998_NE <-cbind.data.frame(PNAD1998_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD1998_NE))
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
PNAD1998_BA <- filter(PNAD1998, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD1998_BA <- PNAD1998_BA[order(PNAD1998_BA$RDPC),]

#Produto doS V4729S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD1998_BA$V4729*PNAD1998_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD1998_BA <-cbind.data.frame(PNAD1998_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD1998_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD1998_BA <-cbind.data.frame(PNAD1998_BA,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD1998_BA$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD1998_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD1998_BA <-cbind.data.frame(PNAD1998_BA,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V4729)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD1998_BA$V4729)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD1998_BA <-cbind.data.frame(PNAD1998_BA,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD1998_BA$V4729)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD1998_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD1998_BA <-cbind.data.frame(PNAD1998_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD1998_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(1998)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI
