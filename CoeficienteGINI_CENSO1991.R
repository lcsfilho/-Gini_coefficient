setwd("E:/DPS/03. Banco de Dados/PESQUISAS/Censo Demogr?fico/Censo 1991")

require("foreign")
library('dplyr') # data manipulation
library(tidyr)
library(haven)
# carregando a base como uma lista
#Peso=n?mero stimado de pessoas;
#V1061=situa??o do domic?lio 
#v7003=Regi?o Metropolitana
#RDPC=Rendimento mensal domiciliar per capita
CENSO1991 <- read_sav("microdados_pessoa_domicilio_1991.sav",col_select = c("UF","v1061","v7003","Peso","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(CENSO1991)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(CENSO1991))
colSums(CENSO1991=="")

#Removendo dados missing:
CENSO1991 <- CENSO1991 %>% drop_na(RDPC)

summary(CENSO1991)

#Contar dados missing
colSums(is.na(CENSO1991))
colSums(CENSO1991=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
CENSO1991_BR <- CENSO1991[order(CENSO1991$RDPC),]

#Produto doS PESOS pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- CENSO1991_BR$Peso*CENSO1991_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
CENSO1991_BR <-cbind.data.frame(CENSO1991_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(CENSO1991_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
CENSO1991_BR <-cbind.data.frame(CENSO1991_BR,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(CENSO1991_BR$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- CENSO1991_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
CENSO1991_BR <-cbind.data.frame(CENSO1991_BR,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (PESO)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(CENSO1991_BR$Peso)

#Incluindo a coluna da soma acumulada da frequ?ncia:
CENSO1991_BR <-cbind.data.frame(CENSO1991_BR,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(CENSO1991_BR$Peso)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- CENSO1991_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
CENSO1991_BR <-cbind.data.frame(CENSO1991_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(CENSO1991_BR))
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
CENSO1991_NE <-filter(CENSO1991, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
CENSO1991_NE <- CENSO1991_NE[order(CENSO1991_NE$RDPC),]

#Produto doS PESOS pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- CENSO1991_NE$Peso*CENSO1991_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
CENSO1991_NE <-cbind.data.frame(CENSO1991_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(CENSO1991_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
CENSO1991_NE <-cbind.data.frame(CENSO1991_NE,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(CENSO1991_NE$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- CENSO1991_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
CENSO1991_NE <-cbind.data.frame(CENSO1991_NE,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (PESO)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(CENSO1991_NE$Peso)

#Incluindo a coluna da soma acumulada da frequ?ncia:
CENSO1991_NE <-cbind.data.frame(CENSO1991_NE,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(CENSO1991_NE$Peso)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- CENSO1991_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
CENSO1991_NE <-cbind.data.frame(CENSO1991_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(CENSO1991_NE))
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
CENSO1991_BA <- filter(CENSO1991, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
CENSO1991_BA <- CENSO1991_BA[order(CENSO1991_BA$RDPC),]

#Produto doS PESOS pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- CENSO1991_BA$Peso*CENSO1991_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
CENSO1991_BA <-cbind.data.frame(CENSO1991_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(CENSO1991_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
CENSO1991_BA <-cbind.data.frame(CENSO1991_BA,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(CENSO1991_BA$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- CENSO1991_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
CENSO1991_BA <-cbind.data.frame(CENSO1991_BA,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (PESO)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(CENSO1991_BA$Peso)

#Incluindo a coluna da soma acumulada da frequ?ncia:
CENSO1991_BA <-cbind.data.frame(CENSO1991_BA,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(CENSO1991_BA$Peso)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- CENSO1991_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
CENSO1991_BA <-cbind.data.frame(CENSO1991_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(CENSO1991_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(1991)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI
