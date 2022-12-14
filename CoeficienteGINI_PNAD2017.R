setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Cont?nua/Anual/PNADc 2017")

require("foreign")
library('dplyr') # data manipulation
library(tidyr)
library(haven)
# carregando a base como uma lista
#UF=C?digo da UF
#V1022=Situa??o do domic?lio;
#RM_RIDE=Regi?o Metropolitana e Regi?o Administrativa Integrada de Desenvolvimento;
#V1032=Peso do domic?lio e das pessoas (com p?s estratifica??o);
#RDPC=Rendimento mensal domiciliar percapita
PNAD2017 <- read_sav("PNADC_2017_visita1.sav",col_select = c("UF","V1022","RM_RIDE","V1032","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD2017)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD2017))
colSums(PNAD2017=="")

#Removendo dados missing:
PNAD2017 <- PNAD2017 %>% drop_na(RDPC)

summary(PNAD2017)

#Contar dados missing
colSums(is.na(PNAD2017))
colSums(PNAD2017=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD2017_BR <- PNAD2017[order(PNAD2017$RDPC),]

#Produto doS V1032S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2017_BR$V1032*PNAD2017_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2017_BR <-cbind.data.frame(PNAD2017_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2017_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2017_BR <-cbind.data.frame(PNAD2017_BR,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2017_BR$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2017_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2017_BR <-cbind.data.frame(PNAD2017_BR,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V1032)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD2017_BR$V1032)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD2017_BR <-cbind.data.frame(PNAD2017_BR,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD2017_BR$V1032)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD2017_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD2017_BR <-cbind.data.frame(PNAD2017_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD2017_BR))
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
PNAD2017_NE <-filter(PNAD2017, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2017_NE <- PNAD2017_NE[order(PNAD2017_NE$RDPC),]

#Produto doS V1032S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2017_NE$V1032*PNAD2017_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2017_NE <-cbind.data.frame(PNAD2017_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2017_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2017_NE <-cbind.data.frame(PNAD2017_NE,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2017_NE$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2017_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2017_NE <-cbind.data.frame(PNAD2017_NE,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V1032)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD2017_NE$V1032)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD2017_NE <-cbind.data.frame(PNAD2017_NE,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD2017_NE$V1032)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD2017_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD2017_NE <-cbind.data.frame(PNAD2017_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD2017_NE))
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
PNAD2017_BA <- filter(PNAD2017, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2017_BA <- PNAD2017_BA[order(PNAD2017_BA$RDPC),]

#Produto doS V1032S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2017_BA$V1032*PNAD2017_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2017_BA <-cbind.data.frame(PNAD2017_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2017_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2017_BA <-cbind.data.frame(PNAD2017_BA,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2017_BA$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2017_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2017_BA <-cbind.data.frame(PNAD2017_BA,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V1032)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(PNAD2017_BA$V1032)

#Incluindo a coluna da soma acumulada da frequ?ncia:
PNAD2017_BA <-cbind.data.frame(PNAD2017_BA,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(PNAD2017_BA$V1032)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- PNAD2017_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
PNAD2017_BA <-cbind.data.frame(PNAD2017_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD2017_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2017)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI

