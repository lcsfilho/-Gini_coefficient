setwd("E:/DPS/03. Banco de Dados/PESQUISAS/Censo Demográfico/Censo 2000")

require("foreign")
library('dplyr') # data manipulation
library(tidyr)
library(haven)
# carregando a base como uma lista
#v0102=Código da UF
#V1005=Setor, situação censitária;
#V1004=Região metropolitana;
#P001=P001 da pessoa;
#RDPC=Rendimento mensal domiciliar percapita
CENSO2000 <- read_sav("Pes_Dom_BRASIL.sav",col_select = c("V0102","V1005","V1004","P001","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(CENSO2000)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(CENSO2000))
colSums(CENSO2000=="")

#Removendo dados missing:
CENSO2000 <- CENSO2000 %>% drop_na(RDPC)

summary(CENSO2000)

#Contar dados missing
colSums(is.na(CENSO2000))
colSums(CENSO2000=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
CENSO2000_BR <- CENSO2000[order(CENSO2000$RDPC),]

#Produto doS P001S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- CENSO2000_BR$P001*CENSO2000_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
CENSO2000_BR <-cbind.data.frame(CENSO2000_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(CENSO2000_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
CENSO2000_BR <-cbind.data.frame(CENSO2000_BR,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(CENSO2000_BR$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- CENSO2000_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
CENSO2000_BR <-cbind.data.frame(CENSO2000_BR,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (P001)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(CENSO2000_BR$P001)

#Incluindo a coluna da soma acumulada da frequência:
CENSO2000_BR <-cbind.data.frame(CENSO2000_BR,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(CENSO2000_BR$P001)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- CENSO2000_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
CENSO2000_BR <-cbind.data.frame(CENSO2000_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(CENSO2000_BR))
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
CENSO2000_NE <-filter(CENSO2000, V0102>=21 & V0102<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
CENSO2000_NE <- CENSO2000_NE[order(CENSO2000_NE$RDPC),]

#Produto doS P001S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- CENSO2000_NE$P001*CENSO2000_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
CENSO2000_NE <-cbind.data.frame(CENSO2000_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(CENSO2000_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
CENSO2000_NE <-cbind.data.frame(CENSO2000_NE,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(CENSO2000_NE$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- CENSO2000_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
CENSO2000_NE <-cbind.data.frame(CENSO2000_NE,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (P001)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(CENSO2000_NE$P001)

#Incluindo a coluna da soma acumulada da frequência:
CENSO2000_NE <-cbind.data.frame(CENSO2000_NE,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(CENSO2000_NE$P001)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- CENSO2000_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
CENSO2000_NE <-cbind.data.frame(CENSO2000_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(CENSO2000_NE))
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
CENSO2000_BA <- filter(CENSO2000, V0102==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
CENSO2000_BA <- CENSO2000_BA[order(CENSO2000_BA$RDPC),]

#Produto doS P001S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- CENSO2000_BA$P001*CENSO2000_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
CENSO2000_BA <-cbind.data.frame(CENSO2000_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(CENSO2000_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
CENSO2000_BA <-cbind.data.frame(CENSO2000_BA,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(CENSO2000_BA$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- CENSO2000_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
CENSO2000_BA <-cbind.data.frame(CENSO2000_BA,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (P001)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(CENSO2000_BA$P001)

#Incluindo a coluna da soma acumulada da frequência:
CENSO2000_BA <-cbind.data.frame(CENSO2000_BA,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(CENSO2000_BA$P001)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- CENSO2000_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
CENSO2000_BA <-cbind.data.frame(CENSO2000_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(CENSO2000_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2000)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI
