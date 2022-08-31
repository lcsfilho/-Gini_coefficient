setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Contínua/Anual/PNADc 2016")

require("foreign")
library('dplyr') # data manipulation
library(tidyr)
library(haven)
# carregando a base como uma lista
#UF=Código da UF
#V1022=Situação do domicílio;
#RM_RIDE=Região Metropolitana e Região Administrativa Integrada de Desenvolvimento;
#V1032=Peso do domicílio e das pessoas (com pós estratificação);
#RDPC=Rendimento mensal domiciliar percapita
PNAD2016 <- read_sav("PNADC_2016_visita1.sav",col_select = c("UF","V1022","RM_RIDE","V1032","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD2016)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD2016))
colSums(PNAD2016=="")

#Removendo dados missing:
PNAD2016 <- PNAD2016 %>% drop_na(RDPC)

summary(PNAD2016)

#Contar dados missing
colSums(is.na(PNAD2016))
colSums(PNAD2016=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD2016_BR <- PNAD2016[order(PNAD2016$RDPC),]

#Produto doS V1032S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2016_BR$V1032*PNAD2016_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2016_BR <-cbind.data.frame(PNAD2016_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2016_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2016_BR <-cbind.data.frame(PNAD2016_BR,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2016_BR$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2016_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2016_BR <-cbind.data.frame(PNAD2016_BR,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V1032)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2016_BR$V1032)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2016_BR <-cbind.data.frame(PNAD2016_BR,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2016_BR$V1032)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2016_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2016_BR <-cbind.data.frame(PNAD2016_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD2016_BR))
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
PNAD2016_NE <-filter(PNAD2016, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2016_NE <- PNAD2016_NE[order(PNAD2016_NE$RDPC),]

#Produto doS V1032S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2016_NE$V1032*PNAD2016_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2016_NE <-cbind.data.frame(PNAD2016_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2016_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2016_NE <-cbind.data.frame(PNAD2016_NE,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2016_NE$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2016_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2016_NE <-cbind.data.frame(PNAD2016_NE,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V1032)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2016_NE$V1032)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2016_NE <-cbind.data.frame(PNAD2016_NE,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2016_NE$V1032)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2016_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2016_NE <-cbind.data.frame(PNAD2016_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD2016_NE))
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
PNAD2016_BA <- filter(PNAD2016, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2016_BA <- PNAD2016_BA[order(PNAD2016_BA$RDPC),]

#Produto doS V1032S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2016_BA$V1032*PNAD2016_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2016_BA <-cbind.data.frame(PNAD2016_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2016_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2016_BA <-cbind.data.frame(PNAD2016_BA,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2016_BA$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2016_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2016_BA <-cbind.data.frame(PNAD2016_BA,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V1032)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2016_BA$V1032)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2016_BA <-cbind.data.frame(PNAD2016_BA,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2016_BA$V1032)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2016_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da freq uência:
PNAD2016_BA <-cbind.data.frame(PNAD2016_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD2016_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2016)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI

