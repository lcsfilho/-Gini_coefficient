setwd("E:/DPS/03. Banco de Dados/PESQUISAS/PNAD Contínua/Anual/PNADc 2019/Visita 1")

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
PNAD2019 <- read_sav("PNADC_2019_visita1.sav",col_select = c("UF","V1022","RM_RIDE","V1032","RDPC"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(PNAD2019)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(PNAD2019))
colSums(PNAD2019=="")

#Removendo dados missing:
PNAD2019 <- PNAD2019 %>% drop_na(RDPC)

summary(PNAD2019)

#Contar dados missing
colSums(is.na(PNAD2019))
colSums(PNAD2019=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo RDPC:
PNAD2019_BR <- PNAD2019[order(PNAD2019$RDPC),]

#Produto doS V1032S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2019_BR$V1032*PNAD2019_BR$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2019_BR <-cbind.data.frame(PNAD2019_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2019_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2019_BR <-cbind.data.frame(PNAD2019_BR,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2019_BR$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2019_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2019_BR <-cbind.data.frame(PNAD2019_BR,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V1032)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2019_BR$V1032)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2019_BR <-cbind.data.frame(PNAD2019_BR,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2019_BR$V1032)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2019_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da frequência:
PNAD2019_BR <-cbind.data.frame(PNAD2019_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(PNAD2019_BR))
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
PNAD2019_NE <-filter(PNAD2019, UF>=21 & UF<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2019_NE <- PNAD2019_NE[order(PNAD2019_NE$RDPC),]

#Produto doS V1032S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2019_NE$V1032*PNAD2019_NE$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2019_NE <-cbind.data.frame(PNAD2019_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2019_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2019_NE <-cbind.data.frame(PNAD2019_NE,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2019_NE$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2019_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2019_NE <-cbind.data.frame(PNAD2019_NE,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V1032)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2019_NE$V1032)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2019_NE <-cbind.data.frame(PNAD2019_NE,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2019_NE$V1032)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2019_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da frequência:
PNAD2019_NE <-cbind.data.frame(PNAD2019_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(PNAD2019_NE))
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
PNAD2019_BA <- filter(PNAD2019, UF==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo RDPC:
PNAD2019_BA <- PNAD2019_BA[order(PNAD2019_BA$RDPC),]

#Produto doS V1032S pelos RDPC (RENDIMENTO TOTAL):
Rend_Total <- PNAD2019_BA$V1032*PNAD2019_BA$RDPC

# Incluindo a coluna do RENDIMENTO TOTAL
PNAD2019_BA <-cbind.data.frame(PNAD2019_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(PNAD2019_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
PNAD2019_BA <-cbind.data.frame(PNAD2019_BA,RendTotal_Acumulado)

#Somatório do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(PNAD2019_BA$Rend_Total)

#Proporção acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- PNAD2019_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
PNAD2019_BA <-cbind.data.frame(PNAD2019_BA,Perc_RendTotalAcumulado)


###############################################
#FREQUÊNCIA (V1032)
###############################################

#Soma acumulada da FREQUÊNCIA:
FrequenciaAcumulada <- cumsum(PNAD2019_BA$V1032)

#Incluindo a coluna da soma acumulada da frequência:
PNAD2019_BA <-cbind.data.frame(PNAD2019_BA,FrequenciaAcumulada)

#Somatório da FREQUÊNCIA:
TotalFrequencia <- sum(PNAD2019_BA$V1032)

#Proporção acumulada da FREQUÊNCIA:
Perc_FreqAcumulada <- PNAD2019_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Proporçao acumulada da frequência:
PNAD2019_BA <-cbind.data.frame(PNAD2019_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(PNAD2019_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2019)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI

