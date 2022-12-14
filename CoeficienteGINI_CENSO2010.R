setwd("E:/DPS/03. Banco de Dados/PESQUISAS/Censo Demogr?fico/Censo 2010")

require("foreign")
library('dplyr') # data manipulation
library(tidyr)
library(haven)
# carregando a base como uma lista
#V0001=C?digo da unidade da federa??o
#V1004=C?digo da Regi?o Metropolitana;
#V1006=Situa??o do domic?lio;
#V0010=Peso amostral;
#V6531=Rendimento m?dio domiciliar percapita
CENSO2010 <- read_sav("Amostra_Domicilios_Brasil.sav",col_select = c("V0001","V1004","V1006","V0010","V6531"))


#########################################
###ANALISE DO CONJUNTO DE DADOS NO GERAL
#########################################
summary(CENSO2010)

###DADOS MISSING######

#Contar dados missing
colSums(is.na(CENSO2010))
colSums(CENSO2010=="")

#Removendo dados missing:
CENSO2010 <- CENSO2010 %>% drop_na(V6531)

summary(CENSO2010)

#Contar dados missing
colSums(is.na(CENSO2010))
colSums(CENSO2010=="")


###############################################
##                  BRASIL                   ##
###############################################

###############################################
#RENDIMENTO
###############################################


# Ordenando os dados pelo V6531:
CENSO2010_BR <- CENSO2010[order(CENSO2010$V6531),]

#Produto doS V0010S pelos V6531 (RENDIMENTO TOTAL):
Rend_Total <- CENSO2010_BR$V0010*CENSO2010_BR$V6531

# Incluindo a coluna do RENDIMENTO TOTAL
CENSO2010_BR <-cbind.data.frame(CENSO2010_BR,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(CENSO2010_BR$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
CENSO2010_BR <-cbind.data.frame(CENSO2010_BR,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(CENSO2010_BR$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- CENSO2010_BR$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
CENSO2010_BR <-cbind.data.frame(CENSO2010_BR,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V0010)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(CENSO2010_BR$V0010)

#Incluindo a coluna da soma acumulada da frequ?ncia:
CENSO2010_BR <-cbind.data.frame(CENSO2010_BR,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(CENSO2010_BR$V0010)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- CENSO2010_BR$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
CENSO2010_BR <-cbind.data.frame(CENSO2010_BR,Perc_FreqAcumulada)


###############################################
#COEFICIENTE DE GINI - BRASIL
###############################################

Soma=0
n <- as.numeric(count(CENSO2010_BR))
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
CENSO2010_NE <-filter(CENSO2010, V0001>=21 & V0001<=29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo V6531:
CENSO2010_NE <- CENSO2010_NE[order(CENSO2010_NE$V6531),]

#Produto doS V0010S pelos V6531 (RENDIMENTO TOTAL):
Rend_Total <- CENSO2010_NE$V0010*CENSO2010_NE$V6531

# Incluindo a coluna do RENDIMENTO TOTAL
CENSO2010_NE <-cbind.data.frame(CENSO2010_NE,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(CENSO2010_NE$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
CENSO2010_NE <-cbind.data.frame(CENSO2010_NE,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(CENSO2010_NE$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- CENSO2010_NE$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
CENSO2010_NE <-cbind.data.frame(CENSO2010_NE,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V0010)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(CENSO2010_NE$V0010)

#Incluindo a coluna da soma acumulada da frequ?ncia:
CENSO2010_NE <-cbind.data.frame(CENSO2010_NE,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(CENSO2010_NE$V0010)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- CENSO2010_NE$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
CENSO2010_NE <-cbind.data.frame(CENSO2010_NE,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - NORDESTE
###############################################

Soma=0
n <- as.numeric(count(CENSO2010_NE))
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
CENSO2010_BA <- filter(CENSO2010, V0001==29)


###############################################
#RENDIMENTO
###############################################

# Ordenando os dados pelo V6531:
CENSO2010_BA <- CENSO2010_BA[order(CENSO2010_BA$V6531),]

#Produto doS V0010S pelos V6531 (RENDIMENTO TOTAL):
Rend_Total <- CENSO2010_BA$V0010*CENSO2010_BA$V6531

# Incluindo a coluna do RENDIMENTO TOTAL
CENSO2010_BA <-cbind.data.frame(CENSO2010_BA,Rend_Total)

#Soma acumulada do RENDIMENTO TOTAL:
RendTotal_Acumulado <- cumsum(CENSO2010_BA$Rend_Total)

#Incluindo a coluna do RENDIMENTO TOTAL ACUMULADO:
CENSO2010_BA <-cbind.data.frame(CENSO2010_BA,RendTotal_Acumulado)

#Somat?rio do RENDIMENTO TOTAL:
Soma_RendTotal <- sum(CENSO2010_BA$Rend_Total)

#Propor??o acumulada do RENDIMENTO TOTAL:
Perc_RendTotalAcumulado <- CENSO2010_BA$RendTotal_Acumulado/Soma_RendTotal

#Incluindo a coluna do Percentual acumulado do Rendimento
CENSO2010_BA <-cbind.data.frame(CENSO2010_BA,Perc_RendTotalAcumulado)


###############################################
#FREQU?NCIA (V0010)
###############################################

#Soma acumulada da FREQU?NCIA:
FrequenciaAcumulada <- cumsum(CENSO2010_BA$V0010)

#Incluindo a coluna da soma acumulada da frequ?ncia:
CENSO2010_BA <-cbind.data.frame(CENSO2010_BA,FrequenciaAcumulada)

#Somat?rio da FREQU?NCIA:
TotalFrequencia <- sum(CENSO2010_BA$V0010)

#Propor??o acumulada da FREQU?NCIA:
Perc_FreqAcumulada <- CENSO2010_BA$FrequenciaAcumulada/TotalFrequencia

#Incluindo a Propor?ao acumulada da freq u?ncia:
CENSO2010_BA <-cbind.data.frame(CENSO2010_BA,Perc_FreqAcumulada)

###############################################
#COEFICIENTE DE GINI - BAHIA
###############################################

Soma=0
n <- as.numeric(count(CENSO2010_BA))
for (k in 0:n-1)
{
  Soma[k] <- (Perc_FreqAcumulada[k+1] - Perc_FreqAcumulada[k])*(Perc_RendTotalAcumulado[k+1] + Perc_RendTotalAcumulado[k])
}
Gini_BA <- 1-sum(Soma)
print(Gini_BA)


###############################################
# MONTANDO VETOR DO cOEFICIENTE DE GINI
###############################################

ANO=c(2010)
BRASIL=c(Gini_BR)
NORDESTE=c(Gini_NE)
BAHIA=c(Gini_BA)

COEFICINTE_GINI <- cbind(ANO, BRASIL, NORDESTE, BAHIA)
COEFICINTE_GINI

