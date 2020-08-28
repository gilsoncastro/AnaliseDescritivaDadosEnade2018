#carregando pacotes
library(ggplot2)
library(readr)
library(e1071)
library(plotly)
require(dplyr)
require(Hmisc)
require(esquisse)
require(devtools)
require(readr)
require(ggplot2)
require(e1071)
require(plotly)
library(tidyverse)

#verificação de pasta destino
setwd("C:/Users/gilso/OneDrive/Documentos/enade2018")
getwd()

#carregando a base de dados
base_enade=read.table("microdados_enade_2018.txt",header = TRUE,sep = ";",dec =",",colClasses = c(NT_OBJ_FG="numeric"))
#visualizando completa a base em outra aba
View(base_enade)
# verificação da estrutura do conjunto de dados
str(base_enade)


#Transformação , manipulaação e limpeza dos dados

# aqui vamos selecionar as variavéis de interesse no nosso conjunto de dados
dados=base_enade%>%dplyr::select(NT_OBJ_FG,
                                 CO_GRUPO,
                                 CO_REGIAO_CURSO,
                                 QE_I02,
                                 CO_TURNO_GRADUACAO,CO_UF_CURSO)
View(dados)
# aqui filtrei um curso para analisar nesse caso, foi escolhido ciências econômicas
bd_enade=dados%>%filter(CO_GRUPO==13)
View(bd_enade)
# transformação do codigo 13 em ciência econômicas
bd_enade=bd_enade%>% mutate (CURSO = case_when (CO_GRUPO == 13 ~ "CIÊNCIAS ECONÔMICAS"))
View(bd_enade)
# aqui uso a função novamente para transformar e classificar  CO_REGIAO_CURSO 
bd_enade=bd_enade%>% mutate (REGIAO = case_when (CO_REGIAO_CURSO == 1 ~ "Norte",
                                                 CO_REGIAO_CURSO == 2 ~ "Nordeste",
                                                 CO_REGIAO_CURSO == 3 ~ "Sudeste",
                                                 CO_REGIAO_CURSO == 4 ~ "Sul",
                                                 CO_REGIAO_CURSO == 5 ~ "Centro-Oeste" ))
View(bd_enade)
#transformação  e classificação da QE_I02 em seus respectivos nomes

bd_enade=bd_enade%>% mutate (RACA = case_when (QE_I02 == "A" ~ "Branca",
                                               QE_I02 == "B" ~ "Preta",
                                               QE_I02 == "C" ~ "Amarela",
                                               QE_I02 == "D" ~ "Parda",
                                               QE_I02 == "E" ~ "Indígena",
                                               QE_I02 == "F" ~ "Não quero declarar" ))
View(bd_enade)
#transformaçao de turno
bd_enade=bd_enade%>% mutate (TURNO = case_when (CO_TURNO_GRADUACAO == 1 ~ "Matutino",
                                                CO_TURNO_GRADUACAO == 2 ~ "Vespertino",
                                                CO_TURNO_GRADUACAO == 3 ~ "Integral",
                                                CO_TURNO_GRADUACAO == 4 ~ "Noturno" ))
#Tranformação da variavel cod_uf_curso em nome dos estados
bd_enade=bd_enade%>% mutate(ESTADOS = case_when (CO_UF_CURSO == 11 ~"RO",
                                                 CO_UF_CURSO == 12 ~ "AC",
                                                 CO_UF_CURSO == 13 ~ "AM",
                                                 CO_UF_CURSO == 14 ~ "RR",
                                                 CO_UF_CURSO == 15 ~ "PA",
                                                 CO_UF_CURSO == 16 ~ "AP",
                                                 CO_UF_CURSO == 17 ~ "TO",
                                                 CO_UF_CURSO == 21 ~ "MA",
                                                 CO_UF_CURSO == 22 ~ "PI",
                                                 CO_UF_CURSO == 23 ~ "CE",
                                                 CO_UF_CURSO == 24 ~ "RN",
                                                 CO_UF_CURSO == 25 ~ "PB",
                                                 CO_UF_CURSO == 26 ~ "PE",
                                                 CO_UF_CURSO == 27 ~ "AL",
                                                 CO_UF_CURSO == 28 ~ "SE",
                                                 CO_UF_CURSO == 29 ~ "BA",
                                                 CO_UF_CURSO == 31 ~ "MG",
                                                 CO_UF_CURSO == 32 ~ "ES",
                                                 CO_UF_CURSO == 33 ~ "RJ",
                                                 CO_UF_CURSO == 35 ~ "SP",
                                                 CO_UF_CURSO == 41 ~ "PR",
                                                 CO_UF_CURSO == 42 ~ "sc",
                                                 CO_UF_CURSO == 43 ~ "RS",
                                                 CO_UF_CURSO == 50 ~ "MS",
                                                 CO_UF_CURSO == 51 ~ "MT",
                                                 CO_UF_CURSO == 52 ~ "GO",
                                                 CO_UF_CURSO == 53 ~ "DF"
))
#nomeando a variavel nota
names(bd_enade)[1] = "NOTAS"
View(bd_enade)
#excluindo variaveis antigas da base
bd_enade=bd_enade[,-c(2,3,4,5)]




#agora que fizemos uma limpeza e transformação dos dados incia-se uma análise descritiva                                            

-----------------------------------------------------------------------------------------
  #uma forma de apresentar as estatisticas resumidas 
  describe(bd_enade)
#resumo do nosso conjunto de dados
summary(bd_enade) 

#se tiver valores faltantes vamos exclui-los com os seguinte comando
bd_enade=bd_enade%>%na.omit()
summary(bd_enade) #foi removido todos valores faltantes 
#mediana das notas do enade
mediana_enade=median(bd_enade$NOTAS)
mediana_enade
#media das notas
media_notas=mean(bd_enade$NOTAS)
media_notas
freq=table(bd_enade$NOTAS)
#valor maximo de notas
max(freq)
#valor minimo
min(freq)
# verificando a moda
valor_maximo=max(freq)
#frequencia das notas
m=names(freq)
#moda das notas
moda=m[freq==valor_maximo]
moda=as.numeric(moda)
moda
#calculando a amplitude das notas
amplitude_notas=max(bd_enade$NOTAS)-min(bd_enade$NOTAS)
amplitude_notas
#apresentando a variância da notas
variancia=var(bd_enade$NOTAS)
variancia
#desvio padrão
desvio=sd(bd_enade$NOTAS)
desvio
#calculando o coeficiente de variação
CV=desvio/media_notas
CV
#curtose dos dados
curtose=kurtosis(bd_enade$NOTAS)
curtose
#assimetria dos dados
ASM= skewness(bd_enade$NOTAS)
ASM
resumo_geral=c(media_notas,mediana_enade,amplitude_notas,variancia,desvio,CV,curtose,ASM,moda)
resumo_geral
#comentários sobre a análise aqui

#grafico de densidade das notas
hist_notas=ggplot(bd_enade, aes(x = NOTAS)) +
  geom_histogram(color = "black", fill = "green", bins = 10) +
  geom_density(col = 2, size = 1, aes(y = 5 * ..count..)) +
  labs(title="Histograma e Curva de Densidade das Notas dos Alunos de Ciências Econômicas")+
  labs(x = "Notas", y = "Freqüência")
hist_notas 
-------------------------------------
  #grafico de densidade das notas
  bd_enade%>%
  filter( NOTAS<200 )%>%
  ggplot( aes(x=NOTAS)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
#grafico de boxplot de notas x estados
bd_enade %>%
  mutate(class = fct_reorder(ESTADOS, NOTAS, .fun='length' )) %>%
  ggplot( aes(x=ESTADOS, y=NOTAS, fill=class)) + 
  geom_boxplot() +
  xlab("ESTADOS") +
  theme(legend.position="none") +
  xlab("") +
  xlab("")



#verficação da frequência e proporção TURNO
table(bd_enade$TURNO)
prop.table(table(bd_enade$TURNO))

table(bd_enade$ESTADOS)

#Verficação da frequência e proporção TURNO X REGIÃO
table(bd_enade$TURNO,bd_enade$REGIAO)
prop.table(table(bd_enade$TURNO,bd_enade$REGIAO))
#verficando a freq e prop raça x regiao
table(bd_enade$RACA,bd_enade$REGIAO)
prop.table(table(bd_enade$RACA,bd_enade$REGIAO))
#agregação de dados 

NOTAS_TURNO=bd_enade%>%select(TURNO,NOTAS)%>%
  group_by (TURNO) %>%
  summarise (MEDIA=mean(NOTAS))

NOTAS_ESTADOS=bd_enade%>%select(ESTADOS,NOTAS)%>%
  group_by (ESTADOS) %>%
  summarise (MEDIA=mean(NOTAS))
NOTAS_ESTADOS

---------------------------------------
  
  NOTA_TUR_REG = bd_enade %>% select (TURNO, REGIAO, NOTAS) %>%
  group_by (TURNO, REGIAO) %>%
  summarise (MEDIA = mean(NOTAS)) 

NTR = bd_enade%>%select(TURNO,REGIAO,NOTAS)%>%
  group_by(TURNO,REGIAO)%>%
  summarise(MEDIA=mean(NOTAS))
NTR


