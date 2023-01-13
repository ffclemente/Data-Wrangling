#Baixando pacotes necessários do R para iniciar manipulação de dados e Data Wrangling 

install.packages("tidyverse")
library("tidyverse")

install.packages(read.csv, read.csv2())
library(readxl)


# EXEMPLOS DE BASES DE DADOS DISPONIVEIS NO PRÓPRIO R 

help(package = "datasets")


#Base de dados CARS
cars
?cars
View(cars)
edit(cars)
summary(cars)

#Base de dados MORLEY
morley
?morley
summary(morley)


#Base de dados MTCARS
mtcars
?mtcars
View(mtcars)
edit(mtcars)
summary(mtcars)


#Criando um objeto mtcars para iniciar as edições
mtcars <- mtcars
2
mtcars <- mtcars %>% rename(Milhas_galão = 1,
                                  Cilindradas = 2,
                                  Km_rodados = 3,
                                  Cavalos_potência = 4)


#No entanto, iremos realizar manipulação de dados e Data Wrangling de uma planilha que trata a respeito do Covid, e que foi adiquirida através da internet.
# Utilizei o exemplo do dataset mtcars apenas para mostrar que o próprio R possui datasets para praticar.
# Contudo, vamos excluir o dataset mtcars do Global Environment para prosseguir.
rm(mtcars)


#MANIPULAÇÃO DE DADOS E DATA WRANGLING COM DADOS DA INTERNET 
#EXEMPLO BASE DE DADOS COVID MUNDO
#Baixar base de dados no site: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

#Acessar planilha pelo R
# R > File > Import Dataset > From excel...

#Após acessar o dataset "COVID_19_geographic..." perceberá que o dataset possui mais de 61mil linhas, bem como possui um título muito grande.
#Vamos criar uma cópia do Dataset com outro nome "COVID19"
COVID19 <- COVID_19_geographic_disbtribution_worldwide_2020_12_14

#removendo Base de dados "antiga" do Environment
rm(COVID_19_geographic_disbtribution_worldwide_2020_12_14)

#Lendo Base de Dados COVID19 no console
COVID19
View(COVID19)
data.frame(COVID19)
dim(COVID19)

head(COVID19, 20) #máximo 20 linhas no console.
tail(COVID19, 20) #máximo 20 linhas no console.

#Criando uma cópia para realizar as primeiras edições.
COVID19_2 <- COVID19
COVID19_2

#A base de dados possui quase 62 mil linhas.
#Vamos excluir algumas linhas para realizar as análises, utilizando a função head e tail.
#lembrando que apenas nesse caso vamos excluir pois não há necessidade de exigir da capacidade computacional, visto que apenas vamos utilizar como exemplo para praticar.

COVID19_head <- head(COVID19, 100)
COVID19_tail <- tail(COVID19, 100)


#vamos excluir, pois apenas criei para exemplificar os possiveis usos das funções "head" e "tail".
rm(COVID19_head)
rm(COVID19_tail)

#vamos criar uma cópia da COVID19 com as 100 primeiras linhas do dataset.
COVID19_2 <- head(COVID19, 100)

#Excluindo colunas do dataser
COVID19_2 <- COVID19_2 %>% select(-dateRep, -countryterritoryCode, -popData2019, -continentExp)
COVID19_2 <- COVID19_2 %>% select(-geold)


#Criando uma cópia para renomear variáveis (colunas)
COVID19_3 <- COVID19_2

COVID19_3 <- COVID19_2 %>% rename(Dia = 1,
                                Mês = 2,
                                Ano = 3,
                                Casos = 4,
                                Mortes = 5,
                                País = 6,
                                Código = 7,
                                Casos_cumulativos = 8)


COVID19_3 <- COVID19_3 %>% select(-Código)

#Ordenando colunas
COVID19_3 %>% select(País, everything()) #NO CONSOLE
COVID19_4 <- COVID19_3 %>% select(País, everything())


#Realocando apenas uma coluna (usar before ou after)
COVID19_4 %>% relocate(Casos, .after = Mortes) #NO CONSOLE 
COVID19_5 <- COVID19_4 %>% relocate(Casos, .after = Mortes) 

COVID19_5 %>% relocate(Casos_cumulativos, .before = Casos) #NO CONSOLE 
COVID19_5 <- COVID19_5 %>% relocate(Casos_cumulativos, .before = Casos) 



#cRIANDO UMA COLUNA COM OS QUARTIS EM RELAÇÃO AOS NÚMEROS DE CASOS
COVID19_5 <- COVID19_5 %>% mutate(quartis_casos = cut(Casos,
                                                      c(-Inf, quantile(COVID19_5$Casos,
                                                                       type = 5,
                                                                       probs = c(0.25, 0.50, 0.75),
                                                                       TRUE),Inf),
                                                      c("primeiro quartil",
                                                        "segundo quartil",
                                                        "terceiro quartil",
                                                        "quarto quartil")))

#Criando uma cópia para alterar conteúdos das observações
COVID19_6 <- COVID19_5
View(COVID19_6)
     

#Alterar conteúdo das observações sem criar uma nova coluna
COVID19_6 <- mutate(COVID19_6, 
                       País = recode(País,
                                      "Afghanistan" = "Afeganistão"))
     
#Alterar conteúdo das observações criando uma nova coluna
COVID19_6 <- mutate(COVID19_6, 
                    Continente = recode(País,
                                  "Afeganistão" = "Asia"))   



