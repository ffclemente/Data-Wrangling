#Baixando pacotes do R para iniciar manipulação de dados (Data Wrangling)

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "sjPlot", #elaboração de tabelas de contingência
             "FactoMineR", #função 'CA' para elaboração direta da Anacor
             "amap", #funções 'matlogic' e 'burt' para matrizes binária e de Burt
             "ade4") #função 'dudi.acm' para elaboração da ACM

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#Baixar base de dados no site: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

#Acessar planilha pelo R
# R > File > Import Dataset > From excel...

#Após acessar o dataset "COVID_19_geographic..." perceberá que o dataset, além de possuir quase 62 mil linhas, também possui um título muito grande.
#Vamos criar uma cópia do Dataset com outro nome, no caso "COVID19"
COVID19 <- COVID_19_geographic_disbtribution_worldwide_2020_12_14

#removendo Base de dados "antiga" do Environment
rm(COVID_19_geographic_disbtribution_worldwide_2020_12_14)

#Lendo Base de Dados COVID19 no console
COVID19
head(COVID19, 20) #máximo 20 primeiras linhas no console.
tail(COVID19, 20) #máximo 20 últimas linhas no console.

View(COVID19)
dim(COVID19)


#E possivel criar um novo objeto utilizando as funções HEAD e TAIL 
COVID19_head <- head(COVID19, 100) 
COVID19_tail <- tail(COVID19, 100)

#Visiaulizando
COVID19_head %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 10)

#RBIND
COVID19_headtail <- rbind(COVID19_head, COVID19_tail)


#vamos excluir, pois apenas criei para exemplificar os possiveis usos das funções "head" e "tail".
rm(COVID19_head)
rm(COVID19_tail)

#vamos utilizar o dataset COVID19 headtail, para não exigir muito da capacidade computacional, no entanto vamos renomea-lo para COVID19_2.
COVID19_2 <- COVID19_headtail
rm(COVID19_headtail)

#Visualizando dados função KABLE
COVID19_2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 12)

#Filtrando informações 
COVID19_2[COVID19_2$deaths >= 5, ] 
COVID19_deaths <- COVID19_2[COVID19_2$deaths >= 5, ] 

#Filtrando apenas 1 variável
COVID19_deaths1 <- COVID19_2[COVID19_2$deaths >= 5, c("deaths")] 


#Filtrando informações mais de uma variável
COVID19_2[COVID19_2$deaths >= 5 & COVID19_2$cases >= 100, ]
COVID19_deathsxcases <- COVID19_2[COVID19_2$deaths >= 5 & COVID19_2$cases >= 100, ]

#Filtrando apenas 2 variáveis
COVID19_deathsxcases2 <- COVID19_2[COVID19_2$deaths >= 5 & COVID19_2$cases >= 100, c("deaths", "cases")]

#Removendo os exemplos
rm(COVID19_deaths, COVID19_deaths1, COVID19_deaths2, COVID19_deathsxcases, COVID19_deathsxcases2, COVID19_headtail, COVID19_headtail2)



#Excluindo colunas do dataset
COVID19_2 <- COVID19_2 %>% select(-dateRep, -countryterritoryCode, -geold, -popData2019, -continentExp)
#apareceu erro "geold" nao existe. Vamos tentar, sem incluir a coluna "geold"

#Excluindo colunas do dataset
COVID19_2 <- COVID19_2 %>% select(-dateRep, -countryterritoryCode, -popData2019, -continentExp)
COVID19_2 <- COVID19_2 %>% select(-geold)
#Está aparecendo um erro na tentativa de exclusão da coluna "geold", vamos prosseguir mesmo assim.

#Visualizando
COVID19_2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 12)

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

#Visualizando
COVID19_3 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 15)

#Agora sim vamos excluir a coluna "-geold" intitulada "código".
COVID19_3 <- COVID19_3 %>% select(-Código)

#Visualizando
COVID19_3 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 15)


#Ordenando colunas
COVID19_3 %>% select(País, Ano, Mês, Dia, everything()) #NO CONSOLE
COVID19_4 <- COVID19_3 %>% select(País, Ano, Mês, Dia, everything()) #NOVO OBJETO

#visualizando
COVID19_4 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 17)

#Realocando utilizando a função AFTER e BEFORE
COVID19_4 %>% relocate(Casos, .after = Mortes) #NO CONSOLE 
COVID19_5 <- COVID19_4 %>% relocate(Casos, .after = Mortes) 


#Visualizando
COVID19_5 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 17)


COVID19_5 %>% relocate(Casos_cumulativos, .before = Casos) #NO CONSOLE 
COVID19_5 <- COVID19_5 %>% relocate(Casos_cumulativos, .before = Casos) 


#Visualizando
COVID19_5 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 17)


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

#Visualizando
COVID19_5 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 12)


#Alterar conteúdo das observações sem criar uma nova coluna
COVID19_6 <- COVID19_5

COVID19_6 <- mutate(COVID19_6, 
                       País = recode(País,
                                      "Afghanistan" = "Afeganistão"))

#Visualizando  
COVID19_6 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 12)


#Alterar conteúdo das observações criando uma nova coluna
COVID19_6 <- mutate(COVID19_6, 
                    Continente = recode(País,
                                  "Afeganistão" = "Asia",
                                  "Zimbabwe" = "Africa"))   

#visualizando
COVID19_6 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 11)



#Criando nova coluna com operadores binários (0,1 ou sim, não) (REPLACE/ RECODE)
# ????????

#MERGE/ JOIN
# ?????????

#IF, ELSE, IF ELSE
# ???????????

# FOR, WHILE, REPEAT
# ????????????

# MUTATE + CASE WHEN
# ???????????

#HISTOGRAMAS
# ????????????

# GRÁFICOS
# ???????????



