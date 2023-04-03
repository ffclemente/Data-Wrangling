#Baixando pacotes necessários

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "sjPlot", #elaboração de tabelas de contingência
             "FactoMineR", #função 'CA' para elaboração direta da Anacor
             "amap", #funções 'matlogic' e 'burt' para matrizes binária e de Burt
             "ade4", #função 'dudi.acm' para elaboração da ACM
             "DescTools", #ADICIONAL
             "dplyr")#ADICIONAL


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Exemplos de Bases de Dados (Datasets) disponíveis no R 
help(package = "datasets")

#Exemplo Dataset MTCARS
mtcars
?mtcars
View(mtcars)
edit(mtcars)

mtcars %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

#DICA ADICIONAL
#No caso dos dados de mtcars temos, por exemplo, milhas ao invés de kilometros. 
# E se quisessemos converter para kilometros?
milhas <- 50 #km que deseja converter
quilometros <- milhas * 1.609344
print(quilometros)

#remover 
rm(milhas,quilometros)


#INICIANDO DATA WRANGLING DATASET MTCARS
#Criando objeto
mtcars <- mtcars

#Estatística Descritiva
summary(mtcars)

#Média
mean(mtcars$mpg)
mean(mtcars$cyl)
mean(mtcars$hp)

#Mediana
median(mtcars$cyl)
median(mtcars$hp)

#Moda - Definir a função "Mode()" para moda
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(mtcars$mpg)
Mode(mtcars$cyl)
Mode(mtcars$hp) 


# Exibir a moda da variável "mpg"
cat("A moda da variável xxx é: ????")

rm(Mode)

#Amplitude
max(mtcars$mpg) - min(mtcars$mpg)
max(mtcars$cyl) - min(mtcars$cyl)
max(mtcars$hp) - min(mtcars$hp)

#Variancia
var(mtcars$mpg)
var(mtcars$cyl)
var(mtcars$hp)

#Desvio Padrao
sd(mtcars$mpg)
sd(mtcars$cyl)
sd(mtcars$hp)

#BOXPLOT
# Criar os boxplots variáveis MPG, CYL e HP
boxplot(mtcars$mpg, main="Boxplot da variável mpg", ylab="mpg")
boxplot(mtcars$cyl, main="Boxplot da variável cyl", ylab="cyl")
boxplot(mtcars$hp, main="Boxplot da variável hp", ylab="hp")

# Configurar o layout do gráfico
par(mfrow=c(1,3))

# Criar os boxplots
boxplot(mtcars$mpg, main="Boxplot das variáveis mpg, cyl e hp do dataset MTCARS", ylab="mpg")
boxplot(mtcars$cyl, main="", ylab="cyl")
boxplot(mtcars$hp, main="", ylab="hp")

#Boxplot interativo ggplot2????

#HISTOGRAMA
# Criar o histograma com a função ggplot() da variável MPG
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 3, color = "black", fill = "white") +
  xlab("Miles per gallon (mpg)") +
  ylab("Frequency") +
  ggtitle("Histogram of mpg in mtcars")


# Criar o histograma com a função ggplot() da variável CYL
ggplot(mtcars, aes(x = cyl)) +
  geom_histogram(binwidth = 3, color = "black", fill = "white") +
  xlab("cilindros_por_carro (CYL)") +
  ylab("Frequency") +
  ggtitle("Histogram of cyl in mtcars")



#GRÁFICO
# Criar o gráfico de dispersão com a função ggplot()
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point() +
  xlab("Milhas por galão (mpg)") +
  ylab("Cavalos de Potência (hp)") +
  ggtitle("Gráfico de Dispersão de Milhas por Galão vs. Cavalos de Potência - MTCARS")


#Erro padrão
sd(mtcars$mpg) / sqrt(length(mtcars$mpg))

# Coeficiente de variação
sd(mtcars$mpg) / mean(mtcars$mpg) * 100


#RENOMEANDO VARIAVEIS DO DATASET MTCARS
mtcars2 <- mtcars %>% rename (Milhas_galão = 1,
                              Cilindros = 2,
                              Deslocamento = 3,
                              Cavalos_Potência = 4,
                              Eixo_traseiro = 5,
                              Peso = 6,
                              Motor = 8,
                              Transmissão_auto_manual = 9,
                              Número_de_engrenagens = 10,
                              Número_de_carburadores = 11)


#EXCLUINDO VARIÁVEIS DO DATASET MTCARS
mtcars3 <- mtcars2 %>% select(-qsec, -Motor)

mtcars3 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 15)

#REALOCANDO VARIÁVEIS DO DATASET MTCARS
mtcars3 <- mtcars3 %>% select(Milhas_galão, Cilindros,Cavalos_Potência, Peso, everything()) 


#RENOMEANDO OBSERVAÇÕES DO DATASET MTCARS
#Alterar o valor 1 de Trasmissão auto ou manual para 11111. Vamos ver o que ocorre quando fazemos isso com um valor numérico
mtcars3 <- mutate(mtcars3, 
                  Transmissão_auto_ou_manual = recode(Transmissão_auto_ou_manual,
                                                      "1" = "1111111"))


#RENOMEANDO NOME DAS VARIAVÉIS?
mtcars$car_name <- gsub("Mazda RX4", "Mazda RX4 BR", mtcars$)
head(mtcars$car_name)


#Visualizando 
mtcars[1, ] 
mtcars[, 1]

#criando objeto
mtcarsx <- mtcars[1:3, 1:3]


#FILTRANDO INFORMAÇÕES
mtcars[mtcars$mpg >= 20, ] #todas irão aparecer, agora é possivel filtrar os dados e criar um novo objeto.

mtcars[mtcars$mpg >= 0 & mtcars$hp >= 0, c("mpg", "hp")]
mtcars4 <- mtcars[mtcars$mpg >= 0 & mtcars$hp >= 0, c("mpg", "hp")]

mtcars[mtcars$mpg >= 0 & mtcars$cyl >= 0 & mtcars$hp >= 0, c("mpg", "cyl", "hp")]
mtcars5 <- mtcars[mtcars$mpg >= 0 & mtcars$cyl >= 0 & mtcars$hp >= 0, c("mpg", "cyl", "hp")]


#OUTRAS FUNÇÕES IMPORTANTES
#group by
mtcars %>% 
  group_by(cyl) %>% 
  summarize(mean_mpg = mean(mpg))

#join
#?????



#VERIFICANDO E TRANSFORMANDO CLASSE DO OBJETO
class(mtcars$mpg)

# Converter a coluna mpg para numérica
mtcars$mpg <- as.numeric(mtcars$mpg)

# Agrupar o dataset mtcars pelo número de cilindros e calcular a média de mpg para cada grupo
mtcars %>% 
  group_by(cyl) %>% 
  summarize(mean_mpg = mean(mpg))

class(mpg)
class(mpg) == "numeric"
class(mpg) == "character"
class(mpg) == "tbl_df"
class(mpg) == "tbl"
class(mpg) == "data.frame"

mtcars$mpg <- as.numeric(as.character(mtcars$mpg))
mtcars$mpg <- as.numeric(mtcars$mpg)
class(mpg) #??????
class(mpg) #??????


#JOGO VERDADEIRO OU FALSO

pergunta <- function(pergunta, resposta_correta) {
  resposta <- readline(prompt = paste(pergunta, " (S/N): "))
  resposta <- toupper(resposta) # converter para letras maiúsculas
  if (resposta == "S") {
    resultado <- TRUE
  } else if (resposta == "N") {
    resultado <- FALSE
  } else {
    cat("Resposta inválida! Tente novamente.\n")
    resultado <- pergunta(pergunta, resposta_correta)
  }
  
  if (resultado == resposta_correta) {
    cat("Parabéns!!! Você acertou!\n")
  } else {
    cat("Você precisa melhorar!!!\n")
  }
  
  return(resultado)
}


pergunta("A terra é redonda?", TRUE)
N



#SALVANDO ARQUIVO
# Modificar o dataset mtcars
mtcars_salvo <- mtcars

# Salvar o dataset modificado em um arquivo csv
write.csv(mtcars_salvo, "C:/Users/ffcgp/OneDrive/Área de Trabalho/Projetos DSA/1. Data Wrangling/Data Wrangling R/mtcars_salvo.csv", row.names = FALSE)


#Salvar arquivo XLSX
install.packages("openxlsx")
library(openxlsx)

mtcars_salvo2 <-mtcars
write.xlsx(mtcars_salvo2, "C:/Users/ffcgp/OneDrive/Área de Trabalho/Projetos DSA/1. Data Wrangling/Data Wrangling R/mtcars_salvo2.xlsx")

