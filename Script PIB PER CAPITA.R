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


#Visualizando base de dados
PIB_Per_Capita_Países %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 13)

#Estatística descritiva
summary(PIB_Per_Capita_Países)
glimpse(PIB_Per_Capita_Países)

freq_PIB <- table(PIB_Per_Capita_Países$`Income group`)
freq_PIB

79+27+55+55
is.na(PIB_Per_Capita_Países$`Income group`)
freq_NA <- table(is.na(PIB_Per_Capita_Países$`Income group`))
freq_NA
sum(is.na(PIB_Per_Capita_Países$`Income group`))
216+49

barplot(freq_PIB, main = "Frequência dos países por nível de renda", xlab = "Nível de renda", ylab = "Frequência")
barplot(freq_PIB, main = "Frequência dos países por nível de renda", xlab = "Nível de renda", ylab = "Frequência", col = "lightpink")


library(ggplot2)

# criando a tabela de frequência
freq <- table(PIB_Per_Capita_Países$`Income group`, useNA = "ifany")

# definindo as cores para cada categoria
cores <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "lightpink")

# criando o gráfico de barras com as cores e rótulos
barplot(freq, col = cores, ylim = c(0, 80), 
        main = "Distribuição de Income Group", 
        xlab = "Income Group", ylab = "Frequência")
text(1:5, freq + 5, labels = freq, col = "black")
text(6, 50, paste("NA:", sum(is.na(PIB_Per_Capita_Países$`Income group`))))

# Criar a tabela de frequência
freq <- data.frame(category = c("High income", "Low income", "Lower middle income", "Upper middle income"),
                   freq = c(79, 27, 55, 55))

# Criar o gráfico de barras com cores diferentes para cada categoria
ggplot(freq, aes(x = category, y = freq, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73", "#F0E442")) +
  # Adicionar os valores da frequência no gráfico
  geom_text(aes(label = freq), vjust = -0.5)


library(ggplot2)

# Dados de frequência
freq <- c(79, 27, 55, 55, 49)

# Nomes das categorias
categorias <- c("High Income", "Low Income", "Lower Middle Income", "Upper Middle Income", "NA")

# Vetor de cores
cores <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "red")

# Data frame com dados de frequência e categorias
df <- data.frame(categorias, freq)

# Gráfico de barras com cores diferentes para cada categoria
ggplot(df, aes(x = categorias, y = freq, fill = categorias)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores) +
  labs(title = "Frequência de categorias de renda",
       x = "Categoria de renda",
       y = "Frequência")


# criar tabela de frequência
freq_table <- table(PIB_Per_Capita_Países$`Income group`, useNA = "ifany")

# ordenar tabela por ordem crescente
freq_table <- freq_table[order(freq_table)]

# criar gráfico de barras ordenado
barplot(freq_table, col = rainbow(length(freq_table)))



library(dplyr)
library(ggplot2)

df_freq <- data.frame(grupo = c("High income", "Low income", "Lower middle income", "Upper middle income", "NA"), 
                      frequencia = c(79, 27, 55, 55, 49))

df_freq_ord <- df_freq %>%
  arrange(desc(frequencia))

ggplot(df_freq_ord, aes(x=grupo, y=frequencia, fill=grupo)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=frequencia), vjust=1.5, color="white", size=3.5) +
  theme_minimal() +
  xlab("Income group") +
  ylab("Frequency") +
  ggtitle("Frequência dos países por Grupo de PIB") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(df_freq, aes(x=reorder(grupo, -frequencia, FUN = function(x) x), y=frequencia, fill=grupo)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=frequencia), vjust=1.5, color="white", size=3.5) +
  scale_fill_manual(values=c("#0072B2", "#E69F00", "#009E73", "#FF8C00", "#F0E442", "gray")) +
  theme_minimal() +
  xlab("Income group") +
  ylab("Frequency") +
  ggtitle("Frequência dos países por Grupo de PIB") +
  theme(plot.title = element_text(hjust = 0.5)) 
