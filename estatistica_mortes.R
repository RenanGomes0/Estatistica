# Função para calcular o total de suicídios em um data frame
calcularTotalSuicidios <- function(data) {
  if ("CIRCOBITO" %in% colnames(data)) {
    total_suicidios <- sum(data$CIRCOBITO == 2, na.rm = TRUE)
    return(total_suicidios)
  } else {
    return(NA)  # Retorna NA se a coluna CIRCOBITO não estiver presente
  }
}

# Carregar os dados de 1996
dados_mortalidade_1996 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_1996.csv", sep = ";")

# Carregar os dados de 2021
dados_mortalidade_2021 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2021.csv", sep = ";")

# Calcular o total de suicídios em cada tabela
total_suicidios_1996 <- calcularTotalSuicidios(dados_mortalidade_1996)
total_suicidios_2021 <- calcularTotalSuicidios(dados_mortalidade_2021)

# Imprimir os resultados
cat("Total de suicídios em 1996:", total_suicidios_1996, "\n")
cat("Total de suicídios em 2021:", total_suicidios_2021, "\n")

############################################################################################

#Instale o pacote ggplot2 se ainda não estiver instalado
 install.packages("ggplot2")

# Carregar a biblioteca ggplot2
library(ggplot2)

# Dados
anos <- c("1996", "2021")
total_suicidios <- c(total_suicidios_1996, total_suicidios_2021)

# Criar um data frame com os dados
dados_comparativos <- data.frame(Ano = anos, TotalSuicidios = total_suicidios)

# Criar o gráfico de barras comparativo
ggplot(dados_comparativos, aes(x = Ano, y = TotalSuicidios, fill = Ano)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparação de Suicídios em 1996 e 2021",
       x = "Ano",
       y = "Total de Suicídios") +
  
  
  theme_minimal()




#############################################################################################
# Carregar a biblioteca ggplot2
library(ggplot2)

# População em milhões
populacao_1996 <- 166000000
populacao_2021 <- 214300000

# Criar um data frame com os dados de população
dados_populacao <- data.frame(
  Ano = c("1996", "2021"),
  Populacao = c(populacao_1996, populacao_2021)
)

# Gráfico de linhas para população
populacao_plot <- ggplot(dados_populacao, aes(x = Ano, y = Populacao, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolução da População (1996-2021)",
    x = "Ano",
    y = "População (milhões)"
  ) +
  theme_minimal()

# Dados de suicídios
dados_suicidios <- data.frame(
  Ano = c("1996", "2021"),
  Suicidios = c(total_suicidios_1996, total_suicidios_2021)
)

# Gráfico de linhas para suicídios
suicidios_plot <- ggplot(dados_suicidios, aes(x = Ano, y = Suicidios, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolução do Número Total de Suicídios (1996-2021)",
    x = "Ano",
    y = "Total de Suicídios"
  ) +
  theme_minimal()

# Imprimir os gráficos
print(populacao_plot)
print(suicidios_plot)


###########################################################

  