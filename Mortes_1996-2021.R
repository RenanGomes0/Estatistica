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

# Filtrar apenas mortes violentas (CIRCOBITO diferente de 2 e não vazias)
mortes_violentas_1996 <- dados_mortalidade_1996[!is.na(dados_mortalidade_1996$CIRCOBITO) & dados_mortalidade_1996$CIRCOBITO != 2, ]
mortes_violentas_2021 <- dados_mortalidade_2021[!is.na(dados_mortalidade_2021$CIRCOBITO) & dados_mortalidade_2021$CIRCOBITO != 2, ]

# Criar um data frame com os dados
dados_grafico <- data.frame(
  Ano = rep(c("1996", "2021"), each = 2),
  Categoria = rep(c("Mortes não naturais", "Suicídios"), times = 2),
  Total = c(
    nrow(mortes_violentas_1996), total_suicidios_1996,
    nrow(mortes_violentas_2021), total_suicidios_2021
  )
)

# Criar o gráfico de colunas sobrepostas
library(ggplot2)
grafico <- ggplot(dados_grafico, aes(x = Ano, y = Total, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparação entre Mortes não naturais e Suicídios (1996 vs. 2021)",
       x = "Ano",
       y = "Total") +
  scale_fill_manual(values = c("Mortes não naturais" = "blue", "Suicídios" = "red"))

# Exibir o gráfico
print(grafico)
