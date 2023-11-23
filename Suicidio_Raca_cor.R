# Função para verificar a raça/cor dos casos de suicídio
verificarRacaCorSuicidio <- function(data) {
  # Verificar se as colunas necessárias estão presentes nos dados
  if ("CIRCOBITO" %in% colnames(data) & "RACACOR" %in% colnames(data)) {
    # Filtrar casos de suicídio (CIRCOBITO == 2)
    suicidios <- data[data$CIRCOBITO == 2, ]
    
    # Contar casos por raça/cor
    total_Branco <- sum(suicidios$RACACOR == 1, na.rm = TRUE)
    total_Preta <- sum(suicidios$RACACOR == 2, na.rm = TRUE)
    total_Amarela <- sum(suicidios$RACACOR == 3, na.rm = TRUE)
    total_Pardo <- sum(suicidios$RACACOR == 4, na.rm = TRUE)
    total_Indigena <- sum(suicidios$RACACOR == 5, na.rm = TRUE)
    
    # Armazenar os totais em um vetor ou lista
    totais <- c(Branco = total_Branco, Preta = total_Preta, Amarela = total_Amarela, Pardo = total_Pardo, Indigena = total_Indigena)
    
    # Retornar um vetor com os totais
    return(totais)
  } else {
    stop("As colunas CIRCOBITO e/ou RACACOR não estão presentes nos dados.")
  }
}

# Uso da função
totais_casos_suicidio_por_racacor <- verificarRacaCorSuicidio(dados_mortalidade_2021)

# Imprimir resultados
print("Totais de casos de suicídios por raça/cor em 2021:")
print(totais_casos_suicidio_por_racacor)
####################

# Carregar a biblioteca ggplot2
library(ggplot2)

# Criar um data frame com os dados
df_racacor <- data.frame(
  RacaCor = c("Branco", "Preta", "Amarela", "Pardo", "Indigena"),
  Total = totais_casos_suicidio_por_racacor
)

# Criar o gráfico de colunas
grafico_colunas_racacor <- ggplot(df_racacor, aes(x = RacaCor, y = Total, fill = RacaCor)) +
  geom_col() +
  labs(title = "Distribuição de Casos de Suicídio por Raça/Cor",
       x = "Raça/Cor",
       y = "Total") +
  theme_minimal()

# Exibir o gráfico
print(grafico_colunas_racacor)
