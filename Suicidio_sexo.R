# Função para calcular o total de suicídios em um data frame
calcularTotalSuicidios <- function(data) {
  if ("CIRCOBITO" %in% colnames(data)) {
    total_suicidios <- sum(data$CIRCOBITO == 2, na.rm = TRUE)
    return(total_suicidios)
  } else {
    return(NA)  # Retorna NA se a coluna CIRCOBITO não estiver presente
  }
}

# Função para verificar o sexo dos casos de suicídio
verificarSexoSuicidio <- function(data) {
  if ("CIRCOBITO" %in% colnames(data) & "SEXO" %in% colnames(data)) {
    # Filtrar casos de suicídio (CIRCOBITO == 2)
    suicidios <- data[data$CIRCOBITO == 2, ]
    
    # Inicializar vetores para contar os casos por sexo
    total_M <- sum(suicidios$SEXO == 1, na.rm = TRUE)
    total_F <- sum(suicidios$SEXO == 2, na.rm = TRUE)
    total_I <- sum(suicidios$SEXO == 3, na.rm = TRUE)
    # Armazenar os totais em um vetor ou lista
    totais <- c(M = total_M, F = total_F, I = total_I)
    
    # Retornar uma lista com os totais
    return(totais)
  } else {
    stop("As colunas CIRCOBITO e/ou SEXO não estão presentes nos dados.")
  }
}
# Função para verificar o sexo e raça/cor dos casos de suicídio
verificarSexoRacaCorSuicidio <- function(data) {
  # Verificar se as colunas necessárias estão presentes nos dados
  if ("CIRCOBITO" %in% colnames(data) & "SEXO" %in% colnames(data) & "RACACOR" %in% colnames(data)) {
    # Filtrar casos de suicídio (CIRCOBITO == 2)
    suicidios <- data[data$CIRCOBITO == 2, ]
    
    # Inicializar vetores para contar os casos por sexo e raça/cor
    total_M <- sum(suicidios$SEXO == 1, na.rm = TRUE)
    total_F <- sum(suicidios$SEXO == 2, na.rm = TRUE)
    total_I <- sum(suicidios$SEXO == 3, na.rm = TRUE)
    
    # Contar casos por raça/cor
    total_Branco <- sum(suicidios$RACACOR == 1, na.rm = TRUE)
    total_Preta <- sum(suicidios$RACACOR == 2, na.rm = TRUE)
    total_Amarela <- sum(suicidios$RACACOR == 3, na.rm = TRUE)
    total_Pardo <- sum(suicidios$RACACOR == 4, na.rm = TRUE)
    total_Indigena <- sum(suicidios$RACACOR == 5, na.rm = TRUE)
    
    # Armazenar os totais em um vetor ou lista
    totais <- list(
      Sexo = c(M = total_M, F = total_F, I = total_I),
      RacaCor = c(Branco = total_Branco, Preta = total_Preta, Amarela = total_Amarela, Pardo = total_Pardo, Indigena = total_Indigena)
    )
    
    # Retornar uma lista com os totais
    return(totais)
  } else {
    stop("As colunas CIRCOBITO, SEXO e/ou RACACOR não estão presentes nos dados.")
  }
}

# Carregar os dados de 2021
dados_mortalidade_2021 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2021.csv", sep = ";")

# Uso das funções
total_suicidios <- calcularTotalSuicidios(dados_mortalidade_2021)
totais_casos_suicidio_por_sexo <- verificarSexoSuicidio(dados_mortalidade_2021)

# Imprimir resultados
print("Total de casos de suicídios em 2021:")
print(total_suicidios)

print("Totais de casos de suicídios por sexo em 2021:")
print(totais_casos_suicidio_por_sexo)

######################
# Carregar a biblioteca ggplot2
library(ggplot2)

# Remover a categoria "Indefinido"
dados_suicidio_por_sexo <- dados_suicidio_por_sexo[-3]

# Criar um data frame com os dados
df <- data.frame(Sexo = c("Masculino", "Feminino"), Total = dados_suicidio_por_sexo)

# Criar o gráfico de colunas
grafico_colunas <- ggplot(df, aes(x = Sexo, y = Total, fill = Sexo)) +
  geom_col() +
  labs(title = "Distribuição de Casos de Suicídio por Sexo",
       x = "Sexo",
       y = "Total") +
  theme_minimal()

# Exibir o gráfico
print(grafico_colunas)
