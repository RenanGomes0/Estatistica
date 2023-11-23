verificarRacaCorAcidentesTrabalho <- function(data) {
  # Verificar se as colunas necessárias estão presentes nos dados
  if ("ACIDTRAB" %in% colnames(data) & "RACACOR" %in% colnames(data)) {
    # Filtrar casos de acidentes de trabalho (ACIDTRAB == 1)
    acidentes_trabalho <- data[data$ACIDTRAB == 1, ]
    
    # Contar casos por raça/cor
    total_Branco <- sum(acidentes_trabalho$RACACOR == 1, na.rm = TRUE)
    total_Preta <- sum(acidentes_trabalho$RACACOR == 2, na.rm = TRUE)
    total_Amarela <- sum(acidentes_trabalho$RACACOR == 3, na.rm = TRUE)
    total_Pardo <- sum(acidentes_trabalho$RACACOR == 4, na.rm = TRUE)
    total_Indigena <- sum(acidentes_trabalho$RACACOR == 5, na.rm = TRUE)
    
    # Armazenar os totais em um vetor ou lista
    totais <- c(Branco = total_Branco, Preta = total_Preta, Amarela = total_Amarela, Pardo = total_Pardo, Indigena = total_Indigena)
    
    # Retornar um vetor com os totais
    return(totais)
  } else {
    stop("As colunas ACIDTRAB e/ou RACACOR não estão presentes nos dados.")
  }
}

# Carregar os dados de 2021
dados_mortalidade_2021 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2021.csv", sep = ";")

# Uso da nova função para acidentes de trabalho
totais_acidentes_trabalho_por_racacor <- verificarRacaCorAcidentesTrabalho(dados_mortalidade_2021)

# Imprimir resultados
print("Totais de casos de acidentes de trabalho por raça/cor em 2021:")
print(totais_acidentes_trabalho_por_racacor)
