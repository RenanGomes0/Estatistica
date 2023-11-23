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

# Criar uma tabela de contingência para o teste qui-quadrado
tabela_contingencia <- table(c("1996", "2021"), c("Suicídio", "Mortes Violentas"))
tabela_contingencia[1, 1] <- total_suicidios_1996
tabela_contingencia[2, 1] <- total_suicidios_2021
tabela_contingencia[1, 2] <- nrow(mortes_violentas_1996)
tabela_contingencia[2, 2] <- nrow(mortes_violentas_2021)

# Realizar o teste qui-quadrado
resultado_teste <- chisq.test(tabela_contingencia)

# Imprimir os resultados
print(resultado_teste)
