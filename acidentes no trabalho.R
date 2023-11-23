verificarAciden <- function(data) {
  # Verificar se a coluna necessária está presente nos dados
  if ("ACIDTRAB" %in% colnames(data)) {
    # Filtrar acidentes
    Ignorado <- sum(data$ACIDTRAB == 9, na.rm = TRUE)
    Sim <- sum(data$ACIDTRAB == 1, na.rm = TRUE)
    Nao <- sum(data$ACIDTRAB == 2, na.rm = TRUE)
    
    # Armazenar os totais em um vetor, incluindo Ignorado e Não
    totais <- c(Sim = Sim, Ignorado_Nao = Ignorado + Nao)
    
    # Retornar um vetor com os totais
    return(totais)
  } else {
    stop("A coluna ACIDTRAB não está presente nos dados.")
  }
}


# Carregar os dados de 2020
dados_mortalidade_2020 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2020.csv", sep = ";")

# Carregar os dados de 2021
dados_mortalidade_2021 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2021.csv", sep = ";")

# Uso da função para 2020
totais_acidentes_2020 <- verificarAciden(dados_mortalidade_2020)

# Uso da função para 2021
totais_acidentes_2021 <- verificarAciden(dados_mortalidade_2021)

# Imprimir resultados para 2020
print("Totais de acidentes por relação com o trabalho em 2020:")
print(totais_acidentes_2020)

# Imprimir resultados para 2021
print("Totais de acidentes por relação com o trabalho em 2021:")
print(totais_acidentes_2021)



# Criar uma tabela de contingência apenas com os casos conhecidos
tabela_contingencia <- matrix(c(totais_acidentes_2020["Sim"],
                                totais_acidentes_2021["Sim"]),
                              nrow = 2, byrow = TRUE)



# Criar uma tabela de contingência apenas com os casos "Sim"
tabela_contingencia <- matrix(c(totais_acidentes_2020["Sim"], totais_acidentes_2021["Sim"]),
                              nrow = 1, byrow = TRUE)

# Verificar se há valores NA na matriz
if (any(is.na(tabela_contingencia))) {
  stop("A matriz contém valores ausentes (NA). Corrija antes de continuar.")
}

# Verificar se todas as entradas são numéricas
if (!all(is.numeric(tabela_contingencia))) {
  stop("A matriz contém valores não numéricos. Corrija antes de continuar.")
}

# Verificar se todas as entradas são não negativas
if (!all(tabela_contingencia >= 0)) {
  stop("A matriz contém valores negativos. Corrija antes de continuar.")
}

# Adicionar rótulos às linhas e colunas
rownames(tabela_contingencia) <- c("Sim")
colnames(tabela_contingencia) <- c("2020", "2021")

# Realizar o teste qui-quadrado de independência
teste_qui_quadrado <- chisq.test(tabela_contingencia)

# Imprimir os resultados do teste
print("Resultados do teste qui-quadrado (apenas casos 'Sim'):")
print(teste_qui_quadrado)



# Calcular proporção de casos "Sim" para cada ano
prop_2020 <- totais_acidentes_2020["Sim"] / sum(totais_acidentes_2020)
prop_2021 <- totais_acidentes_2021["Sim"] / sum(totais_acidentes_2021)

# Comparar as proporções
aumento_proporcao <- prop_2021 - prop_2020

# Imprimir a diferença nas proporções
print("Diferença nas proporções entre 2021 e 2020:")
print(aumento_proporcao)
