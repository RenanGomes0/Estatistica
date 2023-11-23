calcularTotalSuicidios <- function(data) {
  tryCatch({
    if ("CIRCOBITO" %in% colnames(data)) {
      total_suicidios <- sum(data$CIRCOBITO == 2, na.rm = TRUE)
      return(total_suicidios)
    } else {
      return(NA)
    }
  }, error = function(e) {
    cat("Erro:", conditionMessage(e), "\n")
    return(NA)
  })
}

# Carregar os dados de 1996
dados_mortalidade_1996 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_1996.csv", sep = ";")

# Carregar os dados de 1998
dados_mortalidade_1998 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_1998.csv", sep = ";")

# Carregar os dados de 2000
dados_mortalidade_2000 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2000.csv", sep = ";")

# Carregar os dados de 2002
#dados_mortalidade_2002 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2002.csv", sep = ";")

# Carregar os dados de 2004
dados_mortalidade_2004 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2004.csv", sep = ";")

# Carregar os dados de 2006
dados_mortalidade_2006 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2006.csv", sep = ";")

# Carregar os dados de 2008
dados_mortalidade_2008 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2008.csv", sep = ";")

# Carregar os dados de 2010
dados_mortalidade_2010 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2010.csv", sep = ";")

# Carregar os dados de 2012
dados_mortalidade_2012 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2012.csv", sep = ";")

# Carregar os dados de 2014
dados_mortalidade_2014 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2014.csv", sep = ";")

# Carregar os dados de 2016
dados_mortalidade_2016 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2016.csv", sep = ";")

# Carregar os dados de 2018
dados_mortalidade_2018 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2018.csv", sep = ";")

# Carregar os dados de 2020
dados_mortalidade_2020 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2020.csv", sep = ";")

# Carregar os dados de 2021
dados_mortalidade_2021 <- read.csv2("C:\\Users\\renan\\OneDrive\\Documentos\\estatistica\\Mortalidade_Geral_2021.csv", sep = ";")

# Calcular o total de suicídios em cada tabela
total_suicidios_1996 <- calcularTotalSuicidios(dados_mortalidade_1996)
total_suicidios_1998 <- calcularTotalSuicidios(dados_mortalidade_1998)
total_suicidios_2000 <- calcularTotalSuicidios(dados_mortalidade_2000)
#total_suicidios_2002 <- calcularTotalSuicidios(dados_mortalidade_2002)
total_suicidios_2004 <- calcularTotalSuicidios(dados_mortalidade_2004)
total_suicidios_2006 <- calcularTotalSuicidios(dados_mortalidade_2006)
total_suicidios_2008 <- calcularTotalSuicidios(dados_mortalidade_2008)
total_suicidios_2010 <- calcularTotalSuicidios(dados_mortalidade_2010)
total_suicidios_2012 <- calcularTotalSuicidios(dados_mortalidade_2012)
total_suicidios_2014 <- calcularTotalSuicidios(dados_mortalidade_2014)
total_suicidios_2016 <- calcularTotalSuicidios(dados_mortalidade_2016)
total_suicidios_2018 <- calcularTotalSuicidios(dados_mortalidade_2018)
total_suicidios_2020 <- calcularTotalSuicidios(dados_mortalidade_2020)
total_suicidios_2021 <- calcularTotalSuicidios(dados_mortalidade_2021)

# Imprimir os resultados
cat("Total de suicídios em 1996:", total_suicidios_1996, "\n")
cat("Total de suicídios em 1998:", total_suicidios_1998, "\n")
cat("Total de suicídios em 2000:", total_suicidios_2000, "\n")
#cat("Total de suicídios em 2002:", total_suicidios_2002, "\n")
cat("Total de suicídios em 2004:", total_suicidios_2004, "\n")
cat("Total de suicídios em 2006:", total_suicidios_2006, "\n")
cat("Total de suicídios em 2008:", total_suicidios_2008, "\n")
cat("Total de suicídios em 2010:", total_suicidios_2010, "\n")
cat("Total de suicídios em 2012:", total_suicidios_2012, "\n")
cat("Total de suicídios em 2014:", total_suicidios_2014, "\n")
cat("Total de suicídios em 2016:", total_suicidios_2016, "\n")
cat("Total de suicídios em 2018:", total_suicidios_2018, "\n")
cat("Total de suicídios em 2020:", total_suicidios_2020, "\n")
cat("Total de suicídios em 2021:", total_suicidios_2021, "\n")
################################################################################

#fazer o grafico 
# Instalar e carregar a biblioteca ggplot2
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Criar um data frame com os anos e o total de suicídios
dados_grafico <- data.frame(
  Ano = c(1996, 1998, 2000, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2021),
  Total_Suicidios = c(
    total_suicidios_1996, total_suicidios_1998, total_suicidios_2000,
    total_suicidios_2004, total_suicidios_2006, total_suicidios_2008,
    total_suicidios_2010, total_suicidios_2012, total_suicidios_2014,
    total_suicidios_2016, total_suicidios_2018, total_suicidios_2020, total_suicidios_2021
  )
)

# Criar o gráfico de linhas
ggplot(dados_grafico, aes(x = Ano, y = Total_Suicidios)) +
  geom_line() +
  geom_point() +
  labs(title = "Total de Suicídios ao Longo dos Anos",
       x = "Ano",
       y = "Total de Suicídios") +
  theme_minimal()



##### 
#teste de hipotese 
# Definir os totais de suicídios reais para 2020 e 2021
total_suicidios_2020_real <- total_suicidios_2020  
total_suicidios_2021_real <- total_suicidios_2021

# Criar amostras com base nos totais reais
amostra_2020 <- rep(2020, total_suicidios_2020_real)
amostra_2021 <- rep(2021, total_suicidios_2021_real)

# Combinar as amostras em um único vetor
dados <- c(amostra_2020, amostra_2021)

# Criar um vetor indicando o ano correspondente para cada observação
anos <- rep(c("2020", "2021"), c(total_suicidios_2020_real, total_suicidios_2021_real))

# Criar um data frame
dados_teste <- data.frame(Ano = as.factor(anos), Suicidios = as.factor(dados))

# Realizar o teste exato de Fisher
resultado_teste <- fisher.test(table(dados_teste$Ano, dados_teste$Suicidios))

# Exibir os resultados do teste
cat("Resultado do Teste exato de Fisher:\n")
print(resultado_teste)


############################
#qui quadrado
# Definir os totais de suicídios reais para 2020 e 2021
total_suicidios_2020_real <- total_suicidios_2020  
total_suicidios_2021_real <- total_suicidios_2021

# Criar amostras com base nos totais reais
amostra_2020 <- rep(2020, total_suicidios_2020_real)
amostra_2021 <- rep(2021, total_suicidios_2021_real)

# Combinar as amostras em um único vetor
dados <- c(amostra_2020, amostra_2021)

# Criar um vetor indicando o ano correspondente para cada observação
anos <- rep(c("2020", "2021"), c(total_suicidios_2020_real, total_suicidios_2021_real))

# Criar um data frame
dados_teste <- data.frame(Ano = as.factor(anos), Suicidios = as.factor(dados))

# Criar uma tabela de contingência
tabela_contingencia <- table(dados_teste$Ano, dados_teste$Suicidios)

# Realizar o teste qui-quadrado
resultado_teste_qui_quadrado <- chisq.test(tabela_contingencia)

# Exibir os resultados do teste qui-quadrado
cat("Resultado do Teste Qui-Quadrado:\n")
print(resultado_teste_qui_quadrado)


#################################
# Carregar a biblioteca ggplot2 se ainda não estiver carregada
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Definir os totais de suicídios reais para 2020 e 2021
total_suicidios_2020_real <- total_suicidios_2020
total_suicidios_2021_real <- total_suicidios_2021

# Criar amostras com base nos totais reais
amostra_2020 <- rep(2020, total_suicidios_2020_real)
amostra_2021 <- rep(2021, total_suicidios_2021_real)

# Combinar as amostras em um único vetor
dados <- c(amostra_2020, amostra_2021)

# Criar um vetor indicando o ano correspondente para cada observação
anos <- rep(c("2020", "2021"), c(total_suicidios_2020_real, total_suicidios_2021_real))

# Criar um data frame
dados_teste <- data.frame(Ano = as.factor(anos), Suicidios = as.factor(dados))

# Criar uma tabela de contingência
tabela_contingencia <- table(dados_teste$Ano, dados_teste$Suicidios)

# Realizar o teste qui-quadrado
resultado_teste_qui_quadrado <- chisq.test(tabela_contingencia)

# Exibir os resultados do teste qui-quadrado
cat("Resultado do Teste Qui-Quadrado:\n")
print(resultado_teste_qui_quadrado)

# Criar um gráfico de barras empilhadas invertido
grafico <- ggplot(dados_teste, aes(x = Suicidios, fill = Ano)) +
  geom_bar(stat = "count", position = "stack") +
  labs(title = "Distribuição de Suicídios em 2020 e 2021",
       x = "Suicídios",
       y = "Contagem",
       fill = "Ano") +
  theme_minimal() +
  coord_flip()

# Exibir o gráfico
print(grafico)
