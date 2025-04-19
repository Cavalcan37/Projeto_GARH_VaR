# COMPARANDO O ETF GLOBAL SPY COM O ÍDICE VIX
#DIAGNÓSTICO DE VOLATILIDADE

# Passo 1: Instalação e carregamento dos pacotes 
library(quantmod) # Funções 'getSymbols' e 'ROC'
library(rugarch) # Funções 'ugarchspec', 'ugarchfit' e 'sigma'
library(FinTS) # Função 'ArchTest'

# Lista de ativos
ativos <- c("SPY", "EEM", "QQQ", "FXI", "SMH", "^VIX", "^BVSP")

# Baixar dados (ajuste as datas conforme necessário)
dados <- lapply(ativos, function(x) {
  getSymbols(x, src = "yahoo", from = "2024-01-01", to = Sys.Date(), auto.assign = FALSE)
})
dados

# Nomear os elementos da lista com os tickers
names(dados) <- ativos

# Calcular retornos (discretos) e remover NAs
retornos <- lapply(dados, function(x) {
  na.omit(ROC(Cl(x), type = "discrete"))
})

# Renomear colunas para clarity
for (i in seq_along(retornos)) {
  colnames(retornos[[i]]) <- paste0("Retornos_", names(retornos)[i])
}

library(xts)
library(ggplot2)
library(reshape2)

# 1. Normalizar preços para base 100
precos_normalizados <- lapply(dados, function(x) {
  (Cl(x) / as.numeric(Cl(x)[1])) * 100
})

# Temas
library(ggthemes)

# 2. Combinar em um único xts e converter para dataframe
precos_combinados <- do.call(merge, precos_normalizados)
colnames(precos_combinados) <- ativos
df <- fortify(precos_combinados, melt = TRUE)  # Converte xts para dataframe "long format"

# 3. Plotar com ggplot2 CORRETAMENTE
ggplot(df, aes(x = Index, y = Value, color = Series)) +
  geom_line() +
  labs(title = "Comparação de ETFs/Índices (Base 100)", 
       x = "Data", 
       y = "Preço Normalizado") +
  theme_bw()

library(quantmod)
library(PerformanceAnalytics)

# 1. Baixar os dados (ajuste as datas se necessário)
ativos <- c("SPY", "EEM", "QQQ", "FXI", "SMH", "^VIX", "^BVSP")
dados <- lapply(ativos, function(x) {
  getSymbols(x, src = "yahoo", from = "2024-01-01", to = Sys.Date(), auto.assign = FALSE)
})
names(dados) <- ativos

# 2. Extrair retornos diários e combinar em um dataframe
retornos <- lapply(dados, function(x) {
  na.omit(ROC(Cl(x), type = "discrete"))  # Retornos discretos e remove NAs
})

# Combinar todos os retornos em uma única matriz
retornos_combinados <- do.call(merge, retornos)
colnames(retornos_combinados) <- ativos

# Remover linhas com NAs residuais (se houver)
retornos_combinados <- na.omit(retornos_combinados)

# 3. Calcular e plotar a matriz de correlação
chart.Correlation(retornos_combinados, 
                  histogram = TRUE,   # Mostra histogramas na diagonal
                  method = "pearson", # Método de correlação
                  pch = 19)          # Símbolo dos pontos
