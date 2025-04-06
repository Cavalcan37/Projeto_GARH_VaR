# BITCOIN: DIAGNÓSTICO DE VIABILIDADE

# Passo 1: Instalação e carregamento dos pacotes 
install.packages("quantmod")
install.packages("rugarch")
install.packages("FinTS")
library(quantmod) # Funções 'getSymbols' e 'ROC'
library(rugarch) # Funções 'ugarchspec', 'ugarchfit' e 'sigma'
library(FinTS) # Função 'ArchTest'

# Ensaio Prévio - BTC 2025
getSymbols("BTC-USD", from = "2025-02-01", to = Sys.Date())
BTCUSD <- `BTC-USD`
BTCUSD # Série Temporal (xts/zoo)
chartSeries(BTCUSD, theme = "white", TA = "addVo(); addBBands()")

# Passo 2: Obtenção dos dados do Bitcoin (BTC-USD) 
getSymbols("BTC-USD", src = "yahoo", from = "2014-09-17", to = Sys.Date())

# Renomeia o objeto para um nome válido no R
BTCUSD <- `BTC-USD`
BTCUSD

# Passo 3: Calcule os retornos do Bitcoin
retornos_btc <- na.omit(ROC(Ad(BTCUSD), type = "discrete"))
retornos_btc

# Renomeia a coluna
colnames(retornos_btc) <- "Retornos_BTC"
retornos_btc

################################################################################
# Passo 4: Testar Estacionariedade
# Carregar pacotes necessários
install.packages("tseries")
library(tseries)

# Teste ADF
adf_test <- adf.test(retornos_btc) # H0: Série Não Estacionária
print(adf_test)                    # H1: Série Estacionária

# Teste KPSS
kpss_test <- kpss.test(retornos_btc) # H0: Série Estacionária
print(kpss_test)                     # H0: Série Não Estacionária
################################################################################

# Passo 5: Análise exploratória
plot(retornos_btc, main = "Retornos Diários do Bitcoin", col = "blue4", lwd = 1)
hist(retornos_btc, breaks = 25, main = "Distribuição dos Retornos do Bitcoin", 
     xlab = "Retornos", col = "lightcoral",probability = TRUE)

densidade <- density(retornos_btc)  # Calcula a densidade kernel
lines(densidade, col = "blue", lwd = 2)  # Adiciona a curva de densidade ao gráfico

ArchTest(retornos_btc, lags = 12)

# Passo 6: Especificação do modelo GARCH(1,1) para Bitcoin
especificacao_btc <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"  # Distribuição t de Student
)

# Passo 7: Estimação do modelo GARCH(1,1) para Bitcoin
modelo_garch_btc <- ugarchfit(spec = especificacao_btc, data = retornos_btc)
print(modelo_garch_btc)

# Passo 8: Obtenha a volatilidade estimada para Bitcoin
volatilidade_estimada_btc <- sigma(modelo_garch_btc)
volatilidade_estimada_btc

# Passo 9: Plotagem dos retornos e volatilidades estimadas

# Bloco do Bitcoin
par(mfrow = c(2, 1))  # Divide a área de plotagem em 2 linhas e 1 coluna

# Gráfico de cima: Retornos do Bitcoin
plot(retornos_btc, main = "Retornos Diários do Bitcoin", col = "blue4", lwd = 1)
legend("topright", 
       legend = c("Retornos Bitcoin"), 
       col = c("blue4"), 
       lwd = 1, 
       cex = 0.8,  # Tamanho da legenda
       bty = "n")  # Remove a caixa ao redor da legenda

# Gráfico de baixo: Volatilidade estimada do Bitcoin
plot(volatilidade_estimada_btc, main = "Volatilidade Estimada do Bitcoin (GARCH(1,1))", col = "red3", lwd = 2)
legend("topright", 
       legend = c("Volatilidade Bitcoin"), 
       col = c("red3"), 
       lwd = 2, 
       cex = 0.8,  # Tamanho da legenda
       bty = "n")  # Remove a caixa ao redor da legenda

par(mfrow = c(1, 1))

# Passo 10: Previsão de volatilidade para os próximos 30* dias
previsao_volatilidade <- ugarchforecast(modelo_garch_btc, n.ahead = 30)
print(previsao_volatilidade)

# Passo 11: Plotagem da previsão de volatilidade
plot(previsao_volatilidade, which = 3, main = "Previsão de Volatilidade para os Próximos 30 Dias")

# VaR
# Passo 12: Calcular o Value-at-Risk (VaR) para 1 dia

# Definir o nível de confiança (95% é comum)
alpha <- 0.05

# Calcular o VaR para os retornos históricos
# Usando a volatilidade estimada pelo GARCH(1,1) e a distribuição t de Student
# O quantil da distribuição t de Student é obtido com qt()
df <- modelo_garch_btc@fit$coef["shape"]  # Graus de liberdade da distribuição t
VaR_historico <- volatilidade_estimada_btc * qt(alpha, df)

# Plotar o VaR histórico sobre os retornos
plot(retornos_btc, main = "Retornos do Bitcoin com VaR (95%)", col = "blue4", lwd = 1)
lines(VaR_historico, col = "red", lwd = 2)
legend("topright", 
       legend = c("Retornos Bitcoin", "VaR 95%"), 
       col = c("blue4", "red"), 
       lwd = c(1, 2), 
       cex = 0.8, 
       bty = "n")

# Passo 13: Calcular o Value-at-Risk (VaR) para 10 dias

# Definir o nível de confiança (95% é comum)
alpha <- 0.05

# Calcular o VaR para 10 dias
# A volatilidade para 10 dias é a volatilidade diária multiplicada pela raiz quadrada de 10
volatilidade_10_dias <- volatilidade_estimada_btc * sqrt(10)

# Calcular o VaR para 10 dias usando a distribuição t de Student
df <- modelo_garch_btc@fit$coef["shape"]  # Graus de liberdade da distribuição t
VaR_10_dias <- volatilidade_10_dias * qt(alpha, df)

# Plotar o VaR para 10 dias sobre os retornos
plot(retornos_btc, main = "Retornos do Bitcoin com VaR (95%) para 10 Dias", col = "blue4", lwd = 1)
lines(VaR_10_dias, col = "red", lwd = 2)
legend("topright", 
       legend = c("Retornos Bitcoin", "VaR 95% (10 Dias)"), 
       col = c("blue4", "red"), 
       lwd = c(1, 2), 
       cex = 0.8, 
       bty = "n")

# Value-at-Risk (VaR)
# Passo 1: Extrair a volatilidade estimada pelo modelo GARCH(1,1)
volatilidade_diaria <- as.numeric(tail(volatilidade_estimada_btc, 1))  # Último valor da volatilidade diária

# Passo 2: Definir o nível de confiança (95% é comum)
alpha <- 0.05

# Passo 3: Calcular o quantil da distribuição t de Student
df <- modelo_garch_btc@fit$coef["shape"]  # Graus de liberdade da distribuição t
quantil <- qt(alpha, df)  # Quantil para o nível de confiança alpha

# Passo 4: Calcular o VaR para 1 dia
VaR_1_dia <- volatilidade_diaria * quantil

# Passo 5: Calcular o VaR para 10 dias
volatilidade_10_dias <- volatilidade_diaria * sqrt(10)
VaR_10_dias <- volatilidade_10_dias * quantil

# Passo 6: Exibir os resultados
cat("VaR para 1 dia (95% de confiança):", VaR_1_dia, "\n")
cat("VaR para 10 dias (95% de confiança):", VaR_10_dias, "\n")

# Interpretação:
# Para 1 dia: VaR = -0.08. Significa que há 95% de confiança de que a perda
# máxima para um 1 não exceda 8% (ou seja, temos um risco de 5% da perda 
# máxima em 1 dia ultrapassar os 8%).

# Para 10 dia: VaR = -0.25. Significa que há 95% de confiança de que a perda
# máxima para um 1 não exceda 25% (ou seja, temos um risco de 5% da perda 
# máxima em 10 dia ultrapassar os 25%).

################################################################################
# Plotando Retornos e VaR + GARCH(1,1) abaixo (p/Linkedin)
# Ensaio Prévio - BTC 2025
getSymbols("BTC-USD", from = "2025-02-01", to = Sys.Date())
BTCUSD <- `BTC-USD`
BTCUSD # Série Temporal (xts/zoo)
chartSeries(BTCUSD, theme = "white", TA = "addVo(); addBBands()")

par(mfrow = c(1, 1))
plot(retornos_btc, main = "Retornos do Bitcoin com VaR (95%)", col = "blue4", lwd = 1)
graf <- lines(VaR_historico, col = "red", lwd = 2)

# Gráfico Retornos + VaR 
par(mfrow = c(2, 1))
graf

# Gráfico GARCH(1,1)
plot(volatilidade_estimada_btc, main = "Volatilidade do Bitcoin (GARCH(1,1))", col = "red3", lwd = 2)

par(mfrow = c(1, 1)) 
################################################################################