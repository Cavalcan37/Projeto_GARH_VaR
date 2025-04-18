# ETF GLOBAL ÍNDICE DE AÇÕES DO S&p 500 (SPY)
#DIAGNÓSTICO DE VOLATILIDADE

# Passo 1: Instalação e carregamento dos pacotes 
install.packages("quantmod")
install.packages("rugarch")
install.packages("FinTS")
library(quantmod) # Funções 'getSymbols' e 'ROC'
library(rugarch) # Funções 'ugarchspec', 'ugarchfit' e 'sigma'
library(FinTS) # Função 'ArchTest'

# Ensaio Prévio - SPY
getSymbols("SPY", from = "2025-03-01", to = Sys.Date())
chartSeries(SPY, theme = "white", TA = "addVo(); addBBands()")

# Passo 2: Obtenção dos dados do ETF Índice de Ações S&P 500 (SPY) 
spy <- getSymbols("SPY", src = "yahoo", from = "2024-01-01", to = Sys.Date(), auto.assign = FALSE)
spy <- na.locf(spy)  # Valores faltantes preenchidos com o último valor válido
# Muinto comum em finanças                           
spy

# Passo 3: Calcule os retornos do SPY
retornos_spy <- na.omit(ROC(Cl(spy), type = "discrete"))
retornos_spy

# Renomeia a coluna
colnames(retornos_spy) <- "Retornos_SPY"
retornos_spy

################################################################################
# Passo 4: Testar Estacionariedade
# Carregar pacotes necessários
install.packages("tseries")
library(tseries)

# Teste ADF
adf_test <- adf.test(retornos_spy)       # H0: Série Não Estacionária
print(adf_test)                          # H1: Série Estacionária

# Teste KPSS
kpss_test <- kpss.test(retornos_spy)     # H0: Série Estacionária
print(kpss_test)                         # H0: Série Não Estacionária
################################################################################

# Passo 5: Análise exploratória
par(mfrow = c(2, 1))
plot(retornos_spy, main = "Retornos Diários SPY", col = "blue4", lwd = 1)
hist(retornos_spy, breaks = 25, main = "Distribuição dos Retornos SPY", 
     xlab = "Retornos", col = "lightgreen",probability = TRUE)

densidade <- density(retornos_spy)  # Calcula a densidade kernel
lines(densidade, col = "red", lwd = 2)  # Adiciona a curva de densidade ao gráfico
par(mfrow = c(1, 1))

ArchTest(retornos_spy, lags = 12)

# Passo 6: Especificação do modelo GARCH(1,1) para SPY
especificacao_spy <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"  # Distribuição t de Student
)

# Passo 7: Estimação do modelo GARCH(1,1) para SPY
modelo_garch_spy <- ugarchfit(spec = especificacao_spy, data = retornos_spy)
print(modelo_garch_spy)

# Passo 8: Obtenha a volatilidade estimada para SPY
volatilidade_estimada_spy <- sigma(modelo_garch_spy)
volatilidade_estimada_spy

# Passo 9: Plotagem dos retornos e volatilidades estimadas

# Bloco do SPY
par(mfrow = c(2, 1))  # Divide a área de plotagem em 2 linhas e 1 coluna

# Gráfico de cima: Retornos do Bitcoin
plot(retornos_spy, main = "Retornos Diários SPY", col = "blue4", lwd = 1)
legend("topright", 
       legend = c("Retornos SPY"), 
       col = c("blue4"), 
       lwd = 1, 
       cex = 0.8,  # Tamanho da legenda
       bty = "n")  # Remove a caixa ao redor da legenda

# Gráfico de baixo: Volatilidade estimada do SPY
plot(volatilidade_estimada_spy, main = "Volatilidade Estimada SPY (GARCH(1,1))", col = "red3", lwd = 2)
legend("topright", 
       legend = c("Volatilidade SPY"), 
       col = c("red3"), 
       lwd = 2, 
       cex = 0.8,  # Tamanho da legenda
       bty = "n")  # Remove a caixa ao redor da legenda

par(mfrow = c(1, 1))

# Passo 10: Previsão de volatilidade para os próximos 30* dias
previsao_volatilidade <- ugarchforecast(modelo_garch_spy, n.ahead = 30)
print(previsao_volatilidade)

# Passo 11: Plotagem da previsão de volatilidade
plot(previsao_volatilidade, which = 3, main = "Previsão de Volatilidade para os Próximos 30 Dias")

# VALUE-AT-RISK:
# Passo 12: Calcular o Value-at-Risk (VaR) para 1 dia

# Definir o nível de confiança (95% é comum)
alpha <- 0.05

# Calcular o VaR para os retornos históricos
# Usando a volatilidade estimada pelo GARCH(1,1) e a distribuição t de Student
# O quantil da distribuição t de Student é obtido com qt()
df <- modelo_garch_spy@fit$coef["shape"]  # Graus de liberdade da distribuição t
VaR_historico <- volatilidade_estimada_spy * qt(alpha, df)

# Plotar o VaR histórico sobre os retornos
plot(retornos_spy, main = "Retornos SPY com VaR (95%)", col = "blue4", lwd = 1)
lines(VaR_historico, col = "red", lwd = 2)
legend("topright", 
       legend = c("Retornos SPY", "VaR 95%"), 
       col = c("blue4", "red"), 
       lwd = c(1, 2), 
       cex = 0.8, 
       bty = "n")

# Passo 13: Calcular o Value-at-Risk (VaR) para 10 dias

# Definir o nível de confiança (95% é comum)
alpha <- 0.05

# Calcular o VaR para 10 dias
# A volatilidade para 10 dias é a volatilidade diária multiplicada pela raiz quadrada de 10
volatilidade_10_dias <- volatilidade_estimada_spy * sqrt(10)

# Calcular o VaR para 10 dias usando a distribuição t de Student
df <- modelo_garch_spy@fit$coef["shape"]  # Graus de liberdade da distribuição t
VaR_10_dias <- volatilidade_10_dias * qt(alpha, df)

# Plotar o VaR para 10 dias sobre os retornos
plot(retornos_spy, main = "Retornos do SPY com VaR (95%) para 10 Dias", col = "blue4", lwd = 1)
lines(VaR_10_dias, col = "red", lwd = 2)
legend("topright", 
       legend = c("Retornos SPY", "VaR 95% (10 Dias)"), 
       col = c("blue4", "red"), 
       lwd = c(1, 2), 
       cex = 0.8, 
       bty = "n")

# Value-at-Risk (VaR):
# Passo 14: Extrair a volatilidade estimada pelo modelo GARCH(1,1)
volatilidade_diaria <- as.numeric(tail(volatilidade_estimada_spy, 1))  # Último valor da volatilidade diária

# Passo 15: Definir o nível de confiança (95% é comum)
alpha <- 0.05

# Passo 16: Calcular o quantil da distribuição t de Student
df <- modelo_garch_spy@fit$coef["shape"]  # Graus de liberdade da distribuição t
quantil <- qt(alpha, df)  # Quantil para o nível de confiança alpha

# Passo 17: Calcular o VaR para 1 dia
VaR_1_dia <- volatilidade_diaria * quantil

# Passo 18: Calcular o VaR para 10 dias
volatilidade_10_dias <- volatilidade_diaria * sqrt(10)
VaR_10_dias <- volatilidade_10_dias * quantil

# Passo 19: Exibir os resultados
cat("VaR para 1 dia (95% de confiança):", VaR_1_dia, "\n")
cat("VaR para 10 dias (95% de confiança):", VaR_10_dias, "\n")

# Interpretação:
# Para 1 dia: VaR = -0.08. Significa que há 95% de confiança de que a perda
# máxima para um 1 não exceda 8% (ou seja, temos um risco de 5% da perda 
# máxima em 1 dia ultrapassar os 8%).

# Para 10 dia: VaR = -0.25. Significa que há 95% de confiança de que a perda
# máxima para um 1 não exceda 25% (ou seja, temos um risco de 5% da perda 
# máxima em 10 dia ultrapassar os 25%).

# LINKEDIN######################################################################
# Plotando Retornos e VaR + GARCH(1,1) abaixo (p/Linkedin)
# Ensaio Prévio - SMAL11.SA
getSymbols("SPY", from = "2025-03-01", to = Sys.Date())
chartSeries(SPY, theme = "white", TA = "addVo(); addBBands()")

par(mfrow = c(1, 1))
plot(retornos_spy, main = "Retornos SPY com VaR (95%)", col = "blue4", lwd = 1)
graf <- lines(VaR_historico, col = "red", lwd = 2)

# Gráfico Retornos + VaR 
par(mfrow = c(2, 1))
graf

# Gráfico GARCH(1,1)
plot(volatilidade_estimada_spy, main = "Volatilidade SPY (GARCH(1,1))", col = "red3", lwd = 2)

par(mfrow = c(1, 1)) 

####### Cálculo do Retorno Médio Geométrico #######
#Carregar pacotes necessários
library(quantmod)
library(PerformanceAnalytics)  # Para funções financeiras avançadas

# Baixar dados do SMAL11.SA
getSymbols("SPY", src = "yahoo", from = "2020-01-01", to = Sys.Date())

# Calcular retornos diários (usando preços ajustados)
retornos_diarios <- dailyReturn(Ad(SPY), type = "log")  # Retornos logarítmicos

# Remover valores NA (se houver)
retornos_diarios <- na.omit(retornos_diarios)

# Calcular retorno geométrico anualizado
retorno_geometrico_anual <- Return.annualized(retornos_diarios, geometric = TRUE)
retorno_geometrico_anual
################################################################################













