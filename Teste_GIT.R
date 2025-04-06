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
getSymbols("SPY", from = "2025-01-01", to = Sys.Date())
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