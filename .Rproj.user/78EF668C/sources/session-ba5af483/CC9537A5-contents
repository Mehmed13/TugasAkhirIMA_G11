library(quantmod)
library(PerformanceAnalytics)
library(quantmod)
library(zoo)



# 1. SMCI
SMCI <- read.csv("Downloads/SMCI.csv")

# Pemisahan Data Train dan Data Testing
SMCI_Close<- SMCI$Close
SMCI_Close_Train <- SMCI_Close[1:2516]
SMCI_Close_Test <- SMCI_Close[2517:2767]
summary(SMCI_Close_Test)

stock_symbol <- "SMCI"
start_date <- "2019-01-01"
end_date <- "2019-12-31"

# Get historical stock prices
getSymbols(stock_symbol, from = start_date, to = end_date)

stock_prices <- Cl(get(stock_symbol))

# Calculate daily returns
returns <- dailyReturn(stock_prices)

# Calculate summary statistics
summary_stats <- summary(returns)

# Print the summary statistics
print(summary_stats)

# Daily Return
return_SMCI <- dailyReturn(Cl(get(stock_symbol)))
print(return_SMCI)

SMCI_Test <- Cl(SMCI)

# Download risk-free rate data (e.g., 10-year US Treasury Yield)
getSymbols("GS10", src = "FRED", from = start_date, to = end_date)
risk_free_rate <- GS10 / 100 / 252  # Convert annual yield to daily rate
print(risk_free_rate)

# Calculate Sharpe Ratio
sharpe_ratio <- SharpeRatio(returns, Rf = risk_free_rate, FUN = "StdDev")

# Download market returns data
getSymbols("^RUT", from = start_date, to = end_date)
market_returns <- dailyReturn(Cl(RUT))
russell_2000 <- Cl(RUT)

# Calculate Treynor Ratio
treynor_ratio <- TreynorRatio(returns, Rf = risk_free_rate, Rb = market_returns)

# Download benchmark returns data
getSymbols("IWM", from = start_date, to = end_date)
benchmark_returns <- dailyReturn(Cl(IWM))

# Calculate Information Ratio
information_ratio <- InformationRatio(returns, benchmark_returns)

# Print the results
cat("Sharpe Ratio:", sharpe_ratio, "\n")
cat("Treynor Ratio:", treynor_ratio, "\n")
cat("Information Ratio:", information_ratio, "\n")



# 2. SAIA
SAIA <- read.csv("Downloads/SAIA.csv")
# Pemisahan Data Train dan Data Testing
SAIA_Close<- SAIA$Close
SAIA_Close_Train <- SAIA_Close[1:2516]
SAIA_Close_Test <- SAIA_Close[2517:2767]
summary(SAIA_Close_Test)

stock_symbol <- "SAIA"
start_date <- "2019-01-01"
end_date <- "2019-12-31"

# Get historical stock prices
getSymbols(stock_symbol, from = start_date, to = end_date)

stock_prices <- Cl(get(stock_symbol))

# Calculate daily returns
returns <- dailyReturn(stock_prices)

# Calculate summary statistics
summary_stats <- summary(returns)

# Print the summary statistics
print(summary_stats)
# Daily Return
return_SAIA <- dailyReturn(Cl(get(stock_symbol)))
print(return_SAIA)

SAIA_Test <- Cl(SAIA)
print(SAIA_Test)

getSymbols("GS10", src = "FRED", from = start_date, to = end_date)
risk_free_rate <- GS10 / 100 / 252  # Convert annual yield to daily rate
print(risk_free_rate)

# Calculate Sharpe Ratio
sharpe_ratio <- SharpeRatio(returns, Rf = risk_free_rate, FUN = "StdDev")

# Download market returns data (e.g., S&P 500)
getSymbols("^RUT", from = start_date, to = end_date)
market_returns <- dailyReturn(Cl(RUT))

# Calculate Treynor Ratio
treynor_ratio <- TreynorRatio(returns, Rf = risk_free_rate, Rb = market_returns)

# Download benchmark returns data
getSymbols("IWM", from = start_date, to = end_date)
benchmark_returns <- dailyReturn(Cl(IWM))

# Calculate Information Ratio
information_ratio <- InformationRatio(returns, benchmark_returns)

# Print the results
cat("Sharpe Ratio:", sharpe_ratio, "\n")
cat("Treynor Ratio:", treynor_ratio, "\n")
cat("Information Ratio:", information_ratio, "\n")


# 3. IRDM
IRDM <- read.csv("Downloads/IRDM.csv")
# Pemisahan Data Train dan Data Testing
IRDM_Close<- IRDM$Close
IRDM_Close_Train <- IRDM_Close[1:2516]
IRDM_Close_Test <- IRDM_Close[2517:2767]
summary(IRDM_Close_Test)

stock_symbol <- "IRDM"
start_date <- "2019-01-01"
end_date <- "2019-12-31"

# Get historical stock prices
getSymbols(stock_symbol, from = start_date, to = end_date)

stock_prices <- Cl(get(stock_symbol))

# Calculate daily returns
returns <- dailyReturn(stock_prices)

# Calculate summary statistics
summary_stats <- summary(returns)

# Print the summary statistics
print(summary_stats)

# Daily Return
return_IRDM <- dailyReturn(Cl(get(stock_symbol)))
print(return_IRDM)

IRDM_Test <- Cl(IRDM)
print(IRDM_Test)

# Download market returns data (e.g., S&P 500)
getSymbols("^RUT", from = start_date, to = end_date)
market_returns <- dailyReturn(Cl(RUT))

# Calculate Treynor Ratio
treynor_ratio <- TreynorRatio(returns, Rf = risk_free_rate, Rb = market_returns)

# Download benchmark returns data
getSymbols("IWM", from = start_date, to = end_date)
benchmark_returns <- dailyReturn(Cl(IWM))

# Calculate Information Ratio
information_ratio <- InformationRatio(returns, benchmark_returns)

# Print the results
cat("Sharpe Ratio:", sharpe_ratio, "\n")
cat("Treynor Ratio:", treynor_ratio, "\n")
cat("Information Ratio:", information_ratio, "\n")

# 4. EME
EME <- read.csv("Downloads/EME.csv")
# Pemisahan Data Train dan Data Testing
EME_Close<- EME$Close
EME_Close_Train <- EME_Close[1:2516]
EME_Close_Test <- EME_Close[2517:2767]
summary(EME_Close_Test)

stock_symbol <- "EME"
start_date <- "2019-01-01"
end_date <- "2019-12-31"

# Get historical stock prices
getSymbols(stock_symbol, from = start_date, to = end_date)

stock_prices <- Cl(get(stock_symbol))

# Calculate daily returns
returns <- dailyReturn(stock_prices)

# Calculate summary statistics
summary_stats <- summary(returns)

# Print the summary statistics
print(summary_stats)

# Daily Return
return_EME <- dailyReturn(Cl(get(stock_symbol)))
print(return_EME)

EME_Test <- Cl(EME)
print(EME_Test)
# Download market returns data (e.g., S&P 500)
getSymbols("^RUT", from = start_date, to = end_date)
market_returns <- dailyReturn(Cl(RUT))

# Calculate Treynor Ratio
treynor_ratio <- TreynorRatio(returns, Rf = risk_free_rate, Rb = market_returns)

# Download benchmark returns data
getSymbols("IWM", from = start_date, to = end_date)
benchmark_returns <- dailyReturn(Cl(IWM))

# Calculate Information Ratio
information_ratio <- InformationRatio(returns, benchmark_returns)

# Print the results
cat("Sharpe Ratio:", sharpe_ratio, "\n")
cat("Treynor Ratio:", treynor_ratio, "\n")
cat("Information Ratio:", information_ratio, "\n")

# 5. TXRH
TXRH <- read.csv("Downloads/TXRH.csv")

# Pemisahan Data Train dan Data Testing
TXRH_Close<- TXRH$Close
TXRH_Close_Train <- TXRH_Close[1:2516]
TXRH_Close_Test <- TXRH_Close[2517:2767]
summary(TXRH_Close_Test)

stock_symbol <- "TXRH"
start_date <- "2019-01-01"
end_date <- "2019-12-31"

# Get historical stock prices
getSymbols(stock_symbol, from = start_date, to = end_date)

stock_prices <- Cl(get(stock_symbol))

# Calculate daily returns
returns <- dailyReturn(stock_prices)

# Calculate summary statistics
summary_stats <- summary(returns)

# Print the summary statistics
print(summary_stats)

# Daily Return
return_TXRH <- dailyReturn(Cl(get(stock_symbol)))
print(return_TXRH)

TXRH_Test <- Cl(TXRH)
print(TXRH_Test)

# Download risk-free rate data (e.g., 10-year US Treasury Yield)
getSymbols("GS10", src = "FRED", from = start_date, to = end_date)
risk_free_rate <- GS10 / 100 / 252  # Convert annual yield to daily rate
print(risk_free_rate)

# Calculate Sharpe Ratio
sharpe_ratio <- SharpeRatio(returns, Rf = risk_free_rate, FUN = "StdDev")
print(sharpe_ratio)

# Download market returns data (e.g., S&P 500)
getSymbols("^RUT", from = start_date, to = end_date)
market_returns <- dailyReturn(Cl(RUT))

# Calculate Treynor Ratio
treynor_ratio <- TreynorRatio(returns, Rf = risk_free_rate, Rb = market_returns)

# Download benchmark returns data
getSymbols("IWM", from = start_date, to = end_date)
benchmark_returns <- dailyReturn(Cl(IWM))

# Calculate Information Ratio
information_ratio <- InformationRatio(returns, benchmark_returns)

# Print the results
cat("Sharpe Ratio:", sharpe_ratio, "\n")
cat("Treynor Ratio:", treynor_ratio, "\n")
cat("Information Ratio:", information_ratio, "\n")


# Merge the data frames based on the 'Date' column
data_combined <- merge(SMCI_Test, SAIA_Test,IRDM_Test, EME_Test, TXRH_Test  , by = "Date", all = TRUE)
# Model CAPM
lm_model <- lm(return_SMCI ~ market_returns, data = data_combined)
summary(lm_model)
lm_model <- lm(return_SAIA ~ market_returns, data = data_combined)
summary(lm_model)
lm_model <- lm(return_IRDM ~ market_returns, data = data_combined)
summary(lm_model)
lm_model <- lm(return_EME ~ market_returns, data = data_combined)
summary(lm_model)
lm_model <- lm(return_TXRH ~ market_returns, data = data_combined)
summary(lm_model)