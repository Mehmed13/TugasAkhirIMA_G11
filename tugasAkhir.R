library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(ggfortify)
library(ggplot2)
library(quadprog)

# Constants
## Date
test_start_date <- "2019-01-01"
test_end_date <- "2019-12-31"

train_start_date <- "2009-01-01"
train_end_date <- "2018-12-31"

## Risk Free Rate (10 years US Treasury Yield)
getSymbols("GS10", src = "FRED", from = train_start_date, to = test_end_date)
yearly_risk_free_rate <- (period.apply(GS10/100, endpoints(GS10/100, on = "years"), mean))
daily_risk_free_rate <- GS10 / 100 / 252  # Convert annual yield to daily rate
print(yearly_risk_free_rate)
print(daily_risk_free_rate)

# Benchmark
getSymbols("SPY", from = train_start_date, to = train_end_date)
daily_benchmark_returns <- dailyReturn(Cl(SPY))
yearly_benchmark_returns <- (period.apply(SPY, endpoints(SPY, on="years"), mean))


# Index Wilshire 5000 (Market)
## train
### Load Data
getSymbols("^W5000", from = train_start_date, to = train_end_date)
train_market_prices <- na.fill(Cl(get("W5000")), fill = colMeans(Cl(get("W5000")), na.rm = TRUE))

train_market_daily_returns <- dailyReturn(train_market_prices)
colnames(train_market_daily_returns) <- "return"
train_market_yearly_returns <- period.apply(train_market_daily_returns, endpoints(train_market_daily_returns, on = "years"), 
                                      function(x) prod(1 + x) - 1)
### Print Statistic Data
print(summary(train_market_daily_returns))
print(train_market_yearly_returns)
print(summary(train_market_yearly_returns))

### Plot Data
autoplot(train_market_prices, colour="blue") + 
  labs(title = "Wilshire 5000 Index Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_market_daily_returns, colour="blue") + 
  labs(title = "Wilshire 5000 Index Daily Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_market_yearly_returns, colour="blue") + 
  labs(title = "Wilshire 5000 Index Yearly Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

## test
### Load Data
getSymbols("^W5000", from = test_start_date, to = test_end_date)
test_market_prices <- na.fill(Cl(get("W5000")), fill = colMeans(Cl(get("W5000")), na.rm = TRUE))
test_market_daily_returns <- dailyReturn(test_market_prices)
colnames(test_market_daily_returns) <- "return"
test_market_yearly_returns <- period.apply(test_market_daily_returns, endpoints(test_market_daily_returns, on = "years"), 
                               function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(test_market_daily_returns))
print(test_market_yearly_returns)

### Plot Data
autoplot(test_market_prices, colour="blue") + 
  labs(title = "Wilshire 5000 Index Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(test_market_daily_returns, colour="blue") + 
  labs(title = "Wilshire 5000 Index Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))



# 1. T
## train
### Load Data
getSymbols("T", from = train_start_date, to = train_end_date)
train_t_prices <- Cl(get("T"))
train_t_daily_returns <- dailyReturn(train_t_prices)
colnames(train_t_daily_returns) <- "return"
train_t_yearly_returns <- period.apply(train_t_daily_returns, endpoints(train_t_daily_returns, on = "years"), 
                                          function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(train_t_daily_returns))
print(train_t_yearly_returns)
print(summary(train_t_yearly_returns))

### Plot Data
autoplot(train_t_prices, colour="blue") + 
  labs(title = "T Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_t_daily_returns, colour="blue") + 
  labs(title = "T Daily Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_t_yearly_returns, colour="blue") + 
  labs(title = "T Yearly Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

### Additional Analysis
#### Sharpe Ratio
train_t_daily_sharpe_ratio <- SharpeRatio(train_t_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
train_t_yearly_sharpe_ratio <- SharpeRatio(train_t_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
train_t_daily_treynor_ratio <- TreynorRatio(train_t_daily_returns, Rf = daily_risk_free_rate, Rb = train_market_daily_returns)
train_t_yearly_treynor_ratio <- TreynorRatio(train_t_yearly_returns, Rf = yearly_risk_free_rate, Rb = train_market_yearly_returns)

#### Information Ratio
train_t_daily_information_ratio <- InformationRatio(train_t_daily_returns, benchmark_returns)
train_t_yearly_information_ratio <- InformationRatio(train_t_yearly_returns, train_market_yearly_returns)

### Print the Additional Analysis results
print("T Daily Data 2009 - 2018")
cat("Sharpe Ratio:", train_t_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_t_daily_treynor_ratio, "\n")
cat("Information Ratio:", train_t_daily_information_ratio, "\n")

print("T Yearly Data 2009 - 2018")
cat("Sharpe Ratio:", train_t_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_t_yearly_treynor_ratio, "\n")
cat("Information Ratio:", train_t_yearly_information_ratio, "\n")


## test
### Load Data
getSymbols("T", from = test_start_date, to = test_end_date)
test_t_prices <- Cl(get("T"))
test_t_daily_returns <- dailyReturn(test_t_prices)
colnames(test_t_daily_returns) <- "return"
test_t_yearly_returns <- period.apply(test_t_daily_returns, endpoints(test_t_daily_returns, on = "years"), 
                                         function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(test_t_daily_returns))
print(test_t_yearly_returns)

### Plot Data
autoplot(test_t_prices, colour="blue") + 
  labs(title = "T Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(test_t_daily_returns, colour="blue") + 
  labs(title = "T Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


### Additional Analysis
#### Sharpe Ratio
test_t_daily_sharpe_ratio <- SharpeRatio(test_t_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
test_t_yearly_sharpe_ratio <- SharpeRatio(test_t_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
test_t_daily_treynor_ratio <- TreynorRatio(test_t_daily_returns, Rf = daily_risk_free_rate, Rb = test_market_daily_returns)
test_t_yearly_treynor_ratio <- TreynorRatio(test_t_yearly_returns, Rf = yearly_risk_free_rate, Rb = test_market_yearly_returns)

#### Information Ratio
test_t_daily_information_ratio <- InformationRatio(test_t_daily_returns, test_market_daily_returns)
test_t_yearly_information_ratio <- InformationRatio(test_t_yearly_returns, test_market_yearly_returns)

### Print the Additional Analysis results
print("T Daily Data 2019")
cat("Sharpe Ratio:", test_t_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_t_daily_treynor_ratio, "\n")
cat("Information Ratio:", test_t_daily_information_ratio, "\n")

print("T Yearly Data 2019")
cat("Sharpe Ratio:", test_t_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_t_yearly_treynor_ratio, "\n")
cat("Information Ratio:", test_t_yearly_information_ratio, "\n")

# 2. KEY
## train
### Load Data
getSymbols("KEY", from = train_start_date, to = train_end_date)
train_key_prices <- Cl(get("KEY"))
train_key_daily_returns <- dailyReturn(train_key_prices)
colnames(train_key_daily_returns) <- "return"
train_key_yearly_returns <- period.apply(train_key_daily_returns, endpoints(train_key_daily_returns, on = "years"), 
                                        function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(train_key_daily_returns))
print(train_key_yearly_returns)
print(summary(train_key_yearly_returns))

### Plot Data
autoplot(train_key_prices, colour="blue") + 
  labs(title = "KEY Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_key_daily_returns, colour="blue") + 
  labs(title = "KEY Daily Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_key_yearly_returns, colour="blue") + 
  labs(title = "KEY Yearly Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

### Additional Analysis
#### Sharpe Ratio
train_key_daily_sharpe_ratio <- SharpeRatio(train_key_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
train_key_yearly_sharpe_ratio <- SharpeRatio(train_key_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
train_key_daily_treynor_ratio <- TreynorRatio(train_key_daily_returns, Rf = daily_risk_free_rate, Rb = train_market_daily_returns)
train_key_yearly_treynor_ratio <- TreynorRatio(train_key_yearly_returns, Rf = yearly_risk_free_rate, Rb = train_market_yearly_returns)

#### Information Ratio
train_key_daily_information_ratio <- InformationRatio(train_key_daily_returns, train_market_daily_returns)
train_key_yearly_information_ratio <- InformationRatio(train_key_yearly_returns, train_market_yearly_returns)

### Print the Additional Analysis results
print("KEY Daily Data 2009 - 2018")
cat("Sharpe Ratio:", train_key_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_key_daily_treynor_ratio, "\n")
cat("Information Ratio:", train_key_daily_information_ratio, "\n")

print("KEY Yearly Data 2009 - 2018")
cat("Sharpe Ratio:", train_key_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_key_yearly_treynor_ratio, "\n")
cat("Information Ratio:", train_key_yearly_information_ratio, "\n")


## test
### Load Data
getSymbols("KEY", from = test_start_date, to = test_end_date)
test_key_prices <- Cl(get("KEY"))
test_key_daily_returns <- dailyReturn(test_key_prices)
colnames(test_key_daily_returns) <- "return"
test_key_yearly_returns <- period.apply(test_key_daily_returns, endpoints(test_key_daily_returns, on = "years"), 
                                       function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(test_key_daily_returns))
print(test_key_yearly_returns)

### Plot Data
autoplot(test_key_prices, colour="blue") + 
  labs(title = "KEY Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(test_key_daily_returns, colour="blue") + 
  labs(title = "KEY Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


### Additional Analysis
#### Sharpe Ratio
test_key_daily_sharpe_ratio <- SharpeRatio(test_key_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
test_key_yearly_sharpe_ratio <- SharpeRatio(test_key_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
test_key_daily_treynor_ratio <- TreynorRatio(test_key_daily_returns, Rf = daily_risk_free_rate, Rb = test_market_daily_returns)
test_key_yearly_treynor_ratio <- TreynorRatio(test_key_yearly_returns, Rf = yearly_risk_free_rate, Rb = test_market_yearly_returns)

#### Information Ratio
test_key_daily_information_ratio <- InformationRatio(test_key_daily_returns, test_market_daily_returns)
test_key_yearly_information_ratio <- InformationRatio(test_key_yearly_returns, test_market_yearly_returns)

### Print the Additional Analysis results
print("KEY Daily Data 2019")
cat("Sharpe Ratio:", test_key_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_key_daily_treynor_ratio, "\n")
cat("Information Ratio:", test_key_daily_information_ratio, "\n")

print("KEY Yearly Data 2019")
cat("Sharpe Ratio:", test_key_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_key_yearly_treynor_ratio, "\n")
cat("Information Ratio:", test_key_yearly_information_ratio, "\n")

# 3. CVX
## train
### Load Data
getSymbols("CVX", from = train_start_date, to = train_end_date)
train_cvx_prices <- Cl(get("CVX"))
train_cvx_daily_returns <- dailyReturn(train_cvx_prices)
colnames(train_cvx_daily_returns) <- "return"
train_cvx_yearly_returns <- period.apply(train_cvx_daily_returns, endpoints(train_cvx_daily_returns, on = "years"), 
                                         function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(train_cvx_daily_returns))
print(train_cvx_yearly_returns)
print(summary(train_cvx_yearly_returns))

### Plot Data
autoplot(train_cvx_prices, colour="blue") + 
  labs(title = "CVX Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_cvx_daily_returns, colour="blue") + 
  labs(title = "CVX Daily Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_cvx_yearly_returns, colour="blue") + 
  labs(title = "CVX Yearly Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

### Additional Analysis
#### Sharpe Ratio
train_cvx_daily_sharpe_ratio <- SharpeRatio(train_cvx_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
train_cvx_yearly_sharpe_ratio <- SharpeRatio(train_cvx_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
train_cvx_daily_treynor_ratio <- TreynorRatio(train_cvx_daily_returns, Rf = daily_risk_free_rate, Rb = train_market_daily_returns)
train_cvx_yearly_treynor_ratio <- TreynorRatio(train_cvx_yearly_returns, Rf = yearly_risk_free_rate, Rb = train_market_yearly_returns)

#### Information Ratio
train_cvx_daily_information_ratio <- InformationRatio(train_cvx_daily_returns, train_market_daily_returns)
train_cvx_yearly_information_ratio <- InformationRatio(train_cvx_yearly_returns, train_market_yearly_returns)

### Print the Additional Analysis results
print("CVX Daily Data 2009 - 2018")
cat("Sharpe Ratio:", train_cvx_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_cvx_daily_treynor_ratio, "\n")
cat("Information Ratio:", train_cvx_daily_information_ratio, "\n")

print("CVX Yearly Data 2009 - 2018")
cat("Sharpe Ratio:", train_cvx_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_cvx_yearly_treynor_ratio, "\n")
cat("Information Ratio:", train_cvx_yearly_information_ratio, "\n")


## test
### Load Data
getSymbols("CVX", from = test_start_date, to = test_end_date)
test_cvx_prices <- Cl(get("CVX"))
test_cvx_daily_returns <- dailyReturn(test_cvx_prices)
colnames(test_cvx_daily_returns) <- "return"
test_cvx_yearly_returns <- period.apply(test_cvx_daily_returns, endpoints(test_cvx_daily_returns, on = "years"), 
                                        function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(test_cvx_daily_returns))
print(test_cvx_yearly_returns)

### Plot Data
autoplot(test_cvx_prices, colour="blue") + 
  labs(title = "CVX Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(test_cvx_daily_returns, colour="blue") + 
  labs(title = "CVX Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


### Additional Analysis
#### Sharpe Ratio
test_cvx_daily_sharpe_ratio <- SharpeRatio(test_cvx_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
test_cvx_yearly_sharpe_ratio <- SharpeRatio(test_cvx_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
test_cvx_daily_treynor_ratio <- TreynorRatio(test_cvx_daily_returns, Rf = daily_risk_free_rate, Rb = test_market_daily_returns)
test_cvx_yearly_treynor_ratio <- TreynorRatio(test_cvx_yearly_returns, Rf = yearly_risk_free_rate, Rb = test_market_yearly_returns)

#### Information Ratio
test_cvx_daily_information_ratio <- InformationRatio(test_cvx_daily_returns, test_market_daily_returns)
test_cvx_yearly_information_ratio <- InformationRatio(test_cvx_yearly_returns, test_market_yearly_returns)

### Print the Additional Analysis results
print("CVX Daily Data 2019")
cat("Sharpe Ratio:", test_cvx_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_cvx_daily_treynor_ratio, "\n")
cat("Information Ratio:", test_cvx_daily_information_ratio, "\n")

print("CVX Yearly Data 2019")
cat("Sharpe Ratio:", test_cvx_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_cvx_yearly_treynor_ratio, "\n")
cat("Information Ratio:", test_cvx_yearly_information_ratio, "\n")

# 4. TBBK
## train
### Load Data
getSymbols("TBBK", from = train_start_date, to = train_end_date)
train_tbbk_prices <- Cl(get("TBBK"))
train_tbbk_daily_returns <- dailyReturn(train_tbbk_prices)
colnames(train_tbbk_daily_returns) <- "return"
train_tbbk_yearly_returns <- period.apply(train_tbbk_daily_returns, endpoints(train_tbbk_daily_returns, on = "years"), 
                                          function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(train_tbbk_daily_returns))
print(train_tbbk_yearly_returns)
print(summary(train_tbbk_yearly_returns))

### Plot Data
autoplot(train_tbbk_prices, colour="blue") + 
  labs(title = "TBBK Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_tbbk_daily_returns, colour="blue") + 
  labs(title = "TBBK Daily Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_tbbk_yearly_returns, colour="blue") + 
  labs(title = "TBBK Yearly Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

### Additional Analysis
#### Sharpe Ratio
train_tbbk_daily_sharpe_ratio <- SharpeRatio(train_tbbk_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
train_tbbk_yearly_sharpe_ratio <- SharpeRatio(train_tbbk_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
train_tbbk_daily_treynor_ratio <- TreynorRatio(train_tbbk_daily_returns, Rf = daily_risk_free_rate, Rb = train_market_daily_returns)
train_tbbk_yearly_treynor_ratio <- TreynorRatio(train_tbbk_yearly_returns, Rf = yearly_risk_free_rate, Rb = train_market_yearly_returns)

#### Information Ratio
train_tbbk_daily_information_ratio <- InformationRatio(train_tbbk_daily_returns, train_market_daily_returns)
train_tbbk_yearly_information_ratio <- InformationRatio(train_tbbk_yearly_returns, train_market_yearly_returns)

### Print the Additional Analysis results
print("TBBK Daily Data 2009 - 2018")
cat("Sharpe Ratio:", train_tbbk_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_tbbk_daily_treynor_ratio, "\n")
cat("Information Ratio:", train_tbbk_daily_information_ratio, "\n")

print("TBBK Yearly Data 2009 - 2018")
cat("Sharpe Ratio:", train_tbbk_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_tbbk_yearly_treynor_ratio, "\n")
cat("Information Ratio:", train_tbbk_yearly_information_ratio, "\n")


## test
### Load Data
getSymbols("TBBK", from = test_start_date, to = test_end_date)
test_tbbk_prices <- Cl(get("TBBK"))
test_tbbk_daily_returns <- dailyReturn(test_tbbk_prices)
colnames(test_tbbk_daily_returns) <- "return"
test_tbbk_yearly_returns <- period.apply(test_tbbk_daily_returns, endpoints(test_tbbk_daily_returns, on = "years"), 
                                         function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(test_tbbk_daily_returns))
print(test_tbbk_yearly_returns)

### Plot Data
autoplot(test_tbbk_prices, colour="blue") + 
  labs(title = "TBBK Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(test_tbbk_daily_returns, colour="blue") + 
  labs(title = "TBBK Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


### Additional Analysis
#### Sharpe Ratio
test_tbbk_daily_sharpe_ratio <- SharpeRatio(test_tbbk_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
test_tbbk_yearly_sharpe_ratio <- SharpeRatio(test_tbbk_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
test_tbbk_daily_treynor_ratio <- TreynorRatio(test_tbbk_daily_returns, Rf = daily_risk_free_rate, Rb = test_market_daily_returns)
test_tbbk_yearly_treynor_ratio <- TreynorRatio(test_tbbk_yearly_returns, Rf = yearly_risk_free_rate, Rb = test_market_yearly_returns)

#### Information Ratio
test_tbbk_daily_information_ratio <- InformationRatio(test_tbbk_daily_returns, test_market_daily_returns)
test_tbbk_yearly_information_ratio <- InformationRatio(test_tbbk_yearly_returns, test_market_yearly_returns)

### Print the Additional Analysis results
print("TBBK Daily Data 2019")
cat("Sharpe Ratio:", test_tbbk_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_tbbk_daily_treynor_ratio, "\n")
cat("Information Ratio:", test_tbbk_daily_information_ratio, "\n")

print("TBBK Yearly Data 2019")
cat("Sharpe Ratio:", test_tbbk_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_tbbk_yearly_treynor_ratio, "\n")
cat("Information Ratio:", test_tbbk_yearly_information_ratio, "\n")

# 5. JPM
## train
### Load Data
getSymbols("JPM", from = train_start_date, to = train_end_date)
train_jpm_prices <- Cl(get("JPM"))
train_jpm_daily_returns <- dailyReturn(train_jpm_prices)
colnames(train_jpm_daily_returns) <- "return"
train_jpm_yearly_returns <- period.apply(train_jpm_daily_returns, endpoints(train_jpm_daily_returns, on = "years"), 
                                         function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(train_jpm_daily_returns))
print(train_jpm_yearly_returns)
print(summary(train_jpm_yearly_returns))

### Plot Data
autoplot(train_jpm_prices, colour="blue") + 
  labs(title = "JPM Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_jpm_daily_returns, colour="blue") + 
  labs(title = "JPM Daily Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_jpm_yearly_returns, colour="blue") + 
  labs(title = "JPM Yearly Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

### Additional Analysis
#### Sharpe Ratio
train_jpm_daily_sharpe_ratio <- SharpeRatio(train_jpm_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
train_jpm_yearly_sharpe_ratio <- SharpeRatio(train_jpm_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
train_jpm_daily_treynor_ratio <- TreynorRatio(train_jpm_daily_returns, Rf = daily_risk_free_rate, Rb = train_market_daily_returns)
train_jpm_yearly_treynor_ratio <- TreynorRatio(train_jpm_yearly_returns, Rf = yearly_risk_free_rate, Rb = train_market_yearly_returns)

#### Information Ratio
train_jpm_daily_information_ratio <- InformationRatio(train_jpm_daily_returns, train_market_daily_returns)
train_jpm_yearly_information_ratio <- InformationRatio(train_jpm_yearly_returns, train_market_yearly_returns)

### Print the Additional Analysis results
print("JPM Daily Data 2009 - 2018")
cat("Sharpe Ratio:", train_jpm_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_jpm_daily_treynor_ratio, "\n")
cat("Information Ratio:", train_jpm_daily_information_ratio, "\n")

print("JPM Yearly Data 2009 - 2018")
cat("Sharpe Ratio:", train_jpm_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_jpm_yearly_treynor_ratio, "\n")
cat("Information Ratio:", train_jpm_yearly_information_ratio, "\n")


## test
### Load Data
getSymbols("JPM", from = test_start_date, to = test_end_date)
test_jpm_prices <- Cl(get("JPM"))
test_jpm_daily_returns <- dailyReturn(test_jpm_prices)
colnames(test_jpm_daily_returns) <- "return"
test_jpm_yearly_returns <- period.apply(test_jpm_daily_returns, endpoints(test_jpm_daily_returns, on = "years"), 
                                        function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(test_jpm_daily_returns))
print(test_jpm_yearly_returns)

### Plot Data
autoplot(test_jpm_prices, colour="blue") + 
  labs(title = "JPM Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(test_jpm_daily_returns, colour="blue") + 
  labs(title = "JPM Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


### Additional Analysis
#### Sharpe Ratio
test_jpm_daily_sharpe_ratio <- SharpeRatio(test_jpm_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
test_jpm_yearly_sharpe_ratio <- SharpeRatio(test_jpm_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
test_jpm_daily_treynor_ratio <- TreynorRatio(test_jpm_daily_returns, Rf = daily_risk_free_rate, Rb = test_market_daily_returns)
test_jpm_yearly_treynor_ratio <- TreynorRatio(test_jpm_yearly_returns, Rf = yearly_risk_free_rate, Rb = test_market_yearly_returns)

#### Information Ratio
test_jpm_daily_information_ratio <- InformationRatio(test_jpm_daily_returns, test_market_daily_returns)
test_jpm_yearly_information_ratio <- InformationRatio(test_jpm_yearly_returns, test_market_yearly_returns)

### Print the Additional Analysis results
print("JPM Daily Data 2019")
cat("Sharpe Ratio:", test_jpm_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_jpm_daily_treynor_ratio, "\n")
cat("Information Ratio:", test_jpm_daily_information_ratio, "\n")

print("JPM Yearly Data 2019")
cat("Sharpe Ratio:", test_jpm_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_jpm_yearly_treynor_ratio, "\n")
cat("Information Ratio:", test_jpm_yearly_information_ratio, "\n")




## =============================== ANALISIS KESELURUHAN ======================================== ##
# train
# Merge the data of 5 stocks : price
train_data_prices <- merge(train_market_prices, train_t_prices, train_key_prices, train_cvx_prices, train_tbbk_prices, train_jpm_prices, all = TRUE)
print(summary(train_data_prices))

# Merge the data of 5 stocks: daily return
train_data_daily_returns <- merge(na.omit(train_market_daily_returns), train_t_daily_returns, train_key_daily_returns, train_cvx_daily_returns,
                            train_tbbk_daily_returns, train_jpm_daily_returns, all=TRUE)
colnames(train_data_daily_returns) <- c( "return index", "return T", "return KEY", "return CVX", "return TBBK", "return JPM")
print(summary(train_data_daily_returns))

# Merge the data of 5 stocks: yearly return
train_data_yearly_returns <- merge(na.omit(train_market_yearly_returns), train_t_yearly_returns, train_key_yearly_returns, train_cvx_yearly_returns,
                                   train_tbbk_yearly_returns, train_jpm_yearly_returns, all=TRUE)
colnames(train_data_yearly_returns) <- c( "return index", "return T", "return KEY", "return CVX", "return TBBK", "return JPM")
print(summary(train_data_yearly_returns))


# test
# Merge the data of 5 stocks : price
test_data_prices <- merge(test_market_prices, test_t_prices, test_key_prices, test_cvx_prices, test_tbbk_prices, test_jpm_prices, all = TRUE)
print(summary(test_data_prices))

# Merge the data of 5 stocks: daily return
test_data_daily_returns <- merge(na.omit(test_market_daily_returns), test_t_daily_returns, test_key_daily_returns, test_cvx_daily_returns,
                                 test_tbbk_daily_returns, test_jpm_daily_returns, all=TRUE)
colnames(test_data_daily_returns) <- c( "return index", "return T", "return KEY", "return CVX", "return TBBK", "return JPM")
print(summary(test_data_daily_returns))

# Merge the data of 5 stocks: yearly return
test_data_yearly_returns <- merge(na.omit(test_market_yearly_returns), test_t_yearly_returns, test_key_yearly_returns, test_cvx_yearly_returns,
                                  test_tbbk_yearly_returns, test_jpm_yearly_returns, all=TRUE)
colnames(test_data_yearly_returns) <- c( "return index", "return T", "return KEY", "return CVX", "return TBBK", "return JPM")
print(summary(test_data_yearly_returns))

## ========================================= Capital Allocation Line (CAL) ========================================== ##

# Train

## Daily
# Expected return
train_daily_Er_t = mean(train_t_daily_returns)
train_daily_Er_key = mean(train_key_daily_returns)
train_daily_Er_cvx = mean(train_cvx_daily_returns)
train_daily_Er_tbbk = mean(train_tbbk_daily_returns)
train_daily_Er_jpm = mean(train_jpm_daily_returns)
train_daily_Er_market = mean(na.omit(train_market_daily_returns))

# Standard Deviation
train_daily_SD_t = sqrt(var(train_t_daily_returns))
train_daily_SD_key = sqrt(var(train_key_daily_returns))
train_daily_SD_cvx = sqrt(var(train_cvx_daily_returns))
train_daily_SD_tbbk = sqrt(var(train_tbbk_daily_returns))
train_daily_SD_jpm = sqrt(var(train_jpm_daily_returns))
train_daily_SD_market = sqrt(var(na.omit(train_market_daily_returns)))

# Covariance with the market
train_daily_cov_t = cov(train_t_daily_returns, train_market_daily_returns)
train_daily_cov_key = cov(train_key_daily_returns, train_market_daily_returns)
train_daily_cov_cvx = cov(train_cvx_daily_returns, train_market_daily_returns)
train_daily_cov_tbbk = cov(train_tbbk_daily_returns, train_market_daily_returns)
train_daily_cov_jpm = cov(train_jpm_daily_returns, train_market_daily_returns)

Index = c("T", "KEY", "CVX", "TBBK", "JPM", "W5000 (Market)")
daily_Return = c(train_daily_Er_t, train_daily_Er_key, train_daily_Er_cvx, train_daily_Er_tbbk, train_daily_Er_jpm, train_daily_Er_market)
daily_SD = c(train_daily_SD_t, train_daily_SD_key, train_daily_SD_cvx, train_daily_SD_tbbk, train_daily_SD_jpm, train_daily_SD_market)
daily_cov = c (train_daily_cov_t, train_daily_cov_key, train_daily_cov_cvx, train_daily_cov_tbbk, train_daily_cov_jpm, train_daily_SD_market^2)
table = data.frame(Index, daily_Return, daily_SD, daily_cov)
table

#Index daily_Return   daily_SD    daily_cov
#1              T 5.232909e-05 0.01142806 6.697254e-05
#2            KEY 5.708255e-04 0.02732441 1.913578e-04
#3            CVX 2.378816e-04 0.01402987 1.094089e-04
#4           TBBK 7.108339e-04 0.02889587 1.446846e-04
#5            JPM 6.931458e-04 0.02229409 1.779741e-04
#6 W5000 (Market) 5.306390e-04 0.01628567 2.652231e-04
ggplot(table) + geom_abline(aes(intercept = mean(daily_risk_free_rate), slope = train_daily_Er_market/ train_daily_SD_market), 
color = "red") + xlim(0, 0.018) + ylim(0.00005, 0.001) +  
  labs(title = "Capital Allocation Line", x = "Standard Deviation" , y = "Daily Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


## Yearly
# Expected return
train_yearly_Er_t = mean(train_t_yearly_returns)
train_yearly_Er_key = mean(train_key_yearly_returns)
train_yearly_Er_cvx = mean(train_cvx_yearly_returns)
train_yearly_Er_tbbk = mean(train_tbbk_yearly_returns)
train_yearly_Er_jpm = mean(train_jpm_yearly_returns)
train_yearly_Er_market = mean(na.omit(train_market_yearly_returns))

# Standard Deviation
train_yearly_SD_t = sqrt(var(train_t_yearly_returns))
train_yearly_SD_key = sqrt(var(train_key_yearly_returns))
train_yearly_SD_cvx = sqrt(var(train_cvx_yearly_returns))
train_yearly_SD_tbbk = sqrt(var(train_tbbk_yearly_returns))
train_yearly_SD_jpm = sqrt(var(train_jpm_yearly_returns))
train_yearly_SD_market = sqrt(var(na.omit(train_market_yearly_returns)))

# Covariance with the market
train_yearly_cov_t = cov(train_t_yearly_returns, train_market_yearly_returns)
train_yearly_cov_key = cov(train_key_yearly_returns, train_market_yearly_returns)
train_yearly_cov_cvx = cov(train_cvx_yearly_returns, train_market_yearly_returns)
train_yearly_cov_tbbk = cov(train_tbbk_yearly_returns, train_market_yearly_returns)
train_yearly_cov_jpm = cov(train_jpm_yearly_returns, train_market_yearly_returns)

Index = c("T", "KEY", "CVX", "TBBK", "JPM", "W5000 (Market)")
yearly_Return = c(train_yearly_Er_t, train_yearly_Er_key, train_yearly_Er_cvx, train_yearly_Er_tbbk, train_yearly_Er_jpm, train_yearly_Er_market)
yearly_SD = c(train_yearly_SD_t, train_yearly_SD_key, train_yearly_SD_cvx, train_yearly_SD_tbbk, train_yearly_SD_jpm, train_yearly_SD_market)
yearly_cov = c (train_yearly_cov_t, train_yearly_cov_key, train_yearly_cov_cvx, train_yearly_cov_tbbk, train_yearly_cov_jpm, train_yearly_SD_market^2)
table = data.frame(Index, yearly_Return, yearly_SD, yearly_cov)
table

#Index yearly_Return yearly_SD  yearly_cov
#1              T   0.004986114 0.1323483 0.005077019
#2            KEY   0.097614582 0.3360218 0.020639443
#3            CVX   0.046870721 0.1600232 0.008542002
#4           TBBK   0.163241302 0.4525715 0.045840306
#5            JPM   0.136025669 0.1976355 0.018772382
#6 W5000 (Market)   0.111694921 0.1224875 0.015003199

ggplot(table) + geom_abline(aes(intercept = mean(yearly_risk_free_rate), slope = train_yearly_Er_market/ train_yearly_SD_market), 
                            color = "red") + xlim(0, 0.25) + ylim(0.02, 0.3) +  
  labs(title = "Capital Allocation Line", x = "Standard Deviation" , y = "Yearly Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


## ================== GRAFIK KESELURUHAN ======================= ##
autoplot(train_key_prices, colour="blue") + 
  labs(title = "KEY Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

colnames(test_data_prices) <- c("W5000", "T", "KEY", "CVX", "TBBK", "JPM")
df <- data.frame(date=index(test_data_prices), coredata(test_data_prices))
df
ggplot(data=df, aes(x = date)) +   
  geom_line(aes(y = T, color = "T")) + 
  geom_line(aes(y = KEY, color = "KEY")) + 
  geom_line(aes(y = TBBK, color = "TBBK")) + 
  geom_line(aes(y = JPM, color = "JPM")) +
  geom_line(aes(y = CVX, color = "CVX")) + 
  labs(title = "5 Wilshare 5000 Stocks Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

colnames(train_data_yearly_returns) <- c("W5000", "T", "KEY", "CVX", "TBBK", "JPM")
df_yearly_ret <- data.frame(date=index(train_data_yearly_returns), coredata(train_data_yearly_returns))
df_yearly_ret 

ggplot(data=df_yearly_ret, aes(x = date)) +   
  geom_line(aes(y = T, color = "T")) + 
  geom_line(aes(y = KEY, color = "KEY")) + 
  geom_line(aes(y = TBBK, color = "TBBK")) + 
  geom_line(aes(y = JPM, color = "JPM")) +
  geom_line(aes(y = CVX, color = "CVX")) + 
  labs(title = "5 Wilshare 5000 stocks yearly return 2009-2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

colnames(test_data_daily_returns) <- c("W5000", "T", "KEY", "CVX", "TBBK", "JPM")
df_daily_ret <- data.frame(date=index(test_data_daily_returns), coredata(test_data_daily_returns))
df_daily_ret 

ggplot(data=df_daily_ret, aes(x = date)) +   
  geom_line(aes(y = T, color = "T")) + 
  geom_line(aes(y = KEY, color = "KEY")) + 
  geom_line(aes(y = TBBK, color = "TBBK")) + 
  geom_line(aes(y = JPM, color = "JPM")) +
  geom_line(aes(y = CVX, color = "CVX")) + 
  labs(title = "5 Wilshare 5000 Stocks daily return 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

test_data_yearly_returns

# Expected return
test_daily_Er_t = mean(test_t_daily_returns)
test_daily_Er_key = mean(test_key_daily_returns)
test_daily_Er_cvx = mean(test_cvx_daily_returns)
test_daily_Er_tbbk = mean(test_tbbk_daily_returns)
test_daily_Er_jpm = mean(test_jpm_daily_returns)
test_daily_Er_market = mean(na.omit(test_market_daily_returns))

# Standard Deviation
test_daily_SD_t = sqrt(var(test_t_daily_returns))
test_daily_SD_key = sqrt(var(test_key_daily_returns))
test_daily_SD_cvx = sqrt(var(test_cvx_daily_returns))
test_daily_SD_tbbk = sqrt(var(test_tbbk_daily_returns))
test_daily_SD_jpm = sqrt(var(test_jpm_daily_returns))
test_daily_SD_market = sqrt(var(na.omit(test_market_daily_returns)))

# Covariance with the market
test_daily_cov_t = cov(test_t_daily_returns, test_market_daily_returns)
test_daily_cov_key = cov(test_key_daily_returns, test_market_daily_returns)
test_daily_cov_cvx = cov(test_cvx_daily_returns, test_market_daily_returns)
test_daily_cov_tbbk = cov(test_tbbk_daily_returns, test_market_daily_returns)
test_daily_cov_jpm = cov(test_jpm_daily_returns, test_market_daily_returns)

Index = c("T", "KEY", "CVX", "TBBK", "JPM", "W5000 (Market)")
daily_Return = c(test_daily_Er_t, test_daily_Er_key, test_daily_Er_cvx, test_daily_Er_tbbk, test_daily_Er_jpm, test_daily_Er_market)
daily_SD = c(test_daily_SD_t, test_daily_SD_key, test_daily_SD_cvx, test_daily_SD_tbbk, test_daily_SD_jpm, test_daily_SD_market)
daily_var = daily_SD^2
mean2_to_var = daily_Return^2/daily_var
daily_cov = c (test_daily_cov_t, test_daily_cov_key, test_daily_cov_cvx, test_daily_cov_tbbk, test_daily_cov_jpm, test_daily_SD_market^2)
table = data.frame(Index, daily_Return, daily_var, mean_to_var, daily_cov)
table

(data.frame(daily_SD))

(data.frame(test_data_yearly_returns))

# Expected return
test_yearly_Er_t = mean(test_t_yearly_returns)
test_yearly_Er_key = mean(test_key_yearly_returns)
test_yearly_Er_cvx = mean(test_cvx_yearly_returns)
test_yearly_Er_tbbk = mean(test_tbbk_yearly_returns)
test_yearly_Er_jpm = mean(test_jpm_yearly_returns)
test_yearly_Er_market = mean(na.omit(test_market_yearly_returns))

# Standard Deviation
test_yearly_SD_t = sqrt(var(test_t_yearly_returns))
test_yearly_SD_key = sqrt(var(test_key_yearly_returns))
test_yearly_SD_cvx = sqrt(var(test_cvx_yearly_returns))
test_yearly_SD_tbbk = sqrt(var(test_tbbk_yearly_returns))
test_yearly_SD_jpm = sqrt(var(test_jpm_yearly_returns))
test_yearly_SD_market = sqrt(var(na.omit(test_market_yearly_returns)))

# Covariance with the market
test_yearly_cov_t = cov(test_t_yearly_returns, test_market_yearly_returns)
test_yearly_cov_key = cov(test_key_yearly_returns, test_market_yearly_returns)
test_yearly_cov_cvx = cov(test_cvx_yearly_returns, test_market_yearly_returns)
test_yearly_cov_tbbk = cov(test_tbbk_yearly_returns, test_market_yearly_returns)
test_yearly_cov_jpm = cov(test_jpm_yearly_returns, test_market_yearly_returns)

Index = c("T", "KEY", "CVX", "TBBK", "JPM", "W5000 (Market)")
yearly_Return = c(test_yearly_Er_t, test_yearly_Er_key, test_yearly_Er_cvx, test_yearly_Er_tbbk, test_yearly_Er_jpm, test_yearly_Er_market)
yearly_SD = c(test_yearly_SD_t, test_yearly_SD_key, test_yearly_SD_cvx, test_yearly_SD_tbbk, test_yearly_SD_jpm, test_yearly_SD_market)
yearly_cov = c (test_yearly_cov_t, test_yearly_cov_key, test_yearly_cov_cvx, test_yearly_cov_tbbk, test_yearly_cov_jpm, test_yearly_SD_market^2)
table = data.frame(Index, yearly_Return, yearly_SD, yearly_cov)
table



