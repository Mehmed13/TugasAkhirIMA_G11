library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(ggfortify)

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

# Index Wilshire 5000 (Market)
## train
### Load Data
getSymbols("^W5000", from = train_start_date, to = train_end_date)
train_market_prices <- na.omit(Cl(get("W5000")))
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
test_market_prices <- Cl(get("W5000"))
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
train_t_daily_information_ratio <- InformationRatio(train_t_daily_returns, train_market_daily_returns)
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




## ===============================ANALISIS KESELURUHAN======================================== ##
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




