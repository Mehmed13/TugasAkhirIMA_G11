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



# 1. AAPL
## train
### Load Data
getSymbols("AAPL", from = train_start_date, to = train_end_date)
train_aapl_prices <- Cl(get("AAPL"))
train_aapl_daily_returns <- dailyReturn(train_aapl_prices)
colnames(train_aapl_daily_returns) <- "return"
train_aapl_yearly_returns <- period.apply(train_aapl_daily_returns, endpoints(train_aapl_daily_returns, on = "years"), 
                                          function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(train_aapl_daily_returns))
print(train_aapl_yearly_returns)
print(summary(train_aapl_yearly_returns))

### Plot Data
autoplot(train_aapl_prices, colour="blue") + 
  labs(title = "AAPL Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_aapl_daily_returns, colour="blue") + 
  labs(title = "AAPL Daily Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_aapl_yearly_returns, colour="blue") + 
  labs(title = "AAPL Yearly Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

### Additional Analysis
#### Sharpe Ratio
train_aapl_daily_sharpe_ratio <- SharpeRatio(train_aapl_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
train_aapl_yearly_sharpe_ratio <- SharpeRatio(train_aapl_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
train_aapl_daily_treynor_ratio <- TreynorRatio(train_aapl_daily_returns, Rf = daily_risk_free_rate, Rb = train_market_daily_returns)
train_aapl_yearly_treynor_ratio <- TreynorRatio(train_aapl_yearly_returns, Rf = yearly_risk_free_rate, Rb = train_market_yearly_returns)

#### Information Ratio
train_aapl_daily_information_ratio <- InformationRatio(train_aapl_daily_returns, train_market_daily_returns)
train_aapl_yearly_information_ratio <- InformationRatio(train_aapl_yearly_returns, train_market_yearly_returns)

### Print the Additional Analysis results
print("AAPL Daily Data 2009 - 2018")
cat("Sharpe Ratio:", train_aapl_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_aapl_daily_treynor_ratio, "\n")
cat("Information Ratio:", train_aapl_daily_information_ratio, "\n")

print("AAPL Yearly Data 2009 - 2018")
cat("Sharpe Ratio:", train_aapl_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_aapl_yearly_treynor_ratio, "\n")
cat("Information Ratio:", train_aapl_yearly_information_ratio, "\n")


## test
### Load Data
getSymbols("AAPL", from = test_start_date, to = test_end_date)
test_aapl_prices <- Cl(get("AAPL"))
test_aapl_daily_returns <- dailyReturn(test_aapl_prices)
colnames(test_aapl_daily_returns) <- "return"
test_aapl_yearly_returns <- period.apply(test_aapl_daily_returns, endpoints(test_aapl_daily_returns, on = "years"), 
                                         function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(test_aapl_daily_returns))
print(test_aapl_yearly_returns)

### Plot Data
autoplot(test_aapl_prices, colour="blue") + 
  labs(title = "AAPL Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(test_aapl_daily_returns, colour="blue") + 
  labs(title = "AAPL Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


### Additional Analysis
#### Sharpe Ratio
test_aapl_daily_sharpe_ratio <- SharpeRatio(test_aapl_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
test_aapl_yearly_sharpe_ratio <- SharpeRatio(test_aapl_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
test_aapl_daily_treynor_ratio <- TreynorRatio(test_aapl_daily_returns, Rf = daily_risk_free_rate, Rb = test_market_daily_returns)
test_aapl_yearly_treynor_ratio <- TreynorRatio(test_aapl_yearly_returns, Rf = yearly_risk_free_rate, Rb = test_market_yearly_returns)

#### Information Ratio
test_aapl_daily_information_ratio <- InformationRatio(test_aapl_daily_returns, test_market_daily_returns)
test_aapl_yearly_information_ratio <- InformationRatio(test_aapl_yearly_returns, test_market_yearly_returns)

### Print the Additional Analysis results
print("AAPL Daily Data 2019")
cat("Sharpe Ratio:", test_aapl_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_aapl_daily_treynor_ratio, "\n")
cat("Information Ratio:", test_aapl_daily_information_ratio, "\n")

print("AAPL Yearly Data 2019")
cat("Sharpe Ratio:", test_aapl_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_aapl_yearly_treynor_ratio, "\n")
cat("Information Ratio:", test_aapl_yearly_information_ratio, "\n")

# 2. KO
## train
### Load Data
getSymbols("KO", from = train_start_date, to = train_end_date)
train_ko_prices <- Cl(get("KO"))
train_ko_daily_returns <- dailyReturn(train_ko_prices)
colnames(train_ko_daily_returns) <- "return"
train_ko_yearly_returns <- period.apply(train_ko_daily_returns, endpoints(train_ko_daily_returns, on = "years"), 
                                        function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(train_ko_daily_returns))
print(train_ko_yearly_returns)
print(summary(train_ko_yearly_returns))

### Plot Data
autoplot(train_ko_prices, colour="blue") + 
  labs(title = "KO Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_ko_daily_returns, colour="blue") + 
  labs(title = "KO Daily Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_ko_yearly_returns, colour="blue") + 
  labs(title = "KO Yearly Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

### Additional Analysis
#### Sharpe Ratio
train_ko_daily_sharpe_ratio <- SharpeRatio(train_ko_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
train_ko_yearly_sharpe_ratio <- SharpeRatio(train_ko_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
train_ko_daily_treynor_ratio <- TreynorRatio(train_ko_daily_returns, Rf = daily_risk_free_rate, Rb = train_market_daily_returns)
train_ko_yearly_treynor_ratio <- TreynorRatio(train_ko_yearly_returns, Rf = yearly_risk_free_rate, Rb = train_market_yearly_returns)

#### Information Ratio
train_ko_daily_information_ratio <- InformationRatio(train_ko_daily_returns, train_market_daily_returns)
train_ko_yearly_information_ratio <- InformationRatio(train_ko_yearly_returns, train_market_yearly_returns)

### Print the Additional Analysis results
print("KO Daily Data 2009 - 2018")
cat("Sharpe Ratio:", train_ko_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_ko_daily_treynor_ratio, "\n")
cat("Information Ratio:", train_ko_daily_information_ratio, "\n")

print("KO Yearly Data 2009 - 2018")
cat("Sharpe Ratio:", train_ko_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_ko_yearly_treynor_ratio, "\n")
cat("Information Ratio:", train_ko_yearly_information_ratio, "\n")


## test
### Load Data
getSymbols("KO", from = test_start_date, to = test_end_date)
test_ko_prices <- Cl(get("KO"))
test_ko_daily_returns <- dailyReturn(test_ko_prices)
colnames(test_ko_daily_returns) <- "return"
test_ko_yearly_returns <- period.apply(test_ko_daily_returns, endpoints(test_ko_daily_returns, on = "years"), 
                                       function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(test_ko_daily_returns))
print(test_ko_yearly_returns)

### Plot Data
autoplot(test_ko_prices, colour="blue") + 
  labs(title = "KO Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(test_ko_daily_returns, colour="blue") + 
  labs(title = "KO Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


### Additional Analysis
#### Sharpe Ratio
test_ko_daily_sharpe_ratio <- SharpeRatio(test_ko_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
test_ko_yearly_sharpe_ratio <- SharpeRatio(test_ko_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
test_ko_daily_treynor_ratio <- TreynorRatio(test_ko_daily_returns, Rf = daily_risk_free_rate, Rb = test_market_daily_returns)
test_ko_yearly_treynor_ratio <- TreynorRatio(test_ko_yearly_returns, Rf = yearly_risk_free_rate, Rb = test_market_yearly_returns)

#### Information Ratio
test_ko_daily_information_ratio <- InformationRatio(test_ko_daily_returns, test_market_daily_returns)
test_ko_yearly_information_ratio <- InformationRatio(test_ko_yearly_returns, test_market_yearly_returns)

### Print the Additional Analysis results
print("KO Daily Data 2019")
cat("Sharpe Ratio:", test_ko_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_ko_daily_treynor_ratio, "\n")
cat("Information Ratio:", test_ko_daily_information_ratio, "\n")

print("KO Yearly Data 2019")
cat("Sharpe Ratio:", test_ko_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_ko_yearly_treynor_ratio, "\n")
cat("Information Ratio:", test_ko_yearly_information_ratio, "\n")

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

# 4. VRTX
## train
### Load Data
getSymbols("VRTX", from = train_start_date, to = train_end_date)
train_vrtx_prices <- Cl(get("VRTX"))
train_vrtx_daily_returns <- dailyReturn(train_vrtx_prices)
colnames(train_vrtx_daily_returns) <- "return"
train_vrtx_yearly_returns <- period.apply(train_vrtx_daily_returns, endpoints(train_vrtx_daily_returns, on = "years"), 
                                          function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(train_vrtx_daily_returns))
print(train_vrtx_yearly_returns)
print(summary(train_vrtx_yearly_returns))

### Plot Data
autoplot(train_vrtx_prices, colour="blue") + 
  labs(title = "VRTX Price 2009 - 2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_vrtx_daily_returns, colour="blue") + 
  labs(title = "VRTX Daily Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(train_vrtx_yearly_returns, colour="blue") + 
  labs(title = "VRTX Yearly Returns 2009 - 2018", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

### Additional Analysis
#### Sharpe Ratio
train_vrtx_daily_sharpe_ratio <- SharpeRatio(train_vrtx_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
train_vrtx_yearly_sharpe_ratio <- SharpeRatio(train_vrtx_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
train_vrtx_daily_treynor_ratio <- TreynorRatio(train_vrtx_daily_returns, Rf = daily_risk_free_rate, Rb = train_market_daily_returns)
train_vrtx_yearly_treynor_ratio <- TreynorRatio(train_vrtx_yearly_returns, Rf = yearly_risk_free_rate, Rb = train_market_yearly_returns)

#### Information Ratio
train_vrtx_daily_information_ratio <- InformationRatio(train_vrtx_daily_returns, train_market_daily_returns)
train_vrtx_yearly_information_ratio <- InformationRatio(train_vrtx_yearly_returns, train_market_yearly_returns)

### Print the Additional Analysis results
print("VRTX Daily Data 2009 - 2018")
cat("Sharpe Ratio:", train_vrtx_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_vrtx_daily_treynor_ratio, "\n")
cat("Information Ratio:", train_vrtx_daily_information_ratio, "\n")

print("VRTX Yearly Data 2009 - 2018")
cat("Sharpe Ratio:", train_vrtx_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", train_vrtx_yearly_treynor_ratio, "\n")
cat("Information Ratio:", train_vrtx_yearly_information_ratio, "\n")


## test
### Load Data
getSymbols("VRTX", from = test_start_date, to = test_end_date)
test_vrtx_prices <- Cl(get("VRTX"))
test_vrtx_daily_returns <- dailyReturn(test_vrtx_prices)
colnames(test_vrtx_daily_returns) <- "return"
test_vrtx_yearly_returns <- period.apply(test_vrtx_daily_returns, endpoints(test_vrtx_daily_returns, on = "years"), 
                                         function(x) prod(1 + x) - 1)

### Print Statistic Data
print(summary(test_vrtx_daily_returns))
print(test_vrtx_yearly_returns)

### Plot Data
autoplot(test_vrtx_prices, colour="blue") + 
  labs(title = "VRTX Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(test_vrtx_daily_returns, colour="blue") + 
  labs(title = "VRTX Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


### Additional Analysis
#### Sharpe Ratio
test_vrtx_daily_sharpe_ratio <- SharpeRatio(test_vrtx_daily_returns, Rf = daily_risk_free_rate, FUN = "StdDev")
test_vrtx_yearly_sharpe_ratio <- SharpeRatio(test_vrtx_yearly_returns, Rf = yearly_risk_free_rate, FUN = "StdDev")

#### Treynor Ratio
test_vrtx_daily_treynor_ratio <- TreynorRatio(test_vrtx_daily_returns, Rf = daily_risk_free_rate, Rb = test_market_daily_returns)
test_vrtx_yearly_treynor_ratio <- TreynorRatio(test_vrtx_yearly_returns, Rf = yearly_risk_free_rate, Rb = test_market_yearly_returns)

#### Information Ratio
test_vrtx_daily_information_ratio <- InformationRatio(test_vrtx_daily_returns, test_market_daily_returns)
test_vrtx_yearly_information_ratio <- InformationRatio(test_vrtx_yearly_returns, test_market_yearly_returns)

### Print the Additional Analysis results
print("VRTX Daily Data 2019")
cat("Sharpe Ratio:", test_vrtx_daily_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_vrtx_daily_treynor_ratio, "\n")
cat("Information Ratio:", test_vrtx_daily_information_ratio, "\n")

print("VRTX Yearly Data 2019")
cat("Sharpe Ratio:", test_vrtx_yearly_sharpe_ratio, "\n")
cat("Treynor Ratio:", test_vrtx_yearly_treynor_ratio, "\n")
cat("Information Ratio:", test_vrtx_yearly_information_ratio, "\n")

