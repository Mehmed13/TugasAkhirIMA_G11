library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(ggfortify)

start_date <- "2019-01-01"
end_date <- "2019-12-31"

# Index Wilshire 5000 (Market)
getSymbols("^W5000", from = start_date, to = end_date)
market_prices <- Cl(get("W5000"))
market_returns <- dailyReturn(market_prices)

print(summary(market_returns))


autoplot(market_returns, colour="blue") + 
  labs(title = "Wilshire 5000 Index Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

autoplot(market_returns, colour="blue") + 
  labs(title = "Wilshire 5000 Index Daily Returns 2019", x="Date", y="Return") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

# 1. AAPL
AAPL <- read.csv("D:/KULIAH/SEMESTER 5/IMA/Tugas Akhir/AAPL.csv")
#AAPL_prices <- Cl(get("AAPL"))