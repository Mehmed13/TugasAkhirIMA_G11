# Import package
library(ggplot2)
library(readr)
library(dplyr)
library(magrittr)

# load dataset
wilshire <- read_csv("D:/KULIAH/SEMESTER 5/IMA/Tugas Akhir/W5000.csv")
apple <- read_csv("D:/KULIAH/SEMESTER 5/IMA/Tugas Akhir/AAPL.csv")
chevron <- read_csv("D:/KULIAH/SEMESTER 5/IMA/Tugas Akhir/CVX.csv")
jp_morgan <- read_csv("D:/KULIAH/SEMESTER 5/IMA/Tugas Akhir/JPM.csv")
coca_cola <- read_csv("D:/KULIAH/SEMESTER 5/IMA/Tugas Akhir/KO.csv")
vertex <- read_csv("D:/KULIAH/SEMESTER 5/IMA/Tugas Akhir/VRTX.csv")

# Annual T-bills rate 2019: 1.99%
t_bill <- (1+0.0199)^(1/252) - 1

# Split the data
get_test <- function(dataset){
  return ((dataset %>% filter(Date >= "2019-01-01" & Date <= "2019-12-31"))[c("Date", "Close")])
}

get_train <- function(dataset){
  return ((dataset %>% filter(Date >= "2009-01-01" & Date < "2019-01-01"))[c("Date", "Close")])
}

wilshire_train <- get_train(wilshire)
apple_train <- get_train(apple)
chevron_train <- get_train(chevron)
jp_morgan_train <- get_train(jp_morgan)
coca_cola_train <- get_train(coca_cola)
vertex_train <- get_train(vertex)

wilshire_test <- get_test(wilshire)
apple_test <- get_test(apple)
chevron_test <- get_test(chevron)
jp_morgan_test <- get_test(jp_morgan)
coca_cola_test <- get_test(coca_cola)
vertex_test <- get_test(vertex)

# Data Statistic 

## train
data_train <- data.frame(wilshire_train$Date, as.numeric(wilshire_train$Close), apple_train$Close, chevron_train$Close,
                        jp_morgan_train$Close, coca_cola_train$Close, vertex_train$Close)
names(data_train)[c(1, 2, 3, 4, 5, 6, 7)] <- c("date", "wilshire_price", "apple_price", "chevron_price",
                                              "jp_morgan_price", "coca_cola_price", "vertex_price")

summary(data_train)


## test
data_test <- data.frame(wilshire_test$Date, as.numeric(wilshire_test$Close), apple_test$Close, chevron_test$Close,
                   jp_morgan_test$Close, coca_cola_test$Close, vertex_test$Close)
names(data_test)[c(1, 2, 3, 4, 5, 6, 7)] <- c("date", "wilshire_price", "apple_price", "chevron_price",
                                         "jp_morgan_price", "coca_cola_price", "vertex_price")

summary(data_test)

## ========================================================== ##

## Nanti diganti
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



