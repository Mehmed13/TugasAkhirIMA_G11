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

# Data Visualization
## Index
### train
ggplot(data_train, aes(x=date)) + geom_line(aes(y=wilshire_price), color='blue') +
  labs(title = "Wilshire 5000 Index Price 2009-2018", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))
### test
ggplot(data_test, aes(x=date)) + geom_line(aes(y=wilshire_price), color='blue') +
  labs(title = "Wilshire 5000 Index Price 2019", x="Date", y="Price") + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

## 5 Saham

### train
#### AAPL
ggplot(data_train, aes(x=date)) + geom_line(aes(y=apple_price), color="blue") + labs(title = "Apple Stocks Price 2009-2018", x="Date", y="Price") +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5))
#### KO
ggplot(data_train, aes(x=date)) + geom_line(aes(y=coca_cola_price), color="blue") + labs(title = "Coca Cola Stocks Price 2009-2018", x="Date", y="Price") +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5))
#### CVX
ggplot(data_train, aes(x=date)) + geom_line(aes(y=chevron_price), color="blue") + labs(title = "Chevron Stocks Price 2009-2018", x="Date", y="Price") +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5))
#### VRTX
ggplot(data_train, aes(x=date)) + geom_line(aes(y=vertex_price), color="blue") + labs(title = "Vertex Pharmaceuticals Stocks Price 2009-2018", x="Date", y="Price") +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5))
#### JPM
ggplot(data_train, aes(x=date)) + geom_line(aes(y=jp_morgan_price ), color="blue") + labs(title = "JP Morgan Stocks Price 2009-2018", x="Date", y="Price") +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5))



### Test
ggplot(data_test, aes(x=date)) + geom_line(aes(y=apple_price, color="APPL")) + geom_line(aes(y=chevron_price, color="CVX")) + 
  geom_line(aes(y=jp_morgan_price , color="JPM")) + geom_line(aes(y=coca_cola_price, color='KO')) +
  geom_line(aes(y=vertex_price, color='VRTX')) + labs(title = "5 Wilshire 5000 Stocks Price 2019", x="Date", y="Price") +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5))


## ========================================================== ##





