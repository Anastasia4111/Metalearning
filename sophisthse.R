library("tseries")
library(ggplot2)
library(forecast)
library(Tcomp)
library(doParallel)
library(dplyr)
library(tidyr)
library(xgboost)
library(M4metalearning)
library(M4metaresults)
library("ggplot2")
library("fpp2")
library("dplyr")
library("Mcomp")
library("xtable")
library("sophisthse")
library("devtools")

sophisthse_tables()
View(series_info)


# ГОДОВЫЕ ДАННЫЕ
# Прогнозы для рядов из sophisthse с частотой 1 (годовые данные)

series <- filter(series_info, freq == 1)
series_1 <- series


# Создадим список названий рядов
label <- NA
for (i in c(1:length(series$table))) {
  label <- rbind(label, series$table[i])
}

# Удалим дубликаты
label <- unique(label)[-1] 


# метод ETS

rus <- NA
rus1 <- NA
accuracy_ets <- NA


for (i in c(1:length(label))) {
  rus1 <- sophisthse(label[i])
  d <- dim(sophisthse(label[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    h <- 2
    l <- length(rus) - h
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- ets(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_ets <- rbind(accuracy_ets, accuracy_result[5])
  }
}

# метод ARIMA

rus <- NA
accuracy_arima <- NA

for (i in c(1:length(label))) {
  rus1 <- sophisthse(label[i])
  d <- dim(sophisthse(label[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    h <- 2
    l <- length(rus) - h
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- auto.arima(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_arima <- rbind(accuracy_arima, accuracy_result[5])
  }
}

sum(accuracy_arima)

# Комбинация META

rus <- NA
accuracy_meta <- NA


for (i in c(1:length(label))) {
  rus1 <- sophisthse(label[i])
  d <- dim(sophisthse(label[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    if (length(rus) > 4){
      h <- 2
      l <- length(rus) - h
      train <- ts(rus[1:l])
      test <- ts(rus[(l + 1):(l + h)])
      forecast_result <- forecast_meta_M4(model_M4, train, h = h)
      accuracy_result <- accuracy(f = forecast_result$mean, test)
      accuracy_meta <- rbind(accuracy_meta, accuracy_result[5])
    }

  }
}

c(i, j)
rus
length(rus)

# КВАРТАЛЬНЫЕ

series <- filter(series_info, freq == 4)
s_quart <- series

series$table[1]
length(series$table)

# Создадим список названий рядов
label <- NA
for (i in c(1:length(series$table))) {
  label <- rbind(label, series$table[i])
}

# Удалим дубликаты
label <- unique(label)[-1] 


# метод ETS

rus <- NA
rus1 <- NA
accuracy_ets_4 <- NA


for (i in c(1:length(label))) {
  rus1 <- sophisthse(label[i])
  d <- dim(sophisthse(label[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    h <- 4
    l <- length(rus) - h
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- ets(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_ets_4 <- rbind(accuracy_ets_4, accuracy_result[5])
  }
}

c(i, j)
# метод ARIMA

rus <- NA
accuracy_arima_4 <- NA

for (i in c(1:length(label))) {
  rus1 <- sophisthse(label[i])
  d <- dim(sophisthse(label[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    h <- 4
    l <- length(rus) - h
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- auto.arima(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_arima_4 <- rbind(accuracy_arima_4, accuracy_result[5])
  }
}

sum(accuracy_arima)

# Комбинация

rus <- NA
accuracy_meta_4 <- NA


for (i in c(1:length(label))) {
  rus1 <- sophisthse(label[i])
  d <- dim(sophisthse(label[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    if (length(rus) > 4){
      h <- 2
      l <- length(rus) - h
      train <- ts(rus[1:l])
      test <- ts(rus[(l + 1):(l + h)])
      forecast_result <- forecast_meta_M4(model_M4, train, h = h)
      accuracy_result <- accuracy(f = forecast_result$mean, test)
      accuracy_meta_4 <- rbind(accuracy_meta_4, accuracy_result[5])
    }
    
  }
}

c(i, j)
rus1



# МЕСЯЧНЫЕ
# Месячные данные

series <- filter(series_info, freq == 12)
series_12 <- series

series$table[1]
length(series$table)

# Создадим список названий рядов
label <- NA
for (i in c(1:length(series$table))) {
  label <- rbind(label, series$table[i])
}

# Удалим дубликаты
label <- unique(label)[-1] 


# метод ETS

rus <- NA
rus1 <- NA
accuracy_ets_12 <- NA


for (i in c(1:length(label))) {
  rus1 <- sophisthse(label[i])
  d <- dim(sophisthse(label[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    h <- 12
    l <- length(rus) - h
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- ets(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_ets_12 <- rbind(accuracy_ets_12, accuracy_result[5])
  }
}

c(i, j)
# метод ARIMA

rus <- NA
accuracy_arima_12 <- NA

for (i in c(1:length(label))) {
  rus1 <- sophisthse(label[i])
  d <- dim(sophisthse(label[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    h <- 12
    l <- length(rus) - h
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- auto.arima(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_arima_12 <- rbind(accuracy_arima_12, accuracy_result[5])
  }
}

sum(accuracy_arima)

# Комбинация

rus <- NA
accuracy_meta_12_1 <- NA
length(label)

for (i in c(50:length(label))) {
  rus1 <- sophisthse(label[i])
  d <- dim(sophisthse(label[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    if (length(rus) > 4){
      h <- 2
      l <- length(rus) - h
      train <- ts(rus[1:l])
      test <- ts(rus[(l + 1):(l + h)])
      forecast_result <- forecast_meta_M4(meta_model, train, h = h)
      accuracy_result <- accuracy(f = forecast_result$mean, test)
      accuracy_meta_12_1 <- rbind(accuracy_meta_12_1, accuracy_result[5])
    }
    
  }
}
length(label)
c(i, j)
rus1



# Качество прогнозов годовых

accuracy_arima <- na.omit(accuracy_arima)
accuracy_arima <- accuracy_arima[is.finite(accuracy_arima)]
mean(accuracy_arima) # 12.69631

accuracy_ets <- na.omit(accuracy_ets)
accuracy_ets <- accuracy_ets[is.finite(accuracy_ets)]
mean(accuracy_ets) # 15.90514

accuracy_meta <- na.omit(accuracy_meta)
accuracy_meta <- accuracy_meta[is.finite(accuracy_meta)]
mean(accuracy_meta) # 11.85945

# Качество прогнозов квартальных

accuracy_arima_4 <- na.omit(accuracy_arima_4)
accuracy_arima_4 <- accuracy_arima_4[is.finite(accuracy_arima_4)]
mean(accuracy_arima_4) # 11.35624

accuracy_ets_4 <- na.omit(accuracy_ets_4)
accuracy_ets_4 <- accuracy_ets_4[is.finite(accuracy_ets_4)]
mean(accuracy_ets_4) # 13.97002

accuracy_meta_4 <- na.omit(accuracy_meta_4)
accuracy_meta_4 <- accuracy_meta_4[is.finite(accuracy_meta_4)]
mean(accuracy_meta_4) # 10.19009

# Качество прогнозов месячных рядов

accuracy_arima_12 <- na.omit(accuracy_arima_12)
accuracy_arima_12 <- accuracy_arima_12[is.finite(accuracy_arima_12)]
mean(accuracy_arima_12) # 10.85314

accuracy_ets_12 <- na.omit(accuracy_ets_12)
accuracy_ets_12 <- accuracy_ets_12[is.finite(accuracy_ets_12)]
mean(accuracy_ets_12) # 14.21897

accuracy_meta_12 <- na.omit(accuracy_meta_12)
accuracy_meta_12 <- accuracy_meta_12[is.finite(accuracy_meta_12)]
mean(accuracy_meta_12) # 4.149181


# Соединим полученные результаты в одной таблице

comparison_soph <- cbind(
  Type = c("Monthly series", "Quarterly series", "Yearly series"),
  ETS = c(mean(accuracy_ets_12), mean(accuracy_ets_4), mean(accuracy_ets)),
  ARIMA = c(mean(accuracy_arima_12), mean(accuracy_arima_4), mean(accuracy_arima)),
  META = c(mean(accuracy_meta_12), mean(accuracy_meta_4), mean(accuracy_meta))
)

# SD

comparison_soph_sd <- cbind(
  Type = c("Monthly series", "Quarterly series", "Yearly series"),
  ETS = c(sd(accuracy_ets_12), sd(accuracy_ets_4), sd(accuracy_ets)),
  ARIMA = c(sd(accuracy_arima_12), sd(accuracy_arima_4), sd(accuracy_arima)),
  META = c(sd(accuracy_meta_12), sd(accuracy_meta_4), sd(accuracy_meta))
) 

# Экспорт таблицы в латех
xtable(comparison_soph, caption = "MAPE прогнозов sophisthse с помощью 3 основных методов", display = c("s", "f", "e", "E", "g"))

# Экспорт таблицы в латех
xtable(comparison_soph_sd, caption = "MAPE прогнозов sophisthse с помощью 3 основных методов", display = c("s", "f", "e", "E", "g"))

