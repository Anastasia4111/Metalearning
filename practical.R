install.packages("devtools")
library(devtools)

devtools::install_github("bdemeshev/sophisthse")
devtools::install_github("carlanetto/M4comp2018")

# Проверьте, установлены ли все нижеперечисленные пакеты Tools ~ Install Packages
library(ggplot2)
library(forecast)
library(Tcomp)
library(doParallel)
library(dplyr)
library(tidyr)
library(xgboost)
library(M4metalearning)
library(M4metaresults)
library(fpp2)
library(dplyr)
library(Mcomp)
library(M4comp2018)
library(xtable)
library(sophisthse)
library(rio)


# Для примера рассмотрим временной ряд, содержащий данные о
#  ежемесячных продажах кортикостероидов в Австралии
glimpse(h02)
autoplot(h02) + ylab("млн $") + xlab("Месяц") +
  ggtitle("Уровень продаж кортикостероидов в Австралии")

ggseasonplot(h02, polar = TRUE) +
  ylab("млн $") + xlab("Месяц") +
  ggtitle("Сезонность в полярных координатах продаж кортикостероидов в Австралии")


# Теперь попробуем 9 методов прогнозирования, использованные в алгоритме
# Поделим ряд на тренировочную и тестовую выборки
train <- window(h02, start = 1992, end = 2007)
test <- window(h02, start = 2007)

# Если кратко, можно так, или подробнее ниже
fcst_naive <- naive_forec(train, h = 18)
acc_naive <- accuracy(fcst_naive, test)

fcst_s_naive <- snaive_forec(train, h = 18)
acc_s_naive <- accuracy(fcst_s_naive, test)

fcst_arima <- auto_arima_forec(train, h = 18)
acc_arima <- accuracy(fcst_arima, test)

fcst_ets <- ets_forec(train, h = 18)
acc_ets <- accuracy(fcst_ets, test)

fcst_nnet <- nnetar_forec(train, h = 18)
acc_nnet <- accuracy(fcst_nnet, test)

fcst_tbats <- tbats_forec(train, h = 18)
acc_tbats <- accuracy(fcst_tbats, test)

fcst_stlm_ar <- stlm_ar_forec(train, h = 18)
acc_stlm_ar <- accuracy(fcst_stlm_ar, test)

fcst_rw_drift <- rw_drift_forec(train, h = 18)
acc_rw_drift <- accuracy(fcst_rw_drift, test)

fcst_theta <- thetaf_forec(train, h = 18)
acc_theta <- accuracy(fcst_theta, test)

c(acc_naive[5], acc_s_naive[5], acc_arima[5], acc_ets[5], acc_nnet[5], acc_tbats[5], acc_stlm_ar[5], acc_rw_drift[5])

accuracy(naiv$mean, test)

naive_forec()
calc_forecasts(train, methods, n.cores = 1)


# Наивный прогноз
fcst_naive <- naive(train, h = 18)

# Построим график
autoplot(fcst_naive) + xlab("Дата") + ylab("Объем продаж, млн $") + ggtitle("Прогноз объема продаж кортикостероидов в Австралии с помощью Naive")

# Проверим качество
accuracy(fcst_naive$mean, test)
#                 ME      RMSE       MAE       MPE     MAPE      ACF1
# Test set -0.3363762 0.3968374 0.3363762 -46.15849 46.15849 0.6911659


# Наивный прогноз с учетом сезонности
fcst_s_naive <- snaive(train, h = 18)

# Посмотрим график
autoplot(fcst_s_naive) + xlab("Дата") + ylab("Объем продаж, млн $") + ggtitle("Прогноз объема продаж кортикостероидов в Австралии с помощью sNaive")

# Проверим качество
accuracy(fcst_s_naive$mean, test)
#                 ME       RMSE        MAE      MPE     MAPE       ACF1 Theil's U
# Test set 0.02649856 0.08077987 0.06151007 2.444995 7.345278 -0.3012539 0.5897526



# ETS
model_ets <- ets(train)
summary(fcst_ets)
# автоматически была подобрана модель ETS(M,A,M)
fcst_ets <- forecast(model_ets , h = 18)

# Построим график
autoplot(fcst_ets) + xlab("Дата") + ylab("Объем продаж, млн $") +
  ggtitle("Прогноз объема продаж кортикостероидов в Австралии с помощью ETS")

# Проверим качество
accuracy(forets$mean, test)
#                  ME       RMSE        MAE       MPE     MAPE        ACF1 Theil's U
# Test set -0.01814064 0.07401407 0.05845077 -3.146405 7.519454 -0.05680415 0.5329896


# ARIMA
model_arima <- auto.arima(train)
summary(model_arima)

# Была выбрана
# Series: train
# ARIMA(1,0,2)(2,1,1)[12] with drift

fcst_arima <- forecast(model_arima, h = 18)

# Построим график
fit %>%
  forecast(h = 18) %>%
  autoplot() + xlab("Дата") + ylab("Объем продаж, млн $") +
  ggtitle("Прогноз объема продаж  кортикостероидов в Австралии с помощью ARIMA")

# Проверим качество
accuracy(fcst_arima$mean, test)
#                 ME       RMSE        MAE      MPE     MAPE        ACF1 Theil's U
# Test set 0.02345918 0.07143445 0.05690555 1.939375 6.971999 -0.09930066 0.5120271



# RW DRIFT
fcst_rw_drift <- rwf(train, h = 18, drift = TRUE)

# Построим график
autoplot(fcst_rw_drift) + xlab("дата") + ylab("Объем продаж, млн $") +
  ggtitle("Прогноз объема продаж  кортикостероидов в Австралии с помощью RW-Drift")

# Проверим качество
accuracy(fcst_rw_drift$mean, test)
#                  ME      RMSE       MAE       MPE     MAPE      ACF1 Theil's U
# Test set -0.3645362 0.4195183 0.3645362 -49.39236 49.39236 0.6877662  3.205116



# Theta method
fcst_theta <- thetaf(y = train, h = 18)

# Построим график
autoplot(fcst_theta) + xlab("дата") + ylab("Объем продаж, млн $") +
  ggtitle("Прогноз объема продаж  кортикостероидов в Австралии с помощью Theta-method")

# Проверим качество
accuracy(fcst_theta$mean, test)
#                  ME       RMSE        MAE        MPE     MAPE        ACF1 Theil's U
# Test set 0.003803136 0.07383127 0.06249971 -0.7075656 7.806133 0.007525667 0.5296788


# nnetar
fcst_nnet <- nnetar_forec(train, h = 18)

# Построим график
autoplot(fcast) + xlab("дата") + ylab("Объем продаж, млн $") +
  ggtitle("Прогноз объема продаж  кортикостероидов в Австралии с помощью NNAR")

# Проверим качество
accuracy(fcst_nnet, test)
#                 ME       RMSE        MAE     MPE     MAPE        ACF1 Theil's U
# Test set 0.02170774 0.07206502 0.05740173 1.55178 7.095887 -0.00333016   0.51803


# TBATS model
fcst_tbats <- tbats_forec(train, h = 18)

# Построим график
autoplot(fcst_tbats) + xlab("дата") + ylab("Объем продаж, млн $") +
  ggtitle("Прогноз объема продаж  кортикостероидов в Австралии с помощью TBATS")

# Проверим качество
accuracy(fcst_tbats, test)
#                  ME       RMSE        MAE      MPE     MAPE        ACF1 Theil's U
# Test set 0.02345918 0.07143445 0.05690555 1.939375 6.971999 -0.09930066 0.5120271


# STLM

model_stlm_ar <- stlm(train, modelfunction = ar)
fcst_stlm_ar <- forecast(model_stlm_ar, h = 18)

# Построим график
autoplot(fcst_stlm_ar) + xlab("дата") + ylab("Объем продаж, млн $") +
  ggtitle("Прогноз объема продаж  кортикостероидов в Австралии с помощью STLM")

# Проверим качество
accuracy(fcst_stlm_ar$mean, test)
#                ME       RMSE        MAE      MPE     MAPE      ACF1 Theil's U
# Test set 0.039341 0.08847129 0.07126412 3.172814 8.480873 0.2734496 0.6277155

h02 %>%
  stl(t.window = 13, s.window = "periodic", robust = TRUE) %>%
  autoplot() + ggtitle("Объема продаж  кортикостероидов в Австралии, млн$")


# Combination
forec_result <- forecast_meta_M4(model_M4, train, h = 18)
d <- accuracy(f = forec_result$mean, test, test = NULL, d = NULL, D = NULL)
d[5]
#                   ME      RMSE       MAE       MPE     MAPE        ACF1 Theil's U
# Test set -0.005720993 0.2140872 0.1345924 -4.213791 14.78935 -0.06484862 0.8277468


forec_result <- ts(forec_result$mean, start = 2007, frequency = 12)

autoplot(train) + autolayer(forec_result) + xlab("Дата") + ylab("Объем продаж, млн $") +
  ggtitle("Прогноз объема продаж кортикостероидов в Австралии с помощью META")

plot(ts(c(train, forec_result$mean),
  start = start(train), frequency = frequency(train)
),
col = "red", type = "l", ylab = "Объем продаж, млн$", xlab = "Год"
)
lines(train, col = "black")

comparison_m <- cbind(
  c("naive", "snaive", "ets", "arima", "rw-drift", "theta", "nnetar", "tbats", "stlm"),
  c(46.15849, 7.345278, 7.519454, 6.971999, 49.39236, 7.806133, 7.095887, 6.971999, 8.480873)
)

xtable(comparison_m, caption = "MAPE прогнозов базовых методов")


data <- data.frame(y = c(46.15849, 7.345278, 7.519454, 6.971999, 49.39236, 7.806133, 7.095887, 6.971999, 8.480873), x = c("naive", "snaive", "ets", "arima", "rw-drift", "theta", "nnetar", "tbats", "stlm"))
barplot(data$y)
dotchart(data$y, labels = row.names(data$x), xlab = "Миль/галлон", cex = 0.8)


# Общий график

autoplot(h02) +
  autolayer(fcst_ets, series = "ETS", PI = FALSE) +
  autolayer(fcst_arima, series = "ARIMA", PI = FALSE) +
  autolayer(fcst_naive, series = "NAIVE", PI = FALSE) +
  autolayer(fcst_s_naive, series = "SNAIV", PI = FALSE) +
  autolayer(fcst_nnet, series = "NNETAR", PI = FALSE) +
  autolayer(fcst_tbats, series = "TBATS") +
  autolayer(fcst_stlm_ar, series = "STLM") +
  autolayer(fcst_rw_drift, series = "RWD", PI = FALSE) +
  autolayer(fcst_theta, series = "THETA", PI = FALSE) +
  xlab("Год") + ylab("Объем продаж, млн$") +
  ggtitle("Прогноз объема продаж кортикостероидов в Австралии")

fcst_ets

# Для рядов из М4

# Возьмем месячные ряды

monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)

acc <- NULL
aa <- monthly_M4[1:20]
for (i in (1:10)) {
  forec_result <- forecast_meta_M4(model_M4, M4[[i]]$x, h = 12)
  temp <- accuracy(f = forec_result$mean, M4[[i]]$xx[1:12])
  acc <- rbind(acc, temp[5])
}

mean(acc)

acc1 <- NULL

for (i in (1:20)) {
  forec_result <- ets(M4[[i]]$x)
  forets_ets <- forecast(forec_result, h = 12)
  temp_ets <- accuracy(f = forets_ets$mean, M4[[i]]$xx[1:12])
  acc1 <- rbind(acc1, temp_ets[5])
}

mean(acc1)

acc2 <- NULL

for (i in (1:10)) {
  forec_result <- auto.arima(M4[[i]]$x)
  forets_ets <- forecast(forec_result, h = 12)
  temp_ets <- accuracy(f = forets_ets$mean, M4[[i]]$xx[1:12])
  acc2 <- rbind(acc_ets, temp_ets[5])
}

mean(acc2)

# M4
# годовые

monthly_M4 <- Filter(function(l) l$period == "Yearly", M4)

acc <- NULL
aa <- monthly_M4[1:20]
for (i in (1:10)) {
  forec_result <- forecast_meta_M4(model_M4, aa[[i]]$x, h = 12)
  temp <- accuracy(f = forec_result$mean, aa[[i]]$xx[1:12])
  acc <- rbind(acc, temp[5])
}

mean(acc)

acc1 <- NULL

for (i in (1:20)) {
  forec_result <- ets(aa[[i]]$x)
  forets_ets <- forecast(forec_result, h = 12)
  temp_ets <- accuracy(f = forets_ets$mean, aa[[i]]$xx[1:12])
  acc1 <- rbind(acc1, temp_ets[5])
}

mean(acc1)

acc2 <- NULL

for (i in (1:10)) {
  forec_result <- auto.arima(aa[[i]]$x)
  forets_ets <- forecast(forec_result, h = 12)
  temp_ets <- accuracy(f = forets_ets$mean, aa[[i]]$xx[1:12])
  acc2 <- rbind(acc_ets, temp_ets[5])
}

mean(acc2)

# Проверка на 50 месячных рядах

monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
aa <- monthly_M4[1:50]


acc_meta <- NULL
acc_ets <- NULL
acc_arima <- NULL

for (i in (1:50)) {
  meta_result <- forecast_meta_M4(model_M4, aa[[i]]$x, h = 12)
  meta <- accuracy(f = meta_result$mean, aa[[i]]$xx[1:12])
  acc_meta <- rbind(acc_meta, meta[5])

  ets_result <- ets(aa[[i]]$x)
  forets_ets <- forecast(ets_result, h = 12)
  ets <- accuracy(f = forets_ets$mean, aa[[i]]$xx[1:12])
  acc_ets <- rbind(acc_ets, ets[5])

  arima_result <- auto.arima(aa[[i]]$x)
  forets_arima <- forecast(arima_result, h = 12)
  aarima <- accuracy(f = forets_arima$mean, aa[[i]]$xx[1:12])
  acc_arima <- rbind(acc_arima, aarima[5])
}

mean(acc_meta)
mean(acc_ets)
mean(acc_arima)

plot(acc_meta)
plot(acc_ets)
plot(acc_arima)

# meta_month <- acc_meta
# ets_month <- acc_ets
# arima_month <- acc_arima


# Для квартальных данных

quarterly_M4 <- Filter(function(l) l$period == "Quarterly", M4)
aa <- quarterly_M4[1:50]


acc_meta <- NULL
acc_ets <- NULL
acc_arima <- NULL

for (i in (1:50)) {
  meta_result <- forecast_meta_M4(model_M4, aa[[i]]$x, h = 12)
  meta <- accuracy(f = meta_result$mean, aa[[i]]$xx[1:12])
  acc_meta <- rbind(acc_meta, meta[5])
}

for (i in (1:50)) {
  ets_result <- ets(aa[[i]]$x)
  forets_ets <- forecast(ets_result, h = 12)
  ets <- accuracy(f = forets_ets$mean, aa[[i]]$xx[1:12])
  acc_ets <- rbind(acc_ets, ets[5])

  arima_result <- auto.arima(aa[[i]]$x)
  forets_arima <- forecast(arima_result, h = 12)
  aarima <- accuracy(f = forets_arima$mean, aa[[i]]$xx[1:12])
  acc_arima <- rbind(acc_arima, aarima[5])
}

mean(acc_meta)
mean(acc_ets)
mean(acc_arima)


# meta_quarter <- acc_meta
# ets_quarter <- acc_ets
# arima_quarter <- acc_arima

# Для годовых данных

yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)
aa <- yearly_M4[1:50]


acc_meta <- NULL
acc_ets <- NULL
acc_arima <- NULL

for (i in (1:50)) {
  meta_result <- forecast_meta_M4(model_M4, aa[[i]]$x, h = 12)
  meta <- accuracy(f = meta_result$mean, aa[[i]]$xx[1:12])
  acc_meta <- rbind(acc_meta, meta[5])
}

for (i in (1:50)) {
  ets_result <- ets(aa[[i]]$x)
  forets_ets <- forecast(ets_result, h = 12)
  ets <- accuracy(f = forets_ets$mean, aa[[i]]$xx[1:12])
  acc_ets <- rbind(acc_ets, ets[5])

  arima_result <- auto.arima(aa[[i]]$x)
  forets_arima <- forecast(arima_result, h = 12)
  aarima <- accuracy(f = forets_arima$mean, aa[[i]]$xx[1:12])
  acc_arima <- rbind(acc_arima, aarima[5])
}

mean(acc_meta)
mean(acc_ets)
mean(acc_arima)

meta_yearly <- acc_meta
ets_yearly <- acc_ets
arima_yearly <- acc_arima


# Соединим полученные результаты в одной таблице

comparison <- cbind(
  Type = c("Monthly series", "Quarterly series", "Yearly series"),
  ETS = c(mean(ets_month), mean(ets_quarter), mean(ets_yearly)),
  ARIMA = c(mean(arima_month), mean(arima_quarter), mean(arima_yearly)),
  META = c(mean(meta_month), mean(meta_quarter), mean(meta_yearly))
)

# Экспорт таблицы в латех
xtable(comparison, caption = "MAPE прогнозов с помощью 3 основных методов", display = c("s", "f", "e", "E", "g"))


s1 <- sophisthse("WAG_Y")
sophisthse_tables()
View(series_info)
s_50$table

s_50$table[1]
length(s_50$table)


# Прогнозы для рядов из sophisthse с частотой 1 (годовые данные)

s <- filter(series_info, freq == 1)
s_50_1 <- s_50
# метод ETS

rus <- NA
rus1 <- NA
accuracy_ets <- NA

for (i in c(1:length(s_50$table))) {
  rus1 <- sophisthse(s_50$table[i])
  d <- dim(sophisthse(s_50$table[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- rus1[, j]
    l <- round(length(rus) * 0.8)
    h <- length(rus) - l
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- ets(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_ets <- rbind(accuracy_ets, accuracy_result[5])
  }
}

mean(accuracy_ets)

# метод ARIMA

rus <- NA
accuracy_arima <- NA

for (i in c(1:length(s_50$table))) {
  rus1 <- sophisthse(s_50$table[i])
  d <- dim(sophisthse(s_50$table[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- rus1[, j]
    l <- round(length(rus) * 0.8)
    h <- length(rus) - l
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- auto.arima(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_arima <- rbind(accuracy_arima, accuracy_result[5])
  }
}

sum(accuracy_arima)

# Комбинация

rus <- NA
accuracy_meta <- NA

for (i in c(118:length(s_50$table))) {
  rus1 <- sophisthse(s_50$table[i])
  d <- dim(sophisthse(s_50$table[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- rus1[, j]
    l <- round(length(rus) * 0.8)
    h <- length(rus) - l
    train <- ts(rus[1:l])
    test <- ts(rus[(l + 1):(l + h)])
    forecast_result <- forecast_meta_M4(model_M4, train, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_meta <- rbind(accuracy_meta, accuracy_result[5])
  }
}
i
length(s_50$table)


# Квартальные данные

s_50 <- filter(series_info, freq == 4)
s_quart <- s_50

# метод ETS

rus <- NA
rus1 <- NA
accuracy_ets_4 <- NA

for (i in c(1:length(s_50$table))) {
  rus1 <- sophisthse(s_50$table[i])
  d <- dim(sophisthse(s_50$table[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- rus1[, j]
    l <- round(length(rus) * 0.8)
    h <- length(rus) - l
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- ets(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_ets_4 <- rbind(accuracy_ets_4, accuracy_result[5])
  }
}


mean(accuracy_ets_4)
length(s_50$table)
# метод ARIMA

rus <- NA
accuracy_arima_4 <- NA

for (i in c(1:length(s_50$table))) {
  rus1 <- sophisthse(s_50$table[i])
  d <- dim(sophisthse(s_50$table[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- rus1[, j]
    l <- round(length(rus) * 0.8)
    h <- length(rus) - l
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

for (i in c(69:length(s_50$table))) {
  rus1 <- sophisthse(s_50$table[i])
  d <- dim(sophisthse(s_50$table[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- rus1[, j]
    l <- round(length(rus) * 0.8)
    h <- length(rus) - l
    train <- ts(rus[1:l])
    test <- ts(rus[(l + 1):(l + h)])
    forecast_result <- forecast_meta_M4(model_M4, train, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_meta_4 <- rbind(accuracy_meta_4, accuracy_result[5])
  }
}



# Месячные данные

s_50 <- filter(series_info, freq == 12)


# метод ETS

rus <- NA
rus1 <- NA
accuracy_ets_12 <- NA

for (i in c(1:length(s_50$table))) {
  rus1 <- sophisthse(s_50$table[i])
  d <- dim(sophisthse(s_50$table[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- rus1[, j]
    l <- round(length(rus) * 0.8)
    h <- length(rus) - l
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- ets(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_ets_12 <- rbind(accuracy_ets_12, accuracy_result[5])
  }
}


mean(accuracy_ets_12)
length(s_50$table)


# метод ARIMA

rus <- NA
accuracy_arima_12 <- NA

for (i in c(1:length(s_50$table))) {
  rus1 <- sophisthse(s_50$table[i])
  d <- dim(sophisthse(s_50$table[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- rus1[, j]
    l <- round(length(rus) * 0.8)
    h <- length(rus) - l
    train <- rus[1:l]
    test <- rus[(l + 1):(l + h)]
    model <- auto.arima(train)
    forecast_result <- forecast(model, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_arima_12 <- rbind(accuracy_arima_12, accuracy_result[5])
  }
}
i
sum(accuracy_arima)

# Комбинация

rus <- NA
accuracy_meta_12 <- NA

for (i in c(165:length(s_50$table))) {
  rus1 <- sophisthse(s_50$table[i])
  d <- dim(sophisthse(s_50$table[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- rus1[, j]
    l <- round(length(rus) * 0.9)
    h <- length(rus) - l
    train <- ts(rus[1:l])
    test <- ts(rus[(l + 1):(l + h)])
    forecast_result <- forecast_meta_M4(model_M4, train, h = h)
    accuracy_result <- accuracy(f = forecast_result$mean, test)
    accuracy_meta_12 <- rbind(accuracy_meta_12, accuracy_result[5])
  }
}
i
length(s_50$table)


accuracy_arima <- na.omit(accuracy_arima)


# Качество прогнозов годовых

accuracy_arima <- na.omit(accuracy_arima)
accuracy_arima <- accuracy_arima[is.finite(accuracy_arima)]
mean(accuracy_arima)

accuracy_ets <- na.omit(accuracy_ets)
accuracy_ets <- accuracy_ets[is.finite(accuracy_ets)]
mean(accuracy_ets)

accuracy_meta <- na.omit(accuracy_meta)
accuracy_meta <- accuracy_meta[is.finite(accuracy_meta)]
mean(accuracy_meta)

# Качество прогнозов квартальных

accuracy_arima_4 <- na.omit(accuracy_arima_4)
accuracy_arima_4 <- accuracy_arima_4[is.finite(accuracy_arima_4)]
mean(accuracy_arima_4)

accuracy_ets_4 <- na.omit(accuracy_ets_4)
accuracy_ets_4 <- accuracy_ets_4[is.finite(accuracy_ets_4)]
mean(accuracy_ets_4)

accuracy_meta_4 <- na.omit(accuracy_meta_4)
accuracy_meta_4 <- accuracy_meta_4[is.finite(accuracy_meta_4)]
mean(accuracy_meta_4)

# Качество прогнозов месячных рядов

accuracy_arima_12 <- na.omit(accuracy_arima_12)
accuracy_arima_12 <- accuracy_arima_12[is.finite(accuracy_arima_12)]
mean(accuracy_arima_12)

accuracy_ets_12 <- na.omit(accuracy_ets_12)
accuracy_ets_12 <- accuracy_ets_12[is.finite(accuracy_ets_12)]
mean(accuracy_ets_12)

accuracy_meta_12 <- na.omit(accuracy_meta_12)
accuracy_meta_12 <- accuracy_meta_12[is.finite(accuracy_meta_12)]
mean(accuracy_meta_12)


# Соединим полученные результаты в одной таблице

comparison_soph <- cbind(
  Type = c("Monthly series", "Quarterly series", "Yearly series"),
  ETS = c(mean(accuracy_ets_12), mean(accuracy_ets_4), mean(accuracy_ets)),
  ARIMA = c(mean(accuracy_arima_12), mean(accuracy_arima_4), mean(accuracy_arima)),
  META = c(mean(accuracy_meta_12), mean(accuracy_meta_4), mean(accuracy_meta))
)

# Экспорт таблицы в латех
xtable(comparison_soph, caption = "MAPE прогнозов sophisthse с помощью 3 основных методов", display = c("s", "f", "e", "E", "g"))

