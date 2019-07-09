library(dplyr)
library(tseries)
library(ggplot2)
library(forecast)
library(xgboost)
library(M4metalearning)
library(M4metaresults)
library(fpp2)
library(Mcomp)
library(xtable)
library(sophisthse)
library(devtools)
library(M4comp2018)

# Обучаем собственную модель

# Возьмем рядв с месячной периодичностью
series <- filter(series_info, freq == 12)

# Создадим список названий рядов
label_meta <- NA
for (i in c(1:length(series$table))) {
  label_meta <- rbind(label_meta, series$table[i])
}

series$table[5]
# Удалим дубликаты
label_meta <- unique(label_meta)[-1]

length(label_meta)
rus <- NA
data_soph <- list()
k <- 0
for (i in c(1:length(label_meta))) {
  rus1 <- sophisthse(label_meta[i])
  d <- dim(sophisthse(label_meta[i]))
  max <- d[2]
  for (j in c(1:max)) {
    rus <- na.remove(rus1[, j])
    if (length(rus) > 27) {
      k <- k + 1
      h <- as.integer(6)
      ts <- ts(data = rus, frequency = 12)
      data_soph[[k]] <- list(x = ts, h = h)
    }
  }
}

meta_M4 <- temp_holdout(data_soph)


# calculate the forecasts of each method in the pool

# THIS WILL TAKE A LOT OF TIME (hours...)
meta_M4 <- calc_forecasts(meta_M4, forec_methods(), n.cores = 2)

# calculate the OWA errors
meta_M4 <- calc_errors(meta_M4)
# extract the features
meta_M4 <- THA_features(meta_M4, n.cores = 2)

# search for hyperparameters
hyperparameter_search(meta_M4, filename = "M4_hyper.RData", n_iter = 50)

# get the best hyperparameter found
load("M4_hyper.RData")
best_hyper <- bay_results[ which.min(bay_results$combi_OWA), ]

# Train the metalearning model with the best hyperparameters found

train_data <- create_feat_classif_problem(meta_M4)
param <- list(
  max_depth = best_hyper$max_depth,
  eta = best_hyper$eta,
  nthread = 3,
  silent = 1,
  objective = error_softmax_obj,
  num_class = ncol(train_data$errors), # the number of forecast methods used
  subsample = bay_results$subsample,
  colsample_bytree = bay_results$colsample_bytree
)

meta_model <- train_selection_ensemble(train_data$data,
  train_data$errors,
  param = param
)

## Now the model is trained, lest produce the predictions

final_M4 <- M4comp2018::M4[1:20]

# just calculate the forecast and features
final_M4 <- calc_forecasts(final_M4, forec_methods())
final_M4 <- THA_features(final_M4)

# get the feature matrix
final_data <- create_feat_classif_problem(final_M4)
# calculate the predictions using our model
preds <- predict_selection_ensemble(meta_model, final_data$data)
# calculate the final mean forecasts
final_M4 <- ensemble_forecast(preds, final_M4)
# the combination predictions are in the field y_hat of each element in the list
# lets check one
final_M4[[1]]$y_hat


# визуализируем
autoplot(final_M4[[1]]$x) +
  autolayer(final_M4[[1]]$xx, series = "Original") +
  autolayer(ts(final_M4[[1]]$y_hat), series = "Forecast") +
  xlab("Год") + ylab("Объем продаж, млн$") +
  ggtitle("Прогноз объема продаж кортикостероидов в Австралии")

#  можно посмотреть важность признаков в новой модели

mat <- xgboost::xgb.importance(
  feature_names = colnames(train_data$data),
  model = meta_model
)
xgboost::xgb.plot.importance(importance_matrix = mat[1:20], cex = 1.0)
