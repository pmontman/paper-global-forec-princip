# #function for recursive forecasting by a model that uses the standard R predict() function
# forec_recursive_ar_model = function(model, x, lag, h) {
#   pred = t(tail(x, lag))
#   for (i in 1:h) {
#     cp = predict(model, t(rev(tail(as.vector(pred),lag))) )
#     pred = t(c(pred, cp))
#   }
#   tail(as.vector(pred), h)
# }
# 
# 
# #calculates the forecasts to the entry $ff to the list of time series, optional name
# slow_add_model_forecasts = function(model, series_list, name) {
#   stopifnot(!is.null(model$lag))
#   lapply(series_list, function(ll) {
#     ff = forec_recursive_ar_model(model, ll$x, model$lag, length(ll$xx))
#     ll$ff <- rbind(ll$ff, ff)
#     rownames(ll$ff)[nrow(ll$ff)] <- name
#     ll
#   })
# }


library(keras)

root_mse_loss = function(y_true, y_pred) {
  k_sqrt(k_mean(k_square(y_pred - y_true)))
}

root_mse_metric = custom_metric("root_mse_metric", function(y_true, y_pred) {
  root_mse_loss(y_true, y_pred)
})

abs_var_penal_loss = function(y_true, y_pred) {
  err = k_abs(y_pred - y_true)# k_abs((y_pred - y_true)/(1+y_true)) #
  mu_err = k_mean(err)
  sd_err = k_sqrt(k_mean(k_square(err - mu_err)))
  kurt_err = k_pow( k_mean(k_pow(err - mu_err, 4)) , 0.25)
  mu_err +0.01* sd_err #- 0.8*kurt_err##+ 1*k_max(err)# 
}


#Deep Network AR Model an 5-layer ReLu MLP
deepnet_ar_model <- function(X_train, y_train, X_val, y_val, epochs=350, batch=1, patience=ceiling(epochs/10),
                             weights=NULL) {
  
  X_train <- array_reshape(X_train, dim = c(nrow(X_train), ncol(X_train)) )
  y_train <- array(y_train, dim = c(nrow(y_train), ncol(y_train)) )
  



  
  ind_cv = sample(nrow(X_train), floor(nrow(X_train)*0.15))
  X_val = X_train[ind_cv, ,drop=FALSE]
  y_val = y_train[ind_cv]
  X_train = X_train[-ind_cv, ,drop=FALSE]
  y_train = y_train[-ind_cv]
  if (!is.null(weights)) {
    weights = weights[-ind_cv]
  }
  
  # block crossvalidation, leave out some consecutive oversation
  # ind_cv = sample(nrow(X_train), floor(nrow(X_train)*0.15/3))
  # ind_cv = unique(pmin(nrow(X_train),c(ind_cv, ind_cv +1, ind_cv+2)))
  # X_val = X_train[ind_cv,]
  # y_val = y_train[ind_cv]
  # X_train = X_train[-ind_cv,]
  # y_train = y_train[-ind_cv]

  
  sampind = sample(nrow(X_train))
  X_train = X_train[sampind, ,drop=FALSE]
  y_train = y_train[sampind]
  

  #decay_rate = 0.5 / nrow(X_train)
  decay_rate=0
  
  deep_model <- keras_model_sequential() %>%
    layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(X_train))) %>%
    layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(X_train))) %>%
    layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(X_train))) %>%
    layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(X_train))) %>%
    layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(X_train))) %>%
    # layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(W))) %>%
    # layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(W))) %>%
    # layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(W))) %>%
    # layer_dropout(0.15) %>%
    #    layer_batch_normalization() %>%
    layer_dense(units = 1, activation = 'linear') %>%
    compile(
      loss =  'mean_absolute_error', #abs_var_penal_loss, #
      optimizer =  optimizer_adam(decay = decay_rate), #optimizer_adam(lr = 0.001, decay = decay_rate), #optimizer_sgd(lr = 0.00001*5, momentum = 0.0, nesterov = FALSE, decay = 0.05/nrow(X_train)),
      metrics = c('mean_absolute_error')
    )
  
  deep_model %>% fit(X_train, y_train, batch_size = batch, epochs = epochs,
                     #validation_split = 0.15,
                     validation_data = list(X_val, y_val),
                     view_metrics=FALSE,
                     sample_weight=weights,
                     callbacks = list(callback_early_stopping(monitor = "val_mean_absolute_error",
                                                              patience = patience,
                                                              restore_best_weights = TRUE)))
  deep_model$lag = ncol(X_train)
  deep_model
}


library(xgboost)
xgb_ar_model <- function(X_train, y_train, X_val, y_val, weights=NULL) {
  
  
  X_train <- array_reshape(X_train, dim = c(nrow(X_train), ncol(X_train)) )
  y_train <- array(y_train, dim = c(nrow(y_train), ncol(y_train)) )


  ind_cv = sample(nrow(X_train), floor(nrow(X_train)*0.15))
  X_val = X_train[ind_cv, ,drop=FALSE]
  y_val = y_train[ind_cv]
  X_train = X_train[-ind_cv, ,drop=FALSE]
  y_train = y_train[-ind_cv]


  sampind = sample(nrow(X_train))
  X_train = X_train[sampind, ,drop=FALSE]
  y_train = y_train[sampind]

  dtrain <- xgb.DMatrix(X_train, label = y_train)
  dtest <- xgb.DMatrix(X_val, label = y_val)
  watchlist <- list(train=dtrain, eval = dtest)
  
  xgb_model = xgb.train(params=list(booster="gbtree", objective="reg:linear",
                                    eta = 0.03,
                                    max_depth=16,
                                    colsample_bytree=0.9,
                                    subsample=0.9,
                                    eval_metric="mae"),
                        dtrain,
                        nrounds=1000,
                        watchlist=watchlist,
                        early_stopping_rounds = 20)
  
  xgb_model$lag = ncol(X_train)
  xgb_model
  
}



#create a validation set for global models by embedding the true future data into matrices
#NOTE: this is a CHEATING validation set!
create_val_global_set <- function(series_list) {
  x_val = do.call(rbind, lapply(series_list, function(ll) rev(tail(ll$x, lag))  ))
  y_val = do.call(rbind, lapply(series_list, function(ll) ll$xx[1]))
  # 
  # 
  # for (hh in 2) {
  #   x_val_h = do.call(rbind, lapply(series_list,
  #                                   function(ll) rev(tail(c(ll$x, ll$xx[1:(hh-1)]), lag))  ))
  #   y_val_h = do.call(rbind, lapply(series_list, function(ll) ll$xx[hh]))
  #   x_val = rbind(x_val, x_val_h)
  #   y_val = rbind(y_val, y_val_h)
  # }
  # 
  list(X_val = x_val, y_val = y_val)
}
