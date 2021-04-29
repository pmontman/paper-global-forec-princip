
source("embed.R")
source("nl_models.R")
library(RcppArmadillo)


##################################################################
################ FEATURIZATION CODE ##############################
##################################################################


#a featurizer ia s function that take a vector and returns a vector with features stracted from the input vector

add_ext_feat_tsfeatures <- function(series_list) {
  features = tsfeatures::tsfeatures(lapply(series_list, function (ll) ll$x), parallel=TRUE)
  features[is.na(features)] = 0
  features = scale(features)
  features[is.na(features)] = 0
  for (i in 1:length(series_list)) {
    series_list[[i]]$ext_feat <- c(series_list[[i]]$ext_feat, as.numeric(features[i, ]))
  }
  series_list
}

add_ext_feat_MASEscal <- function(series_list) {
  series_list = lapply(series_list, function (ll) {
    MASE_scal <- calc_mase_scal(ll$x)
    ll$ext_feat <- c(ll$ext_feat, MASE_scal)
    ll
  })
  scales = sapply(series_list, function (ll) ll$ext_feat)
  scales = scales / mean(scales)
  for (i in 1:length(series_list)) {
    series_list[[i]]$ext_feat = scales[i]
  }
  series_list
}


featurized_lin_model <- function(X, y, featurizer, lag, weights) {
  X_f = featurizer(X)
  model <- NULL
  if (is.null(weights)) {
    model = RcppArmadillo::fastLmPure(X_f, y)#lm(y~X_f -1, model = FALSE, qr = FALSE)
  } else {
    model = lm(y~X_f -1, weights = weights, model = FALSE, qr = FALSE)
  }
  model$lag = lag
  model$featurizer = featurizer
  structure(model, class = "featurized_lin_model")
}

predict.featurized_lin_model = function(object, newdata) {
  X_f = object$featurizer(newdata)
  X_f %*% object$coef
}


featur_poly = function(X, degree) {
  if (degree == 1) {
    return(X)
  }
  fX = X
  for (i in 2:degree) {
    fX = cbind(fX, X^i)
  }
  fX
}

featur_poly_fun = function(degree) {
  function(X) {
    featur_poly(X, degree)
  }
}



#####################################################################
##################### MODEL FUNCTIONS ###############################
### encapsulate model parameters etc ################################
### from a big X matrix and y vector to a model object as lm() ###### 
#####################################################################



deep_model = function(X, y, lag, weights=NULL) {
  dnm = deepnet_ar_model(X, y, NULL, NULL, 500, 1024, weights = weights)
  dnm$lag = lag
  dnm
}


ensemble_deep_model = function(X, y, lag, weights=NULL, num_models=3) {
  ens_models = NULL
  ens_models$deep_models = NULL
  for (i in 1:num_models) {
    ens_models$deep_models[[i]] = list(deep_model(X, y, lag, weights))
  }
  
  ens_models$lag = lag
  structure(ens_models, class = "ensemble_deep_model")
}

predict.ensemble_deep_model = function(object, newdata) {
  pred = predict(object$deep_models[[1]][[1]], newdata)
  if (length(object$deep_models) > 1) {
    for (i in 2:length(object$deep_models))
      pred = pred + predict(object$deep_models[[i]][[1]], newdata)
  }
  pred = pred / length(object$deep_models)
}

tree_model = function(X, y, lag, weights=NULL) {
  xgbm = xgb_ar_model(X, y, NULL, NULL, weights = weights)
  xgbm$lag = lag
  xgbm
}

linear_model = function(X, y, lag, weights=NULL) {
  featurized_lin_model(X, y, featur_poly_fun(1), lag, weights = weights)
}

poly_2_model = function (X, y, lag, weights=NULL) {
  featurized_lin_model(X, y, featur_poly_fun(2), lag, weights = weights)
}

poly_3_model = function (X, y, lag, weights=NULL) {
  featurized_lin_model(X, y, featur_poly_fun(3), lag, weights = weights)
}


########################################################################

add_model_forecasts = function(model, series_list, name) {
  stopifnot(!is.null(model$lag))
  
  lag = model$lag
  h = length(series_list[[1]]$xx)
  W = do.call(rbind, lapply(series_list, function(ll) {
    c( rev(tail(ll$x, model$lag)), ll$ext_feat)
  }))
  
  lag_feat_dim = ncol(W)
  
  pred_W = matrix(0, nrow(W), h)
  for (i in 1:h) {
    pred_W[,i] = predict(model, W[,1:lag_feat_dim, drop=FALSE])
    if (lag > 1) {
      W[, 2:lag] <- W[, 1:(lag-1), drop=FALSE]
    }
    W[, 1] <- pred_W[,i, drop=FALSE]
  }
  
  for (i in 1:nrow(W)) {
    series_list[[i]]$ff <- rbind(series_list[[i]]$ff, pred_W[i,1:h,drop=FALSE])
    rownames(series_list[[i]]$ff)[nrow(series_list[[i]]$ff)] <- name
  }
  series_list
}


##################################################################################


analyze_glob_model = function(my_series, fit_model_fun, model_name="custom_AR", MASE_norm=TRUE, lag_range=NULL, do_weights = FALSE) {
  message(paste("applying MASE normalization:", MASE_norm))
  message(paste("applying sample weighting:", do_weights))
  series_list = my_series
  
  
  series_list = add_mase_scal(series_list)
  if (MASE_norm) {
    series_list <- mase_normalize(series_list)
  }
  
  series_list <- Filter(function(x) (x$mase_scal > 0.0001),
                        series_list)
  if (length(series_list) < length(my_series)) {
    warning("analyze_glob_model: Some series have very small MASE scale! Removing them from calculations.")
  }
  
  #series_list <- norm2_normalize(series_list)
  #series_list <- centerscale_normalize_bias(series_list, bias=100)
  
  max_lag = min(sapply(series_list, function (ll) length(ll$x))) -1
  min_lag = 1
  if (!is.null(lag_range)) {
    message(paste("Setting Custom Lag Range"))
  } else {
    lag_range = min_lag:max_lag
  }
  
  
  
  for (lag in lag_range) {
    
    lengths <- sapply(series_list, function(ll) length(ll$x))
    lengths <- lengths - lag
    
    feat_dim = 0
    if (!is.null(series_list[[1]]$ext_feat)) {
      feat_dim = length(series_list[[1]]$ext_feat)
    }
    
    weights = NULL
    if (do_weights) {
      weights = rep(1/(lengths), times=lengths)
      weights = length(weights) * weights / sum(weights)
    }
    
    #!!!!maybe precalc the size of the matrix and use hard drive for very large matrices
    
    W = matrix(0, nrow=sum(lengths), ncol=lag + feat_dim)
    z = matrix(0, nrow=sum(lengths), ncol=1)
    
    row_count = 1
    for (i in 1:length(series_list)) {
      emb <- embed(series_list[[i]]$x, lag+1)
      W[row_count:(row_count + nrow(emb)-1), 1:lag] <- emb[,-1, drop=FALSE]
      z[row_count:(row_count + nrow(emb)-1) , ] <- emb[,1, drop=FALSE]
      if (feat_dim > 0) {
        W[row_count:(row_count + nrow(emb)-1), -(1:lag)] <- matrix( rep(series_list[[i]]$ext_feat, times=nrow(emb)),
                                                                    byrow = TRUE, nrow = nrow(emb))
      }
      row_count <- row_count + nrow(emb)
    }
    
    ar_model = fit_model_fun(W, z, lag, weights=weights)
    series_list = add_model_forecasts(ar_model, series_list, paste(model_name, lag, sep=""))
  }
  
  
  #error measurement
  #recalcs the scales
  if (MASE_norm) {
    series_list = mase_denormalize(series_list)
  }
  
  err_results = calc_error_summary(series_list)
  attr(err_results, "last_model") <- ar_model
  
  err_results
  
}


############ testing #################################
# 
# my_series <- Filter(function(x) (x$period)=="DAILY" && length(x$x)>1,
#                     readRDS("data/nn5.rds"))
# my_series <- lapply(my_series, function(ll) {
#   ll$xx = ll$xx[1:1]
#   ll$ff = ll$ff[, 1:1,drop=FALSE]
#   ll$h = 1
#   ll})
# 
# 
# mb = microbenchmark::microbenchmark(
# 
# analyze_glob_model(my_series, linear_model, lag_range=11:12),
# analyze_glob_model_cmp(my_series, linear_model, lag_range=11:12),times = 3)
# 
# err_res_w = analyze_glob_model(my_series, linear_model, lag_range=12, MASE_norm=TRUE, do_weights = TRUE)
# 
# 
# !!!!!!!!!!!!!!!!lapply2_c <- compiler::cmpfun(lapply2)
# 
# fs = add_ext_feat_MASEscal(my_series)
# 
# comp = replicate(10, {
# err_res_w = analyze_glob_model(my_series, deep_model, lag_range=5, MASE_norm=TRUE, do_weights = TRUE)
# f_err = analyze_glob_model(fs, deep_model, lag_range=5, MASE_norm = TRUE, do_weights = TRUE )
# (err_res_w - f_err)[1,4]})
# 

