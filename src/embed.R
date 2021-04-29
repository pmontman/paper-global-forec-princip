
######### NOTE!!: We should normalized by series length! It is common sense and only
######### fails improve in scenarios such as M4 monthly with MASE, because the scale is heavily correlated with the series length
######### the longer the series, the more regular, less mase scale, this can make mase calculation explode when the o.o.s changes.


#return the global linear model coefficients for a dataset, lag order and horizon
deprecated_global_linear_model = function(series_list, lag, h) {
  W = do.call(rbind, lapply(series_list, function(ll) embed(ll$x, lag+h)))
  z = W[, 1, drop=FALSE]
  W = W[, -(1:h), drop=FALSE]
  model = RcppArmadillo::fastLmPure(W, z)$coef #lm(z~W-1, model = FALSE, qr = FALSE)$coef
}

#recursive forecasting of a series until a given horizon
deprecated_forec_recursive = function(beta, x, h) {
  lag = length(beta)
  pred = tail(x, lag)
  for (i in 1:h) {
    cp = rev(t(tail(pred,lag))) %*% beta
    pred = c(pred, cp)
  }
  tail(pred, h)
}

#calculates the forecasts to the entry $ff to the list of time series, optional name
deprecated_add_forec_recursive = function(beta, series_list, name=paste("linglob_",length(beta),sep="")) {
  lapply(series_list, function(ll) {
    ff = forec_recursive(beta, ll$x, length(ll$xx))
    ll$ff <- rbind(ll$ff, ff)
    rownames(ll$ff)[nrow(ll$ff)] <- name
    ll
  })
}



embed_equations = function(x, lag, h) {
  X = embed(x, lag + h)
  y = X[, 1, drop=FALSE]
  X = X[, -(1:h), drop=FALSE]
  list(X=X, y=y)
}

add_lindiv_forec = function(series_list, lag, h, name=paste("lindv_", lag, sep="")) {
  equa = lapply(series_list, function(ll) embed_equations(ll$x, lag, h))
  
  for (i in 1:length(series_list)) {
    ll = equa[[i]]
    beta = lm(ll$y~ll$X-1)$coef
    ll = series_list[[i]]
    ff = deprecated_forec_recursive(beta, ll$x, length(ll$xx))
    ll$ff <- rbind(ll$ff, ff)
    rownames(ll$ff)[nrow(ll$ff)] <- name
    ll
    series_list[[i]] = ll
  }
  series_list
}

#utility for performing total least squares!
total_ls = function(W, z) {
  SVT = svd(cbind(z,W))
  MPinv(t(W) %*% W - diag(ncol(W))*(SVT$d[ncol(W)+1]**2))%*%t(W) %*% z
}

###############################################
###########  PREPROCESSING ####################
###############################################

#mase normalization, requires mase scale already computed with add_mase_scal
mase_normalize = function(series_list) {
  lapply(series_list, function(ll) {
    ll$x = ll$x / ll$mase_scal
    ll$xx = ll$xx / ll$mase_scal
    ll$ff = ll$ff / ll$mase_scal
    ll
  })
}

mase_denormalize = function(series_list) {
  lapply(series_list, function(ll) {
    ll$x = ll$x * ll$mase_scal
    ll$xx = ll$xx * ll$mase_scal
    ll$ff = ll$ff * ll$mase_scal
    ll
  })
}

#centers, scales, and optinally adds a constant bias to the series, can be usefull for the global methods
centerscale_normalize_bias = function(series_list, bias) {
  lapply(series_list, function(ll) {
    mu = mean(ll$x)
    std = sd(ll$x)
    ll$x = (ll$x - mu + bias) / std
    ll$xx = (ll$xx - mu + bias) / std
    ll$ff = (ll$ff - mu + bias) / std
    ll
  })
}

mean_normalize = function(series_list) {
  lapply(series_list, function(ll) {
    mu = sqrt(mean(ll$x))
    ll$x = ll$x  / mu
    ll$xx = ll$xx / mu
    ll$ff = ll$ff / mu
    ll
  })
}

sd_normalize = function(series_list) {
  lapply(series_list, function(ll) {
    stdev = sd(ll$x)
    ll$x = ll$x  / stdev
    ll$xx = ll$xx / stdev
    ll$ff = ll$ff / stdev
    ll
  })
}


norm2_normalize = function(series_list) {
  lapply(series_list, function(ll) {
    scal = 1/length(ll$x) * sqrt(sum(ll$x**2))
    ll$x = ll$x  / scal
    ll$xx = ll$xx / scal
    ll$ff = ll$ff / scal
    ll
  })
}
###############################################
#####  ERROR MEASURES   #######################
###############################################


calc_mase_scal = function(x) {
  frq = floor(stats::frequency(x))
  if (length(x) < frq) {
    warning("MASE calc: Series shorter than its period, period will be set to 1 for MASE calculations")
    frq = 1
  }
  mean(abs(utils::head(as.vector(x), -frq) - utils::tail(as.vector(x), 
                                                                    -frq))**1)
}

#add the MASE scaling constant to each series in the list
add_mase_scal = function(series_list) {
  lapply(series_list, function(ll) {
    scal <- calc_mase_scal(ll$x)
    ll$mase_scal = scal
    ll
  })
}

#generic for MASE but any norm, 1 is MASE, 2 is MSSE, ...
error_mnormse = function(series_entry, norm) {
  ff = series_entry$ff
  XX = matrix(replicate(nrow(ff), series_entry$xx), nrow=nrow(ff), byrow=TRUE)
  err = rowMeans(abs(ff - as.numeric(XX))**norm / series_entry$mase_scal**norm)
  err = matrix(err, nrow=1, byrow = TRUE)
  series_entry$error = rbind(series_entry$error, err)
  rownames(series_entry$error)[nrow(series_entry$error)] <- paste("mnse_",norm,sep="")
  colnames(series_entry$error) <- rownames(ff)
  series_entry
}


#measure error
add_error_mnormse = function(series_list, norm) {
  lapply(series_list, error_mnormse, norm)
}

add_error_mase = function(series_list) {
  add_error_mnormse(series_list, 1)
}


#equivalent in squared to mase
add_error_msse = function(series_list) {
  add_error_mnormse(series_list, 2)
}

add_error_centerscale = function(series_list, norm, just_norm=FALSE) {
  lapply(series_list, function (ll) {
    mu = ifelse(just_norm, 0, mean(ll$x))
    std = ifelse(just_norm, 1, sd(ll$x))
    ff = ll$ff
    XX = matrix(replicate(nrow(ff), ll$xx), nrow=nrow(ff), byrow=TRUE)
    err = rowMeans(abs((ff -  mu)/std - (XX - mu)/std)^norm)
    err = matrix(err, nrow=1, byrow = TRUE)
    ll$error = rbind(ll$error, err)
    rownames(ll$error)[nrow(ll$error)] <- paste(paste("errcscl_",norm))
    colnames(ll$error) <- rownames(ff)
    ll
  })
}



add_error_smape = function(series_list) {
  lapply(series_list, function (ll) {
    ff = ll$ff
    XX = matrix(replicate(nrow(ff), ll$xx), nrow=nrow(ff), byrow=TRUE)
    denom = 0.5*(abs(ff) + abs(XX))
    err = abs(ff -  XX)/ denom
    err[is.infinite(err)] <- 0
    err[abs(denom) < 0.00000001] <- 0
    err = 100*rowMeans(err)
    err = matrix(err, nrow=1, byrow = TRUE)
    ll$error = rbind(ll$error, err)
    rownames(ll$error)[nrow(ll$error)] <- "smape"
    colnames(ll$error) <- rownames(ff)
    ll
  })
}

add_error_norm = function(series_list, norm) {
  lapply(series_list, function (ll) {
    ff = ll$ff
    XX = matrix(replicate(nrow(ff), ll$xx), nrow=nrow(ff), byrow=TRUE)
    err = rowMeans(( abs(ff -  XX)^norm ))
    ll$error = rbind(ll$error, err)
    rownames(ll$error)[nrow(ll$error)] <- paste("norm_",norm, sep="")
    colnames(ll$error) <- rownames(ff)
    ll
  })
}

add_error_relative_msfe = function(series_list) {
  lapply(series_list, function (ll) {
    mu_ff = sapply(head(ll$xx,-1), function(fx) {
      mean(c(ll$x, fx))
    })
    mu_ff = c(mean(ll$x), mu_ff)
    err_scale = mean((ll$xx - mu_ff)^2)
    
    ff = ll$ff
    XX = matrix(replicate(nrow(ff), ll$xx), nrow=nrow(ff), byrow=TRUE)
    err = rowMeans(( abs(ff -  XX)^2 ))/err_scale
    ll$error = rbind(ll$error, err)
    rownames(ll$error)[nrow(ll$error)] <- paste("relat_msfe")
    colnames(ll$error) <- rownames(ff)
    ll
  })
}

add_error_unitnorm = function(series_list) {
  lapply(series_list, function (ll) {
    scaling = sqrt(mean(ll$x**2))
    norm=2
    ff = ll$ff
    XX = t(replicate(nrow(ff), ll$xx))
    err = rowMeans(abs(ff - as.numeric(XX))**norm / scaling)
    err = matrix(err, nrow=1, byrow = TRUE)
    ll$error = rbind(ll$error, err)
    rownames(ll$error)[nrow(ll$error)] <- paste("unitnorm_",norm,sep="")
    colnames(ll$error) <- rownames(ff)
    ll
  })
}



calc_error_summary <- function(series_list) {
  if (is.null(series_list[[1]]$mase_scal)) {
    series_list = add_mase_scal(series_list)
  } 

  series_list = add_error_mase(series_list)
  series_list = add_error_smape(series_list)
  series_list = add_error_centerscale(series_list, norm=1, just_norm = TRUE)


  err_results = do.call(rbind, lapply(series_list, function(ll) ll$error))
  err_names = rownames(err_results[1:3,])
  
  #mean for each of the 3 error measures, they alterante so we agregate by 1:3 repeated
  err_results = aggregate(err_results, by = list(rep(1:3, times=length(series_list))), mean)
  err_results = err_results[,-1] #removing the new "Group" column created by aggregate
  err_results = round(err_results,4)
  rownames(err_results) = err_names
  err_results
}

#everything is adding equations to the model, unifying model combination, regularization, probabilistic...
