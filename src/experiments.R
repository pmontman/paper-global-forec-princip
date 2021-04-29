
source("model.R")


##################################################
######## Nonlinear models ########################
##################################################


list_results = NULL
for (ds_name in c("M1", "M3", "tourism", "M4")) {
  
  
  my_series = readRDS(paste("data/", ds_name, ".rds", sep=""))
  periods = unique( sapply(my_series, function (ll) ll$period) )
  
  
  
  
  for (j in 1:length(periods)) {
    period = periods[j]
    period_series = Filter(function(ll) ll$period == period, my_series)
    lin_err_res = analyze_glob_model(period_series, linear_model, model_name = "linglob_")
    p2_err_res = analyze_glob_model(period_series, poly_2_model, model_name = "poly2glob_")
    p3_err_res = analyze_glob_model(period_series, poly_3_model, model_name = "poly3glob_")
    tree_err_res = analyze_glob_model(period_series, tree_model, model_name = "treeglob_")
    ensdeep_err_res = analyze_glob_model(period_series, ensemble_deep_model, model_name = "ens_deepglob_")
    list_results = append(list_results, list(list(lin_err_res=lin_err_res,
                                                  p2_err_res = p2_err_res,
                                                  p3_err_res = p3_err_res,
                                                  tree_err_res = tree_err_res,
                                                  ensdeep_err_res = ensdeep_err_res,
                                                  ds_name = ds_name, period = period)))
    save(list_results, file="total_experiment.RData")
  }

}

########################################################################
############ HETEROGENEOUS DATASET  ####################################
########################################################################



my_series = Filter(function(x)  length(x$x)>1 ,#& x$period =="Monthly",
                   M4comp2018::M4)
my_series = lapply(my_series, function(ll) {
  ll$ff = ll$pt_ff[c(20, 23, 18, 2, 1), ]
  row.names(ll$ff) <- c("arima", "ets", "theta", "fforma", "smyl")
  ll$pt_ff = NULL
  ll$up_ff = NULL
  ll$low_ff = NULL
  ll})
gc()


my_series <- lapply(my_series, function(ll) {
  ll$xx = ll$xx[1:1]
  ll$ff = ll$ff[, 1:1,drop=FALSE]
  ll$h = 1
  ll})


deep_model = function(X, y, lag, weights=NULL) {
  dnm = deepnet_ar_model(X, y, NULL, NULL, 1000, 512*64,
                         patience=50, weights = weights)
  dnm$lag = lag
  dnm
}


res_lin = analyze_glob_model(my_series, linear_model, lag_range=12)
res_dl = analyze_glob_model(my_series, deep_model, lag_range = 12, do_weights = FALSE)


#add tsfeatures as external features
feat_series = add_ext_feat_tsfeatures(my_series)
feat_res_dl = analyze_glob_model(feat_series, deep_model, lag_range = 12, do_weights = FALSE)


periods = unique(sapply(my_series, function (ll) ll$period))

period_res_lin = res_lin*0
period_res_dl = res_dl*0
for (j in 1:length(periods)) {
  period = periods[j]
  period_series = Filter(function(ll) ll$period == period, my_series)
  period_res_lin = period_res_lin +
    length(period_series)*analyze_glob_model(period_series, linear_model, lag_range = 12, model_name = "linglob_") / length(my_series)
  period_res_dl = period_res_dl + 
    length(period_series)*analyze_glob_model(period_series, deep_model, lag_range = 12, model_name = "deepglob_") / length(my_series)
  
}


long_feat_series = Filter(function(x) length(x$x) > 20, feat_series)
long_feat_res_dl = analyze_glob_model(long_feat_series, deep_model, lag_range = 20, do_weights = FALSE)

long_my_series = Filter(function(x) length(x$x) > 20, my_series)
long_res_dl = analyze_glob_model(long_my_series, deep_model, lag_range = 20, do_weights = FALSE)
long_res_lin = analyze_glob_model(long_my_series, linear_model, lag_range = 20, do_weights = FALSE)

extra_long_feat_series = Filter(function(x) length(x$x) > 70, feat_series)
extra_long_feat_res_dl = analyze_glob_model(extra_long_feat_series, deep_model, lag_range = 70, do_weights = FALSE)

extra_long_my_series = Filter(function(x) length(x$x) > 70, my_series)
extra_long_res_dl = analyze_glob_model(extra_long_my_series, deep_model, lag_range = 70, do_weights = FALSE)
extra_long_res_lin = analyze_glob_model(extra_long_my_series, linear_model, lag_range = 70, do_weights = FALSE)


 save(res_lin, res_dl, feat_res_dl, period_res_lin, period_res_dl, long_feat_res_dl,
      long_res_dl, long_res_lin, extra_long_feat_res_dl, extra_long_res_dl, extra_long_res_lin,
      file="heterog_res.RData")


##################################################################
########### in-sample vs out-of-sample ###########################
##################################################################


my_series = Filter(function(x)  length(x$x)>1,
                   M4comp2018::M4)
my_series = lapply(my_series, function(ll) {
  ll$ff = ll$pt_ff[c(20, 23, 18), ]
  row.names(ll$ff) <- c("arima", "ets", "theta")
  ll$pt_ff = NULL
  ll$up_ff = NULL
  ll$low_ff = NULL
  ll})
gc()


list_results = NULL
for (ds_name in c("M1", "M3", "tourism", "M4")) {
  
  
  load_series = readRDS(paste("data/", ds_name, ".rds", sep=""))
  periods = unique( sapply(load_series, function (ll) ll$period) )
  
  

  
  for (j in 1:length(periods)) {
    period = periods[j]
    my_series = Filter(function(ll) ll$period == period, load_series)
    
    
    
    my_series = lapply(my_series, function(ll) {
      ll$xx = ll$xx[1:1, drop=FALSE]
      ll$h = 1
      ll$ff = ll$ff[,1:1, drop=FALSE]
      ll
    })
    
    set.seed(12345)
    series_list = my_series[sample(length(my_series), min(2000, length(my_series)))]
    
    series_list = add_mase_scal(series_list)
    series_list <- mase_normalize(series_list)
    
    series_list <- Filter(function(x) (x$mase_scal > 0.0001),
                          series_list)
    if (length(series_list) < length(my_series)) {
      warning("analize_glob_lin: Some series have very small MASE scale! Removing them from calculations.")
    }
    
    
    global_lag = min(sapply(series_list, function (ll) length(ll$x))) -1
    
    
    
    h=1
    #deep network global
    W = do.call(rbind, lapply(series_list, function(ll) embed(ll$x, global_lag+h)))
    z = W[, 1, drop=FALSE]
    W = W[, -(1:h), drop=FALSE]
    
    lin_m = linear_model(W,z, global_lag)
    deep_m = deep_model(W, z, global_lag)
    #tree_m = tree_model(W, z, global_lag)
    
    
    lengths <- sapply(series_list, function(ll) length(ll$x))
    lengths <- lengths - global_lag
    weights = rep(1/(lengths), times=lengths)
    weights = length(weights) * weights / sum(weights)
    
    
    ins_loss_globlin = mean(weights*abs(z -  predict(lin_m, W)))
    ins_loss_deep = mean(weights*abs( z - predict(deep_m, W)))
    #ins_loss_tree = mean(weights*abs( z - predict(tree_m, W)))
    
    
    
    series_list = add_model_forecasts(lin_m, series_list, "lin")
    series_list = add_model_forecasts(deep_m, series_list, "deep")
    #series_list = add_model_forecasts(tree_m, series_list, "tree")
    local_lag = floor(global_lag/2)
    series_list = add_lindiv_forec(series_list, local_lag, 1)
    
    err_results=calc_error_summary(series_list)
    
    loss_results = NULL
    num_coef_results = NULL
    
    library(forecast)
    
    for (i in 1:length(series_list)) {
      
      model_ets = ets(series_list[[i]]$x)
      ins_loss_ets = mean(abs(model_ets$residuals))
      
      model_arima = auto.arima(series_list[[i]]$x)
      num_coefs_arima = length(model_arima$coef)
      num_coef_results = c(num_coef_results, num_coefs_arima)
      ins_loss_arima = mean(abs(model_arima$residuals))
      
      
      model_thetaf = forecast::thetaf(series_list[[i]]$x)
      ins_loss_thetaf = mean(abs(model_thetaf$residuals))
      
      #local linear AR
      X = embed(series_list[[i]]$x, local_lag + h)
      y = X[, 1, drop=FALSE]
      X = X[, -(1:h), drop=FALSE]
      model_loclin = lm(y~X-1)
      ins_loss_loclin = mean(abs(model_loclin$residuals))
      #print(ins_loss_loclin)
      if (is.na(ins_loss_loclin)) {ins_loss_loclin=0}
      
      
      loss_results = rbind(loss_results,
                           c(ins_loss_ets, ins_loss_thetaf, ins_loss_arima,
                             ins_loss_globlin, ins_loss_deep, ins_loss_loclin
                           ))
      print(i)
    }
    
    results = c(colMeans(loss_results) - as.numeric(err_results[3,c(2,3,1,4,5,6)]))
    
    list_results = append(list_results, list(list(insamp = colMeans(loss_results),
                                                  outsamp = as.numeric(err_results[3,c(2,3,1,4,5,6)]),
                                                  dataset = ds_name,
                                                  period=period)))
    
    save(list_results, 
         file="results/in_vs_out.RData")
    print(paste("Finishing Dataset:", ds_name, " period: ", period))
    
  }
}

##################################################################
#### Get the coefficients of linear models #######################
##################################################################


list_results = NULL
for (ds_name in c("M1", "M3", "tourism", "M4")) {
  
  
  my_series = readRDS(paste("data/", ds_name, ".rds", sep=""))
  periods = unique( sapply(my_series, function (ll) ll$period) )
  
  
  
  
  for (j in 1:length(periods)) {
    period = periods[j]
    period_series = Filter(function(ll) ll$period == period, my_series)
    max_lag = min(sapply(period_series, function (ll) length(ll$x))) -1
    lin_err_res = analyze_glob_model(period_series, linear_model, lag_range=max_lag, model_name = "linglob_")
    linc_coef = attr(lin_err_res, "last_model")$coef
    list_results = append(list_results, list(list(linc_coef=linc_coef,
                                                  ds_name = ds_name, period = period)))
  }
  print(ds_name)
}

save(list_results, file="results/coefs_lin.RData")



###############################################################
############### SCALE  NORMALIZATION ##########################
###############################################################


avg_deep_model = function(X, y, lag, weights=NULL) {
  ensemble_deep_model(X, y, lag, weights, num_models = 5)
}


list_results = NULL
set.seed(12345) 
for( ds_name in c("M1", "M3", "tourism", "M4", "hospital", "nn5", "pedestrian","electricity", "traffic", "parts", "wiki", "bom_weather")) {

  my_series <- readRDS(paste("data/", ds_name, ".rds", sep=""))
  
  my_series <- my_series[ sample(min(length(my_series),5000))]
  
  LAG = 12

  MAX_LEN = 500

  
  my_series = lapply(my_series, function(ll) {
    ll$x = tail(ll$x, MAX_LEN)
    ll$xx = ll$xx[1:1, drop=FALSE]
    ll$h = 1
    ll$ff = ll$ff[,1:1, drop=FALSE]
    ll
  })
  
  my_series = Filter(function (ll) length(ll$x) > LAG, my_series)

  res_dl_norm = analyze_glob_model(my_series, avg_deep_model, lag_range = LAG, MASE_norm = TRUE)
  res_dl_notnorm = analyze_glob_model(my_series, avg_deep_model, lag_range = LAG, MASE_norm = FALSE)

  my_series_scale = add_ext_feat_MASEscal(my_series)
  res_dl_scal_norm = analyze_glob_model(my_series_scale, avg_deep_model, lag_range = LAG, MASE_norm = TRUE)

  list_results = append(list_results, list(list(res_dl_norm=res_dl_norm,
                                                res_dl_notnorm = res_dl_notnorm,
                                                res_dl_scal_norm = res_dl_scal_norm,
                                                ds_name = ds_name)))
  print(ds_name)
  save(list_results, file="scalenorm_experiment.RData")
  
}


#####################################################################
#### LOng Memory Linear Models ######################################
#####################################################################


list_results = NULL
for (ds_name in c("M1", "M3", "tourism", "M4")) {
  
  load_series <- readRDS(paste("data/", ds_name, ".rds", sep=""))
  
  periods = unique( sapply(load_series, function (ll) ll$period) )
  

  
  set.seed(12345)
  for (j in 1:length(periods)) {
    period = periods[j]
    my_series = Filter(function(ll) ll$period == period, load_series)
    my_series = my_series[ sample(min(length(my_series), 2000)) ]
    
    
    max_len = min(sapply(my_series, function(ll) length(ll$x))) - 1
    
    res_lin = analyze_glob_model(my_series, linear_model, MASE_norm = TRUE, lag_range = 1:min(150,max_len))
    lin_is_better = (min(res_lin[1,1:3]) - res_lin[1, ncol(res_lin)]) > 0
    num_series = length(my_series)
    
    min_len = max_len +1
    while (!lin_is_better) {
      long_series = Filter(function (ll) length(ll$x) > min_len, my_series)
      res_lin = analyze_glob_model(long_series, linear_model, MASE_norm = TRUE, lag_range = 1:min_len)
      lin_is_better = (min(res_lin[1,1:3]) - min(res_lin[1, -(1:3)])) > 0
      num_series = length(long_series)
      print(paste(ds_name, period, "lag:", min_len,
                  " relative error:", min(res_lin[1, -(1:3)]) / min(res_lin[1,1:3]),
                  "nseries:", num_series))
      min_len = min_len + 1
    }
    
    list_results = append(list_results, list(list(lin_err_res=res_lin,
                                                  ds_name = ds_name, period = period,
                                                  min_len = min_len-1)))
    save(list_results, file="longmemlin_res.RData")
    
  }
}


########################################################################
############## PARTITIONING EXPERIMENTS ################################
########################################################################




set.seed(123456)




list_results = NULL
for (ds_name in c("M1", "M3", "tourism", "M4")) {
  
  load_series <- readRDS(paste("data/", ds_name, ".rds", sep=""))
  
  periods = unique( sapply(load_series, function (ll) ll$period) )
  
  for (j in 1:length(periods)) {
    period = periods[j]
    my_series = Filter(function(ll) ll$period == period, load_series)
    
    
    num_chunks = max( ceiling(length(my_series)/1000), 2)
    chunk_indices = floor(seq(1, length(my_series), length.out=num_chunks+1))
    
    sub_experi_res = NULL
    for (i in 1:(length(chunk_indices)-1)) {
      sub_series =  my_series[chunk_indices[i]:(chunk_indices[i+1])]
      sub_experi_res = append(sub_experi_res, list(analyze_glob_model(sub_series, linear_model)))
      gc()
    }
    
    tot_experi_res = analyze_glob_model(my_series, linear_model)
    
    red_experi_res = round(Reduce("+", lapply(sub_experi_res, function (ll) ll[,1:(ncol(tot_experi_res))])) / length(sub_experi_res),3)
    
    print(paste(ds_name, period, "chunks:", num_chunks))
    
    list_results = append(list_results, list(list(tot_experi_res=tot_experi_res,
                                                  red_experi_res = red_experi_res,
                                                  ds_name = ds_name, period = period,
                                                  num_chunks = num_chunks)))
    save(list_results, file="partitioning_res.RData")
  }
}





######################################################
######## WIKIPEDIA DATASET ###########################
######################################################

my_series = readRDS("data/wiki.rds")

nas = sapply(my_series, function(ll) anyNA(ll$x))

my_series = lapply(my_series, function(ll) {
  ll$x = tail(ll$x,366)
  ll
})
gc()

list_results = analyze_glob_model(my_series, linear_model, lag_range = 1:49)
save(list_results, file="results/wiki_full_last366.RData")



###################################################
############ PEDESTRIAN ###########################
###################################################



my_series = readRDS("data/pedestrian.rds")
list_results = analyze_glob_model(my_series, linear_model, lag_range = 1:66)
save(list_results, file="results/pedestrian_res.RData")


###################################################
############ WEATHER ##############################
###################################################


my_series = readRDS("data/bom_weather.rds")
length(my_series)
sapply(my_series, function(ll) length(ll$x))
my_series = lapply(my_series, function (ll) {
  ll$x = tail(ll$x, 733)
  ll
})
err_results = analyze_glob_model(my_series, linear_model, lag_range = 1:100)

#save(err_results, file="results/weather_733_5478.RData")


###################################################
############ TRAFFIC ##############################
###################################################


my_series = readRDS("data/")
length(my_series)
sapply(my_series, function(ll) length(ll$x))
my_series = lapply(my_series, function (ll) {
  ll$x = tail(ll$x, 600)
  ll
})
err_results = analyze_glob_model(my_series, linear_model, lag_range = 1:100)

save(err_results, file="results/results_traffic_25days.RData")


#################################################
########### covid ###############################
#################################################

my_series = readRDS("data/covid_ts.rds")
err_results = analyze_glob_model(my_series, linear_model,  model_name = "linglob_")
save(err_results, file="results/covid_res.RData")

#the rest of the experiments can be done in a similar fashion
