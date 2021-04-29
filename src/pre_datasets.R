#process the datasets to get the forecasts of other methods in a coherent forma
#so we can do advance analysis of results


library(fforma)
library(future.apply)

auto_arima_default = function (x, h) 
{
  model <- forecast::auto.arima(x)
  forecast::forecast(model, h = h)$mean
}



fmethod_list = list( "auto_arima_default", "ets_forec", "thetaf_forec")


data.series = Tcomp::tourism

future::plan(future::multiprocess, workers=4)

#process only the forecasts of auto.arima, ets and theta
data.series = future_lapply(data.series, function(ll) {
  auto_arima_default(1,1)
  calc_forecasts(ll, fmethod_list)
})
tourism = data.series
saveRDS(tourism, file="tourism.rds")


data.series = Mcomp::M1

#process only the forecasts of auto.arima, ets and theta
data.series = future_lapply(data.series, function(ll) {
  auto_arima_default(1,1)
  fforma::calc_forecasts(ll, fmethod_list)
})

M1 = data.series
saveRDS(M1, file="M1.rds")

data.series = Mcomp::M3

#process only the forecasts of auto.arima, ets and theta
data.series = future_lapply(data.series, function(ll) {
  auto_arima_default(1,1)
  fforma::calc_forecasts(ll, fmethod_list)
})
M3 = data.series
saveRDS(M3, file="M3.rds")


#also data is tsdl package
#wikipedia cliks
#Appliances-energy-prediction-data
#uci repository
#traffic and the amazon paper dataset


###### WIKIPEDIA TRAFFIC COMPETITION DATA #############

#solution is on the file solution_11_15.csv.zip (85.63 MB)
#and must be merged with the train data by looking a t the keys

wiki = read.csv("data/train_2.csv")

key = read.csv("data/key_2.csv")

names_wiki = wiki[,1]
names_key = as.character(unique(key[,1]))
strsplit(names_key[1000], '_')

clean_key = unique(sapply(names_key, function(mystr) {
  spl = unlist(strsplit(mystr, '_'))
  paste(spl[-length(spl)], collapse="_")
}
)
)

mn = match(clean_key, names_wiki)


#the solution file is also in the kaggle, a post in the discussion
solu = read.csv("data/research_topic_datasets/web-traffic-time-series-forecasting/solution_11_15.csv")


#62 is the length of the test period
solumat = matrix(solu[,2], ncol=62, byrow = T)
wiki = cbind(wiki[mn,], solumat)
colnames(wiki) <- NULL
rownames(wiki) <- NULL



wiki_list = lapply(1:nrow(wiki), function (i) {
  list(x = head(as.numeric(wiki[i,-1]), -62),
       xx = tail(as.numeric(wiki[i,-1]), 62),
       h = 62,
       st = as.character(wiki[i,1]))
})

saveRDS(wiki_list, file="data/wiki.rds")

#addind submissions to the competition
submi = read.csv("data/keras_kf_12_stage2_sept_10_test.csv")
submi = matrix(submi[,2], ncol=62, byrow = T)

wiki_list = readRDS("data/wiki.rds")
for (i in 1:length(wiki_list)) {
  wiki_list[[i]]$ff = rbind(wiki_list[[i]]$ff, submi[i,])
  rownames(wiki_list[[i]]$ff)[nrow( wiki_list[[i]]$ff)] <- "CPMP"
}


saveRDS(wiki_list, file="data/wiki.rds")

################################
###benchmark local methods #####
################################

wiki = readRDS("data/wiki.rds")
wiki = Filter(function(x) !(anyNA(x$x) | anyNA(x$xx)), wiki)



wiki = lapply(wiki, function (ll) {
  ll$x =  ts(ll$x, frequency =7)
  ll$xx =  ts(ll$xx, frequency =7)
  ll
})

library(forecast)
series_list = wiki
for (i in 1:length(series_list)) {
  #arima_ff = forecast( auto.arima(series_list[[i]]$x),
  #                     h=series_list[[i]]$h)$mean
  ets_ff = forecast(ets(series_list[[i]]$x),
                    h=series_list[[i]]$h)$mean
  theta_ff = thetaf(series_list[[i]]$x, h=series_list[[i]]$h)$mean
  series_list[[i]]$ff = rbind(series_list[[i]]$ff,  ets_ff, theta_ff)
  print(i)
}
wiki = series_list
saveRDS(wiki, file="data/wiki.rds")


####### NOTE: COMPARE AGAINST PROPHET PACKAGE #######


install.packages("prophet")
df <- read.csv('https://github.com/facebook/prophet/blob/master/examples/example_wp_log_peyton_manning.csv')
m <- prophet(df)

library(lubridate)
seq(ymd('2012-04-07'),ymd('2013-03-22'), by = '1 week')
####### NOTE: Also, the wikipediatrend R package for more data ######



##############################################################
#########   FRED-MD DATA #####################################
##############################################################

fred = read.csv("data/fred_MD_Monthly_Sept_2019.csv")

cnames = colnames(fred)
fred = fred[-1,]


fred = t(fred)

keep_series = which( !apply(fred, 1, anyNA) )

fred = fred[keep_series,]
cnames = cnames[keep_series]


fred_h = 12

#WE START AT 2 TO REMOVE THE COLUMN SPECIFYING THE DATE
FRED_MONTH = lapply(2:nrow(fred), function(i) {
  x = fred[i,]
  ele = list(x = ts(as.numeric(head(x, -fred_h)), frequency = 12, start=c(1959,1)),
       xx = ts(as.numeric(tail(x, fred_h), frequency = 12)),
       #ff = t(rep( mean(as.numeric(head(x, -fred_h))), fred_h)),
       period = "MONTHLY",
       h = fred_h,
       sn = cnames[i])
  ele$xx <- ts(ele$xx, frequency = 12, start=end(ele$x) + c(0,1))
  
  #rownames(ele$ff) <- "mean_past"
  ele
})
saveRDS(FRED_MONTH, file="data/FRED_MONTH.rds")

FRED_MONTH = readRDS("data/FRED_MONTH.rds")

#install.packages("forecast")
library(forecast)
for (i in 1:length(FRED_MONTH)) {
  arima_ff = forecast( auto.arima(FRED_MONTH[[i]]$x),
            h=FRED_MONTH[[i]]$h)$mean
  ets_ff = forecast(ets(FRED_MONTH[[i]]$x),
                     h=FRED_MONTH[[i]]$h)$mean
  theta_ff = forecast(thetaf(FRED_MONTH[[i]]$x),
                      h=FRED_MONTH[[i]]$h)$mean
  FRED_MONTH[[i]]$ff = rbind(FRED_MONTH[[i]]$ff, arima_ff, ets_ff, theta_ff)
  print(i)
}
saveRDS(FRED_MONTH, file="data/FRED_MONTH.rds")

#################### Oxford Realized Volatility ##############################

a = read.csv("data/oxfordmanrealizedvolatilityindices.csv")
names(a)

plot(a[ a$Symbol==".AEX",]$medrv,type="l")

HARModel Package


################## DOMINICK SALES DATASET ########################
#library(readr)

#a = read.table(unz("data/dominick/wbat.zip"), header=T, quote="\"", sep=",")


files = list.files("data/dominick/", full.names = TRUE)

dominick = NULL
for (fname in files) {
  
  unz_fname = unzip(fname) 
  dmnk_csv=  read.table(unz_fname, header=T, quote="\"", sep=",")
  file.remove(unz_fname)
  #a = read.table(unz("data/dominick/wbat.zip", "wbat.csv"), header=T, quote="\"", sep=",")
  
  gr = aggregate(list(dmnk_csv$PROFIT,dmnk_csv$WEEK),
                 by=list(dmnk_csv$UPC, dmnk_csv$STORE),
                 function(x) x)
  
  dominick_h = 9
  
  dom_tmp = lapply(1:length(gr[[1]]), function(i) {
    x = gr[[3]][[i]]
    
    if (length(x) < 3*dominick_h) return(NULL)
    
    week = gr[[4]][[i]]
    ele = list(x = ts(as.numeric(head(x, -dominick_h)), frequency = 52, start=week[1]),
               xx = ts(as.numeric(tail(x, dominick_h), frequency = 52)),
               #ff = t(rep( mean(as.numeric(head(x, -fred_h))), fred_h)),
               sn = paste(gr[[1]][[i]],"_",gr[[2]][[i]], sep=""),
               period = "WEEKLY",
               h = dominick_h)
    ele$xx <- ts(ele$xx, frequency = frequency(ele$xx), start=end(ele$x) + 1)
    ele
  })
  dom_tmp = Filter(Negate(is.null), dom_tmp)
  dominick = append(dominick, dom_tmp)
  saveRDS(dominick, file="data/dominick.rds")
  gc()
  print(fname)
}


library(forecast)
dominick = readRDS(file="data/dominick.rds")
for (i in 1:length(dominick)) {
  cap_ts = dominick[[i]]$x
  stlm_ff =  tryCatch({stlf(cap_ts,
                            h=dominick[[i]]$h)$mean}, error= function(e) snaive(ts(cap_ts, frequency = 365),
                                                                                   h=dominick[[i]]$h)$mean)
  theta_ff = tryCatch({forecast(thetaf(cap_ts),
                                h=dominick[[i]]$h)$mean}, error= function(e) snaive(ts(cap_ts, frequency = 365),
                                                                                       h=dominick[[i]]$h)$mean)
  dominick[[i]]$ff = rbind(dominick[[i]]$ff,  stlm_ff, theta_ff)
  if (i%%100 == 0) print(i)
}
saveRDS(dominick, file="data/dominick.rds")

#remove constant time series
dominick = readRDS("data/dominick.rds")
length(dominick)
invalid=sapply(dominick, function(ll) calc_mase_scal(ll$x))
dominick = dominick[which(!is.na(invalid))]
length(dominick)
saveRDS(dominick, file="data/dominick.rds")

#subset
set.seed(12345)
saveRDS(dominick[sample(length(dominick), 100000)], file="data/dominick_100K.rds")

################# FOREDeck ###################

http://fsudataset.com/


##########################################################################
### TLC Taxi trip record package, we can try to aggregate by time scale
### https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page


#csiro wind speed################
#https://data.csiro.au/dap/landingpage?pid=csiro%3AWind_Speed


####### FRED R package ##########

fredr v1.0.0:

  
#### data   for weather ##########
  https://openweathermap.org/current
https://cran.r-project.org/web/packages/ecmwfr/index.html
rnoaa


#install_github("ropensci/bomrang", build_vignettes = F)

library(bomrang)
#dat = get_historical(stationid = "023000", type = "max") 

stat = sweep_for_stations(latlon = c(-35.3, 149.2))

data_stations = NULL

#this will download from the internet!
for (statid in stat$site) {
  data_entry = NULL
  data_entry$rain = tryCatch(get_historical(stationid = statid, latlon = NULL, type = "rain"), error = function (e) NULL)
  data_entry$mintemp = tryCatch(get_historical(stationid = statid, latlon = NULL, type = "min"), error = function (e) NULL)
  data_entry$maxtemp = tryCatch(get_historical(stationid = statid, latlon = NULL, type = "max"), error = function (e) NULL)
  data_entry$solar = tryCatch(get_historical(stationid = statid, latlon = NULL, type = "solar"), error = function (e) NULL)
  
  
  data_stations = append(data_stations, list(data_entry))
  Sys.sleep(10)
  saveRDS(data_stations, file="weather.rds")
}
## we can also try to get the current weather for more granularity!

weather = readRDS("weather.rds")

weather_h = 365


types = c("rain", "mintemp", "maxtemp", "solar")

bom_weather = NULL
for (type in types) {
  type_weather = lapply(1:length(weather), function(i) {
    series = weather[[i]][[type]][,6]
    if (!is.null(series) && length(series)>365) {
      mu = mean(series, na.rm = TRUE)
      series[is.na(series)] = mu
      ll = list(x = ts(head(series, -weather_h), frequency = 365.25),
                xx = tail(series, weather_h),
                period = "DAILY",
                h = weather_h,
                type=type,
                station=weather[[i]]$rain$station_number[1])
    }
  })
  bom_weather = append(bom_weather, type_weather)
}
bom_weather = Filter(Negate(is.null), bom_weather)
bom_weather = Filter(function(ll) length(ll$x)>731, bom_weather) #series over 2 years of obsevations
saveRDS(bom_weather, file="data/bom_weather.rds")

rm(list=ls())
gc()

bom_weather = readRDS("data/bom_weather.rds")
library(forecast)
series_list=bom_weather

for (i in 1:length(series_list)) {
  
  cap_ts = series_list[[i]]$x #ts(tail(series_list[[i]]$x, 365.25*6 + 1), frequency = 365.25)

  series_list[[i]]$ff = NULL
  tbats_ff = forecast( tbats(cap_ts),
                       h=series_list[[i]]$h)$mean
 
  stlm_ff =  tryCatch({stlf(cap_ts,
                    h=series_list[[i]]$h)$mean}, error= function(e) snaive(ts(cap_ts, frequency = 365),
                                                                                      h=series_list[[i]]$h)$mean)
  theta_ff = tryCatch({forecast(thetaf(cap_ts),
                      h=series_list[[i]]$h)$mean}, error= function(e) snaive(ts(cap_ts, frequency = 365),
                                                                                  h=series_list[[i]]$h)$mean)
  
  series_list[[i]]$ff = rbind(series_list[[i]]$ff, tbats_ff, stlm_ff, theta_ff)
  print(i)
}
bom_weather = series_list

saveRDS(bom_weather, file="data/bom_weather.rds")


###############################################################
################## TRAFFIC DATASET FROM DeepAR ################
###############################################################

#download the raw data using the script
#in https://github.com/rofuyu/exp-trmf-nips16

##!/bin/bash

# function gdrive-get() {
#   fileid=$1
#   filename=$2
#   if [[ "${fileid}" == "" || "${filename}" == "" ]]; then
#   echo "gdrive-curl gdrive-url|gdrive-fileid filename"
#   return 1
#   else
#     if [[ ${fileid} = http* ]]; then
#   fileid=$(echo ${fileid} | sed "s/http.*drive.google.com.*id=\([^&]*\).*/\1/")
#   fi
#   echo "Download ${filename} from google drive with id ${fileid}..."
#   cookie="/tmp/cookies.txt"
#   curl -c ${cookie} -s -L "https://drive.google.com/uc?export=download&id=${fileid}" > /dev/null
#   confirmid=$(awk '/download/ {print $NF}' ${cookie})
#   curl -Lb ${cookie} "https://drive.google.com/uc?export=download&confirm=${confirmid}&id=${fileid}" -o ${filename}
#   rm -rf ${cookie}
#   return 0
#   fi
# }
# 
# gdrive-get 1uJDqW4u2shjbA_Y27oIcq-QM2jgW4c9g electricity.npy
# gdrive-get 1R_M3RP-t5CKDj5gUVTU2IEZXDInQKHJG traffic.npy

#install.packages("RcppCNPy")
library(RcppCNPy)
traf = npyLoad("data/traffic.npy")
traf_h = 24 *7 #from the DeepState paper

traffic = lapply(1:ncol(traf), function(i) {
  x = traf[,i]
  ele = list(x = ts(as.numeric(head(x, -traf_h)), frequency = 24*7),
             xx = ts(as.numeric(tail(x, traf_h), frequency = 24*7)),
             period = "HOURLY",
             h = traf_h)
  #rownames(ele$ff) <- "mean_past"
  ele
})

saveRDS(traffic, file="data/traffic.rds")

traffic = readRDS("data/traffic.rds")

#install.packages("forecast")
library(forecast)
for (i in 1:length(traffic)) {
  traffic[[i]]$ff = NULL
  #tbats_4week_ff = forecast( tbats(tail(traffic[[i]]$x, 24*7*4)),
  #                     h=traffic[[i]]$h)$mean
  stlm_ff = forecast(stlf(traffic[[i]]$x),
                    h=traffic[[i]]$h)$mean
  theta_ff = forecast(thetaf(traffic[[i]]$x),
                      h=traffic[[i]]$h)$mean
  traffic[[i]]$ff = rbind(traffic[[i]]$ff,  stlm_ff, theta_ff) #tbats_4week_ff,
  print(i)
}
saveRDS(traffic, file="data/traffic.rds")





############### NN5 ###########################


library(tscompdata)
library(imputeTS)

nn5_h = 56

nn5_imp = lapply(1:length(nn5), function (i) {
  x = imputeTS::na_seadec(tscompdata::nn5[[i]])
  ele = list(x = head(x, -nn5_h),
             xx = tail(x, nn5_h),
             period = "DAILY",
             h = nn5_h)
  #rownames(ele$ff) <- "mean_past"
  ele
})

saveRDS(nn5_imp, "data/nn5.rds")

nn5 = readRDS("data/nn5.rds")

library(forecast)
for (i in 1:length(nn5)) {
  nn5[[i]]$ff = NULL
  tbats_ff = forecast( tbats(nn5[[i]]$x),
                       h=nn5[[i]]$h)$mean
  stlm_ff = forecast(stlf(nn5[[i]]$x),
                     h=nn5[[i]]$h)$mean
  theta_ff = forecast(thetaf(nn5[[i]]$x),
                      h=nn5[[i]]$h)$mean
  nn5[[i]]$ff = rbind(nn5[[i]]$ff, tbats_ff, stlm_ff, theta_ff)
  print(i)
}
saveRDS(nn5, file="data/nn5.rds")


############### NN3 ###########################


library(tscompdata)
library(imputeTS)

nn3_h = 18

nn3_imp = lapply(1:length(nn3), function (i) {
  x = tscompdata::nn3[[i]]
  ele = list(x = head(x, -nn3_h),
             xx = tail(x, nn3_h),
             period = "MONTHLY",
             h = nn3_h)
  #rownames(ele$ff) <- "mean_past"
  ele
})

saveRDS(nn3_imp, "data/nn3.rds")

nn3 = readRDS("data/nn3.rds")

library(forecast)
for (i in 1:length(nn3)) {
  nn3[[i]]$ff = NULL
  arima_ff = forecast( auto.arima(nn3[[i]]$x),
                       h=nn3[[i]]$h)$mean
  ets_ff = forecast(ets(nn3[[i]]$x),
                     h=nn3[[i]]$h)$mean
  theta_ff = forecast(thetaf(nn3[[i]]$x),
                      h=nn3[[i]]$h)$mean
  nn3[[i]]$ff = rbind(nn3[[i]]$ff, arima_ff, ets_ff, theta_ff)
  print(i)
}
saveRDS(nn3, file="data/nn3.rds")

###############  CIF2016 ####################################

CIF2016 = readLines("data/cif-dataset.txt")
true_CIF2016 = readLines("data/cif-results.txt")


CIF2016 = lapply(1:length(CIF2016), function (i) {
  x = as.numeric(unlist( strsplit(CIF2016[[i]], ";") )[-(1:3)])
  xx = as.numeric(unlist( strsplit(true_CIF2016[[i]], ";") )[-(1)])

  ele = list(x = ts(x, frequency = 12),
             xx = ts(xx, frequency = 12),
             period = "MONTHLY",
             h = length(xx))
  ele
})

series_list = CIF2016
library(forecast)
  best_res_CIF2016 = readLines("data/20.csv")
for (i in 1:length(series_list)) {
  lstm_deseas_ff = as.numeric(unlist( strsplit(best_res_CIF2016[[i]], ";") )[-(1)])
  arima_ff = forecast( auto.arima(series_list[[i]]$x),
                       h=series_list[[i]]$h)$mean
  ets_ff = forecast(ets(series_list[[i]]$x),
                    h=series_list[[i]]$h)$mean
  theta_ff = forecast(thetaf(series_list[[i]]$x),
                      h=series_list[[i]]$h)$mean
  series_list[[i]]$ff = rbind(series_list[[i]]$ff, lstm_deseas_ff, arima_ff, ets_ff, theta_ff)
  print(i)
}

CIF2016 = series_list
saveRDS(CIF2016, file="data/CIF2016.rds")

##################### PARTS, CAR PARTS DATASET ##########################################

#library("expsmooth")
library(forecast)
carparts = expsmooth::carparts


parts_h = 8

parts = lapply(1:ncol(carparts), function (i) {
  x = as.numeric(carparts[,i])
  ele = list(x = head(x, -parts_h),
             xx = tail(x, parts_h),
             period = "MONTHLY",
             h = parts_h)
  ele
})

parts = Filter(function(x) !(anyNA(x$x) | anyNA(x$xx)), parts)

saveRDS(parts, file="data/parts.rds")


parts = readRDS("data/parts.rds")
series_list = parts
for (i in 1:length(series_list)) {
  arima_ff = forecast( auto.arima(series_list[[i]]$x),
                       h=series_list[[i]]$h)$mean
  ets_ff = forecast(ets(series_list[[i]]$x),
                    h=series_list[[i]]$h)$mean
  theta_ff = forecast(thetaf(series_list[[i]]$x),
                      h=series_list[[i]]$h)$mean
  series_list[[i]]$ff = rbind(series_list[[i]]$ff, arima_ff, ets_ff, theta_ff)
  print(i)
}
parts = series_list
saveRDS(parts, file="data/parts.rds")

#################################### HOSPITAL

library(forecast)
hosp = expsmooth::hospital


hosp_h = 8

hospital = lapply(1:ncol(hosp), function (i) {
  x = hosp[,i]
  ele = list(x = head(x, -hosp_h),
             xx = tail(x, hosp_h),
             period = "MONTHLY",
             h = hosp_h)
  ele
})

hospital = Filter(function(x) !(anyNA(x$x) | anyNA(x$xx)), hospital)

#saveRDS(parts, file="data/parts.rds")


#parts = readRDS("data/parts.rds")
series_list = hospital
for (i in 1:length(series_list)) {
  arima_ff = forecast( auto.arima(series_list[[i]]$x),
                       h=series_list[[i]]$h)$mean
  ets_ff = forecast(ets(series_list[[i]]$x),
                    h=series_list[[i]]$h)$mean
  theta_ff = forecast(thetaf(series_list[[i]]$x),
                      h=series_list[[i]]$h)$mean
  series_list[[i]]$ff = rbind(series_list[[i]]$ff, arima_ff, ets_ff, theta_ff)
  print(i)
}
hospital = series_list
saveRDS(hospital, file="data/hospital.rds")


################################################################################################
#####################  MELBOURNE PEDESTRIAN COUNT DATA #########################################
################################################################################################


library(rwalkr)


ped = melb_walk_fast(2016:2020)

saveRDS(ped, file = "raw_pedestrians.rds")

ped = readRDS("raw_pedestrians.rds")

sensor_names = unique(ped$Sensor)


ped_h = 24

pedestrian = lapply(sensor_names, function (sensor) {
  
  ped.ts = unlist(ped[ ped$Sensor == sensor, 5])
  
  
  min.valid = min(which(!is.na(ped.ts)))  -1
  if (min.valid > 0) {
    ped.ts = ped.ts[-(1:min.valid)]
  }
  
  
  max.valid = max(which(!is.na(ped.ts)))
  ped.ts = ped.ts[1:max.valid]
  
  ped.ts = as.numeric(ped.ts)
  #plot(ped.ts, type="l")
  #if (anyNA(ped.ts)) {
  num_last_valid = length(ped.ts) - max(is.na(ped.ts)) -1
  
  ped.ts = tail(ped.ts,num_last_valid)
  
  ped.ts = imputeTS::na_seadec(ts(as.numeric(head(ped.ts, -ped_h)), frequency = 24*7))
  
  ele = list(x = ts(as.numeric(head(ped.ts, -ped_h)), frequency = 24*7),
             xx = ts(as.numeric(tail(ped.ts, ped_h), frequency = 24*7)),
             period = "HOURLY",
             h = ped_h)
  
  
  
  ele
})

saveRDS(pedestrian, file = "pedestrian.rds")

pedestrian = readRDS("pedestrian.rds")

series_list = pedestrian
library(forecast)
for (i in 1:length(series_list)) {
  tbats_ff = forecast( tbats(series_list[[i]]$x),
                       h=series_list[[i]]$h)$mean
  # stlm_ff = forecast(stlf(series_list[[i]]$x),
  #                    h=series_list[[i]]$h)$mean
  theta_ff = forecast(thetaf(series_list[[i]]$x),
                      h=series_list[[i]]$h)$mean
  series_list[[i]]$ff = rbind(series_list[[i]]$ff, tbats_ff, theta_ff)
  print(i)
}


pedestrian = series_list
saveRDS(pedestrian, file = "pedestrian.rds")


#################################################################################
###  https://developer.ibm.com/exchanges/data/all/double-pendulum-chaotic/ ######
#################################################################################

folder_name = "data/train_and_test_split/dpc_dataset_traintest_4_200_csv/train/"

files = list.files(folder_name, full.names = TRUE)

pend_h = 200

pendulum = lapply(files, function (file) {
  pen = read.csv(file, sep=" ", header = FALSE)
  pen = pen[,5]
  pen = ts(pen)
  
  ele = list(x = head(pen, -pend_h),
             xx = tail(pen, pend_h),
             h = pend_h)
  ele
})

saveRDS(pendulum, file="data/pendulum.rds")



##############################################################################
#######   Johns Hopskins COVID data, daily deaths ############################
###      https://github.com/CSSEGISandData          ##########################
####  Retrieved aug 2
##############################################################################

covid_ts = readr::read_csv("data/time_series_covid19_deaths_global.csv")
library(forecast)
covid_ts = lapply(1:nrow(covid_ts), function (i) {
  tbl = covid_ts[i,]
  id = paste(tbl$`Province/State`, tbl$`Country/Region`, sep="_")

  x = as.numeric(tbl[-(1:4)])
  x = x[1:90]
  # x = head(x, -60) #remove
   if (sum(x) <1) return(list(x=x))
   x = x[ min(which(x>0)):length(x)]
   if (length(x) <14+14) return(list(x=0:1))
  xx = tail(x, 14)
  x = head(x, -14)
  x=ts(x, frequency = 7)
  
  arima_ff = forecast( auto.arima(x),
                       h=length(xx))$mean
  ets_ff = forecast(ets(x),
                    h=length(xx))$mean
  theta_ff = forecast(thetaf(x, h=length(xx)),
                      h=length(xx))$mean
  
  
  list(id=id, x=x, xx=xx, ff=rbind(arima_ff, ets_ff, theta_ff))
})

covid_ts = Filter(function(ll) mean(tail(ll$x,7)) > 20 && length(ll$x)>13, covid_ts)

saveRDS(covid_ts, "data/covid_ts.rds")

#############################################################################
######## UBER RIDES DATA                          ###########################
####  https://github.com/fivethirtyeight/uber-tlc-foil-response  ############
#############################################################################


a = read.csv("data/uber-tlc-foil-response-master/uber-trip-data/uber-raw-data-janjune-15.csv")


#aggregrate by hour ?

## split by Zone (locationID)
bybase = split(a, a$locationID)




# 


######aggregate by location Id
for (i in 1:length(bybase)) {
mincount = table(bybase[[125]]$Pickup_date)
 print(sum(as.numeric(mincount)))
}
plot(as.numeric(mincount), type="l")

tsintermittent::crost(as.numeric(mincount), h=60)
system.time({
cro = tsintermittent::crost.ma(as.numeric(mincount), h=60)}
)


as.Date(DATE, format = "%Y-%m-%d")

as.POSIXct("080406 10:11", format = "%y%m%d %H:%M")


timescale = 60*24
agg = rep( 1:ceiling(length(mincount) / timescale), each=timescale)
agg = agg[1:length(mincount)]

dset = list(mincount=as.numeric(mincount),agg= as.factor(agg))
timeagg = aggregate(mincount~agg, dset, sum)
plot(as.numeric(timeagg$mincount), type="l")
#simple aggregation, by groups of 5 mins, 15 mins, 30 mins, 60mins usando rep(1:numhours,each=XXX)


##### IDEA: can aggregate by Borough




################################################################################################
###### Opendatasoft airbnb #####################################################################
##### https://public.opendatasoft.com/explore/?sort=modified ###################################
################################################################################################

a = read.csv("data/airbnb-averages_2016.csv", sep=";")
b = a[ , c("Neighbourhood", "Number.of.accommodations", "Date", "City", "Country")]

################################################################################
########### Earth surface temperature data #####################################
########### from opendatasoft ##################################################
################################################################################
a = read.csv("data/earth-surface-temperature-data.csv", sep=";")
  
#STEAM  
#https://api.steampowered.com/ISteamUserStats/GetNumberOfCurrentPlayers/v1/?appid=730
  
########################################################################
#Multitask learning and benchmarking with clinical time series data


#########################
############  https://github.com/awesomedata/awesome-public-datasets#timeseries


####### GEFCOMP2014

################## CRYPTODATADOWNLoad ############################
