# global-forecasting
Paper on global and local methods for time series forecasting.
Intended for reproducibility of the results in the paper:

**Principles and Algorithms for Forecasting Groups of Time Series: Locality and Globality**

https://arxiv.org/abs/2008.00444

Not actual forecasting tools! (though it can be used as such, it is not supported).

Everything in the repo is based on R (code, datasets) and Latex.



## Contents

 * Installation instructions
 * The code for global models
 * Experiments
 * Analysis of Results
 * Data
 * Paper source

## Installation instructions

Because the data is quite large, besides checking the repo,
the file:

https://github.com/pmontman/paper-global-forec-princip/releases/download/data-results/data-results.zip


Should be downloaded and extracted in the src/ folder.
So you should have src/data and src/results folders after extracting data-results.zip.

## A quick note

If mainly interested how the global models described in the paper work can be implemented,
here is a simple example for illustration purposes.

```R
series_list #lets say this is a list of time series, each series is a vector
lag = 16    #the order of the autoregression

#time delay embedding / lagmatrices and stacking of all time series in one line of code
X = do.call("rbind", lapply(series_list, function(x) embed(x, lag+1)))
Y = X[,1]; X = X[,-1] #creating output and input matrices (embed() in R reverses the order)
global_ar = lm(Y~X-1) #and here is our Global AR or order 16.

#we could also use another model class instead of a lm()
#once we get the X, Y matrices is like a regression problem!

###
#prediction of the next time step of one input series
#because of embed() we reverse the order of the input
rev(tail(series_list[[1]], lag)) %*% global_ar$coefficients
```

This is not exactly how it is implemented in this project.
There are complications such as preprocessing (e.g. normalization), additional features, etc. and also
performance reasons, a large set of time series for a big lag order will produce a
GBs size matrix, so it requires chunking and optimization strategies such as stochastic gradient descent.


## Basic Global Models

Modeling functions are in src/model.R

The main convenience function is `analyze_glob_model`
That takes `my_series` a list of series with the elements in a format
similar to the Mcomp, with elements:

 * $x The observed time series
 * $xx The future part
 * $id (optional)
 * $feat Features of the series, for some models (optional, used in some experiments)

Another parameter is `fit_model_fun`, the modeling function, it can be
`linear_model`, `poly_2_model`, `poly_3_model`, `tree_model`, `deep_model`.

Another parameter is `MASE_norm`, whether to apply normalization by the MASE denominator
to each series. It is `TRUE` by default, this normalization was applied to most experiments in the paper.

Another parameter is `lag_range`, the lags orders to experiment with, for example 
`lag_range=20:50` will compute the forecasts for all global models from lag 20 to lag 50.

The parameter `model_name` just identifies the model that is being used with the given string, attaching the lag order,
in the output forecasts.

The output of `analyze_glob_model` are the average errors calculated for the dataset: MAE, SMAPE, for each of the models.

Example of usage in the experiments code.

## Experiments

The file src/experiments.R has the code to launch the experiments.
Just set the working directory, load model.R and run the experiments (it will take a lot of time).
You might take the experiments.R file as inspiration to run your own tests.

To simplify, we have added the results of the experiments in the results folder.

Some experiments might be missing (e.g. a few datasets) in an almost two year
development cycle. How to run them again should be self-explanatory given the data.

## Analyzing the experiments

The code to analyze the experiments is in src/analysis.R,
for creating the plots and tables in the paper.
It requires the results to be downloaded (see Instructions section) or reproduced

Numerical results can be further scrutinized by looking at the files in the src/results folder.
For example the file src/results/total_experiment.RData loads the list `list_results`
with the results for the M1-M4/Tourism yearly/quarterly/monthly datasets. Examining
this list gives the errors in MASE and SMAPE for the benchmark models and the ones
used in the paper.


## Datasets
The folder src/data contains the datasets that were used in the experiments.
The file src/pre_datasets.R has the code to process the datasets from their raw values
to the version it was finally used (data sources, data cleaning and so on).

