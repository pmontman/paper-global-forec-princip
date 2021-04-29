# global-forecasting
Paper on global and local methods for time series forecasting.
Intended for reproducibility of the results in the paper
TITLE, not for actual forecasting (though it can be used as such, it is not supported).

Everything in the repo is based on R (code, datasets) and Latex.



## Contents

 * Installation instructions
 * The code for global models
 * Experiments
 * Analysis of Results
 * Data
 * Paper source

## Installation instructions

Because the data is quite large, besides checking the repo
the file: LINK should be downloaded and extracted in the src folder.
So you should have src/data and src/results folders after extracting.




## Basic Global Models

Modeling functions are in src/model.R

The main convenience function is `analyze_glob_model`
That takes `my_series` a list of series with the elements in a format
similar to the Mcomp, with elements:

 * $x The observed time series
 * $xx The future part
 * $id
 * $feat

Another parameter is `fit_model_fun`, the modeling function, it can be
`linear_model`, `poly_2_model`, `poly_3_model`, `tree_model`, `deep_model`.

Another parameter is `MASE_norm`, whether to apply normalization by the MASE denominator
to each series. It is `TRUE` by default, this normalzation was applied to most experiments in the paper.

Another parameter is `lag_range`, the lags orders to experiment with, for example 
`lag_range=20:50` will compute the forecasts for all global models from lag 20 to lag 50.

The parameter `model_name` just identifies the model that is being used with the given string, attaching the lag order,
in the output forecasts.

The output of `analyze_glob_model` are the average errors calculated for the dataset: MAE, SMAPE.

## Experiments

The file src/experiments.R has the code to launch the experiments.
Just set the working directory, load model.R and run the experiments (it will take a lot of time).
You might take the experiments.R file as inspiration to run your own tests.

To simplify, we have added the results of the experiments in the results folder.

Some experiments might be missing (e.g. a few datasets) in an almost two year
development cycle. How to run them again should be self-explanatory given the data.

## Analyzing the experiments

The code to 


Analyze

## Datasets
The folder src/data contains the datasets that were used in the experiments.
The filer src/pre_datasets.R has the code to process the datasets from their raw values
to version it was finally used (data sources, data cleaning and so on).

