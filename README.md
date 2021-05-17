
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

<!-- badges: start -->

<!-- badges: end -->

The goal of the wordpredictor package is to provide a flexible and easy
to use framework for generating n-gram models for word prediction.

The package allows generating n-gram models from input text files. It
also allows exploring n-grams using plots. Additionally it provides
methods for measuring n-gram model performance using Perplexity and
accuracy.

The n-gram model may be customized using several options such as n-gram
size, data cleaning options and options for tokenization.

The wordpredictor package is based on R6 classes. It is easy to
customize and improve.

## Installation

You can install the released version of wordpredictor from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("wordpredictor")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pakjiddat/word-predictor")
```

## Generating the model

The following example shows how to generate a n-gram model.

``` r
library(wordpredictor)

# The following code generates n-gram model using default options for data
# cleaning and tokenization. See the following section on how to customize these
# options. Note that input.txt is the name of the input data file. It should be
# present in the data directory. ddir is the data directory. mdir is the model
# directory. The output model file, which is def-model.RDS will be placed in
# this directory.

# ModelGenerator class object is created
mg <- ModelGenerator$new(
    name = "def-model",
    desc = "N-gram model generating using default options",
    fn = "def-model.RDS",
    df = "input.txt",
    n = 4,
    ssize = 10,
    ddir = "./tests/testthat/data",
    mdir = "./tests/testthat/data/model",
    dc_opts = list(),
    tg_opts = list(),
    ve = 2
)

# Generates n-gram model. The output is the file
# ./tests/testthat/data/model/def-model.RDS
mg$generate_model()
```

The output file **def-model.RDS** is placed in the
**./tests/testthat/data/model** folder. It represents the n-gram model.

## Predicting words

The following example shows how to predict the next word given a set of
words:

``` r
library(wordpredictor)

# An object of class ModelPredictor is created. The mf parameter is the name of
# the model file that was generated in the previous example.
mp <- ModelPredictor$new(mf = "./tests/testthat/models/def-model.RDS")
# Given the words: "how are", the next word is predicted. The top 3 most likely
# next words are returned along with their respective probabilities.
res <- mp$predict_word(words = "how are", 3)
```
