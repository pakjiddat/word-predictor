library(digest)
library(stringr)
library(dplyr)
library(pryr)
library(ggplot2)
library(patchwork)
library(SnowballC)
library(devtools)
library(testthat)
library(covr)

# The prefix for sourcing files
spre <- "./"
# The current working directory is set
setwd("../../")

source(paste0(spre, "R/data-analyzer.R"))
source(paste0(spre, "R/data-cleaner.R"))
source(paste0(spre, "R/data-sampler.R"))
source(paste0(spre, "R/text-file-processor.R"))
source(paste0(spre, "R/token-generator.R"))
source(paste0(spre, "R/tp-generator.R"))
source(paste0(spre, "R/model/model.R"))
source(paste0(spre, "R/model/model-evaluator.R"))
source(paste0(spre, "R/model/model-generator.R"))
source(paste0(spre, "R/model/model-predictor.R"))
