# nolint start

# The wordpredictor package is loaded
# library(wordpredictor)

library(stringr)
library(digest)
library(ggplot2)
library(SnowballC)
library(patchwork)
library(dplyr)
library(pryr)

# The list of files in R folder
fl <- list.files(path = "../../R", pattern = "*.R", full.names = T)
# Each file is sourced
for (fn in fl) {
    source(fn)
}
# nolint end

# The dir prefix
pre <- "./"

# The model directory within data1 directory
mdir <- paste0(pre, "data1/model")
# The data directory
ddir1 <- paste0(pre, "data1")

# The data directory
ddir2 <- paste0(pre, "data2")

# The models directory
msdir <- paste0(pre, "models")
# The stats directory
sdir <- paste0(pre, "models/stats")
# Sets the stylr identation level to 4
options(
    styler.addins_style_transformer = "styler::tidyverse_style(indent_by = 4)")

# The setup_dirs.R file is sourced
source("./setup_dirs.R")
# Check if the directories exist
check_dirs(ddir1, ddir2, mdir, msdir, sdir)
