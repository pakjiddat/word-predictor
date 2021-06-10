# nolint start

# The wordpredictor package is loaded
# library(wordpredictor)

suppressPackageStartupMessages({
    library(stringr)
    library(digest)
    library(ggplot2)
    library(SnowballC)
    library(patchwork)
    library(dplyr)
})

# The list of files in R folder
fl <- list.files(path = "../../R", pattern = "*.R", full.names = T)
# Each file is sourced
for (fn in fl) {
    source(fn)
}
# nolint end

# If level of verbosity in the information messages
ve <- 0
# The action to take after the test ends
ea <- "t"

# The option are set
options("wordpredictor" = list(
    "ve" = ve,
    "ea" = ea,
    "rf" = NULL,
    "ed" = NULL
))
# The wordpredictor options
wp <- getOption("wordpredictor")
