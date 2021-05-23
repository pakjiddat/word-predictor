# The wordpredictor package is loaded
library(wordpredictor)

# The dir prefix
pre <- "./"
# pre <- "./tests/testthat/"

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

# Check if the directories exist
check_dirs(ddir1, ddir2, mdir, msdir, sdir)
