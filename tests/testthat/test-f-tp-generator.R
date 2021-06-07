test_that("The combined transition probabilities file is correctly generated", {
    # The required files
    wp$rf <- c("n1.RDS", "n2.RDS", "n3.RDS", "n4.RDS")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The list of output files
    fns <- c("words", "model-4", "tp2", "tp3", "tp4")

    # The TPGenerator object is created
    tp <- TPGenerator$new(opts = list(n = 4, dir = ed), ve = wp$ve)
    # The combined transition probabilities are generated
    tp$generate_tp()
    # Each file is checked
    for (fn in fns) {
        # The file name
        fn <- paste0(ed, "/", fn, ".RDS")
        # Check if the file exists
        expect_true(file.exists(fn), label = fn)
    }

    # The cleanup action is performed
    source("./cu.R")
})
