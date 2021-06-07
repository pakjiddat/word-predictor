test_that("Generation of n-gram Model works", {
    # The required files
    wp$rf <- c("input.txt")
    options("wordpredictor" = wp)
    source("./inc.R")

    # ModelGenerator class object is created
    mg <- ModelGenerator$new(
        name = "default-model",
        desc = "1 MB size and default options",
        fn = "def-model.RDS",
        df = "input.txt",
        n = 4,
        ssize = 0.99,
        dir = ed,
        dc_opts = list(),
        tg_opts = list(),
        ve = wp$ve
    )
    # The n-gram model is generated
    mg$generate_model()
    # The file name
    fn <- paste0(ed, "/def-model.RDS")
    # Check if file exists
    fe <- file.exists(fn)

    # Check that the model file exists in the model folder
    expect_true(fe)

    # The cleanup action is performed
    source("./cu.R")
})
