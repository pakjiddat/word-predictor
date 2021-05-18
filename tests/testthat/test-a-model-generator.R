test_that("Generation of n-gram Model words", {
    # The test is skipped since it can take some time
    skip_on_cran()

    # ModelGenerator class object is created
    mg <- ModelGenerator$new(
        name = "default-model",
        desc = "N-gram model based on default data cleaning and tokenization options",
        fn = "def-model.RDS",
        df = "input.txt",
        n = 4,
        ssize = 10,
        ddir = "./data",
        mdir = "./data/model",
        dc_opts = list(),
        tg_opts = list()
    )
    # The n-gram model is generated
    mg$generate_model()
    # Check if file exists
    fe <- file.exists("./data/model/def-model.RDS")
    # Check that the model file exists in the model folder
    expect_true(fe)
})
