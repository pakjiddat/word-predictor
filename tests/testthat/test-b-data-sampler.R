test_that("Sample file of correct size is generated", {
    # The required files
    wp$rf <- c("input.txt")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The sample file name
    sfn <- paste0(ed, "/sample.txt")
    # An object of class DataSampler is created
    ds <- DataSampler$new(dir = ed, ve = wp$ve)
    # The sample file is generated
    ds$generate_sample(
        fn = "input.txt",
        ss = 0.5,
        ic = F,
        ir = F,
        ofn = "sample.txt",
        is = T
    )
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new()
    # The file info is fetched
    fi <- da$get_file_info(sfn)
    # Check that sample file has the correct number of lines
    expect_equal(fi[["file_stats"]][1, 2], 360)

    # The cleanup action is performed
    source("./cu.R")
})


test_that("Test, Train and Validation files are generated", {
    # The required files
    wp$rf <- c("input.txt")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The files to clean
    fns <- c("train", "test", "validate")
    # An object of class DataSampler is created
    ds <- DataSampler$new(dir = ed, ve = wp$ve)
    # The train, test and validation files are generated
    ds$generate_data(
        fn = "input.txt",
        percs = list(
            "train" = 0.8,
            "test" = 0.1,
            "validate" = 0.1
        )
    )
    # Each file is checked
    for (fn in fns) {
        # The test file name
        fn <- paste0(ed, "/", fn, ".txt")
        # Check if file exists
        expect_true(file.exists(fn))
    }

    # The cleanup action is performed
    source("./cu.R")
})
