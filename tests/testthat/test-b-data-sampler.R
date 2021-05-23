test_that("Sample file of correct size is generated", {
    # The training file name
    tfn <- paste0(ddir2, "/train.txt")
    # If the train.txt file exists, then it is removed
    if (file.exists(tfn)) {
        # The file is removed
        file.remove(tfn)
    }
    # An object of class DataSampler is created
    ds <- DataSampler$new(ddir = ddir2, mdir = ddir2)
    # The sample file is generated
    ds$generate_sample(
        fn =  "input.txt",
        ss = 0.99,
        ic = F,
        ir = F,
        t = "tr",
        is = T
    )
    # An object of class DataAnalyzer is created
    da <- DataAnalyzer$new()
    # The file info is fetched
    fi <- da$get_file_info(tfn)
    # The file size is checked
    expect_equal(fi[["file_stats"]][1,6], "11.2 Mb")

})


test_that("Test, Train and Validation files of correct size are generated", {
    # The files to clean
    fns <- c("train", "test", "validate")
    # Each file is removed
    for (fn in fns) {
        # The test file name
        fn <- paste0(ddir2, "/", fn, ".txt")
        # If the file exists, then it is removed
        if (file.exists(fn))
            file.remove(fn)
    }
    # An object of class DataSampler is created
    ds <- DataSampler$new(ddir = ddir2, mdir = ddir2)
    # The train, test and validation files are generated
    ds$generate_data(
        fn =  "input.txt",
        dir = ddir2,
        percs = list(
            "train" = 0.8,
            "test" = 0.1,
            "validate" = 0.1
        )
    )
    # Each file is checked
    for (fn in fns) {
        # The test file name
        fn <- paste0(ddir2, "/", fn, ".txt")
        # Check if file exists
        expect_true(file.exists(fn))
    }

})
