test_that("Sample file of correct size is generated", {
    # The training file name
    tfn <- "./data/model/train.txt"
    # If the train.txt file exists in ./data/models, then it is removed
    if (file.exists(tfn)) {
        # The file is removed
        file.remove(tfn)
    }
    # An object of class DataSampler is created
    ds <- DataSampler$new(ddir = "./data", mdir = "./data/model")
    # The sample file is generated
    ds$generate_sample(
        fn =  "input.txt",
        ss = 0.99,
        ic = F,
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
    # If the training file exists, then it is removed
    if (file.exists("./data/model/train.txt"))
        file.remove("./data/model/train.txt")
    # If the test file exists, then it is removed
    if (file.exists("./data/model/test.txt"))
        file.remove("./data/model/test.txt")
    # If the validate file exists, then it is removed
    if (file.exists("./data/model/validate.txt"))
        file.remove("./data/model/validate.txt")

    # An object of class DataSampler is created
    ds <- DataSampler$new(ddir = "./data", mdir = "./data/model")
    # The train, test and validation files are generated
    ds$generate_data(
        fn =  "input.txt",
        dir = "./data/model",
        percs = list(
            "train" = 0.8,
            "test" = 0.1,
            "validate" = 0.1
        )
    )
    # An object of class DataAnalyzer is created
    da <- DataAnalyzer$new()
    # The file info is fetched
    fi <- da$get_file_info("./data/model")
    # The file sizes are checked
    expect_equal(fi[["file_stats"]][1,6], "1.1 Mb")
    expect_equal(fi[["file_stats"]][2,6], "9 Mb")
    expect_equal(fi[["file_stats"]][3,6], "1.1 Mb")
})
