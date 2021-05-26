test_that("Sample file of correct size is generated", {
    # All files in ddir
    fl <- list.files(ddir2, full.names = T)
    # All files in ddir2 are removed
    for (fn in fl) {
        # The file is removed
        file.remove(fn)
    }
    # input.txt is copied from ddir1 to ddir2
    file.copy(paste0(ddir1, "/input.txt"), ddir2)
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
        fn = "input.txt",
        ss = 0.5,
        ic = F,
        ir = F,
        ofn = "train.txt",
        is = T
    )
    # Check that sample file exists
    expect_true(file.exists(tfn))
})


test_that("Test, Train and Validation files of correct size are generated", {
    # The files to clean
    fns <- c("train", "test", "validate")
    # Each file is removed
    for (fn in fns) {
        # The test file name
        fn <- paste0(ddir2, "/", fn, ".txt")
        # If the file exists, then it is removed
        if (file.exists(fn)) {
              file.remove(fn)
          }
    }
    # An object of class DataSampler is created
    ds <- DataSampler$new(ddir = ddir2, mdir = ddir2)
    # The train, test and validation files are generated
    ds$generate_data(
        fn = "input.txt",
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
