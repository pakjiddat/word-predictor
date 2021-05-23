test_that("A sample text file is cleaned successfully", {
    # The files to clean
    fns <- c("test", "validate")
    # Each file is cleaned
    for (fn in fns) {
        # The cleaned test file name
        cfn <- paste0(ddir2, "/", fn, "-clean.txt")
        # The test file name
        fn <- paste0(ddir2, "/", fn, ".txt")

        # If the test-clean file exists, then it is removed
        if (file.exists(cfn))
            file.remove(cfn)

        # The data cleaner object is created
        dc <- DataCleaner$new(fn)
        # The sample file is cleaned
        dc$clean_file()

        # Check that the file exists
        expect_true(file.exists(cfn))
    }
});

test_that("Sample line of text are cleaned as expected", {
    # Test data is read
    l <- c(
        '"Follow me on Twitter if you want," Brown told viewers'
    )
    # The expected results
    res <- c(
        "follow me on twitter if you want brown told viewers"
    )
    # The DataCleaner object is created
    dc <- DataCleaner$new()
    # The line is cleaned
    cl <- dc$clean_lines(l)
    # The actual cleaned data is compared with expected data
    expect_equal(cl, res)
});
