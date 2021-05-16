test_that("A sample text file is cleaned successfully", {
    # If the test-clean file exists, then it is removed
    if (file.exists("./data/model/test-clean.txt"))
        file.remove("./data/model/test-clean.txt")
    # The custom data cleaning options
    dc_opts <- list("remove_bad" = T, "remove_stop" = T)
    # The data cleaner object is created
    dc <- DataCleaner$new("./data/model/test.txt", dc_opts)
    # The sample file is cleaned
    dc$clean_file()
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new()
    # The file info is fetched
    fi <- da$get_file_info("./data/model/test-clean.txt")
    # The file size is checked
    expect_equal(fi[["file_stats"]][1,6], "647.1 Kb")
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
