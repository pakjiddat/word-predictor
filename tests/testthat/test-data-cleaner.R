test_that("Performance of file cleaning function is acceptable", {
    time_taken <- system.time({
        memory_used <- mem_change({
            # The data cleaner object is created
            dc <- DataCleaner$new("./data/models/test.txt")
            # The sample input file is cleaned
            dc$clean_file()
        });
    });
    # The memory used
    memory_used <- dc$format_size(memory_used)
    # The time taken is tested
    expect_lt(time_taken[[3]], 2)
    # The memory usage is tested
    expect_lt(memory_used, 2)
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

test_that("Performance of line cleaning function is acceptable", {
    time_taken <- system.time({
        memory_used <- mem_change({
            # The DataCleaner object is created
            dc <- DataCleaner$new()
            # The test file is read
            lines <- dc$read_lines("./data/models/test.txt", -1)
            # The sample lines are cleaned
            cl <- dc$clean_lines(lines)
        });
    });
    # The memory used
    memory_used <- dc$format_size(memory_used)
    # The time taken is tested
    expect_lt(time_taken[[3]], 1)
    # The memory usage is tested
    expect_lt(memory_used, 2)
});
