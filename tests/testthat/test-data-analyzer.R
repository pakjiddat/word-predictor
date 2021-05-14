test_that("Performance of random sample generator is acceptable", {
    time_taken <- system.time({
        memory_used <- mem_change({
            # The DataAnalyzer object is created
            da <- DataAnalyzer$new("./data/en_US.input.txt")
            # The options
            opts <- list("save_data" = T, "output_file" = "./data/sample.txt")
            # The sample file is generated
            data <- da$generate_sample(1000, opts)
        });
    });
    # The memory used
    memory_used <- da$format_size(memory_used)
    # The time taken is tested
    expect_lt(time_taken[[3]], 5)
    # The memory usage is tested
    expect_lt(memory_used, 2)
    # The number of lines generated should be 1000
    expect_equal(length(data), 1000)
});

test_that("File information is corrected calculated", {
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new("./data/sample.txt")
    # The sample file is generated
    file_info <- da$get_file_info("./data/sample.txt")
    # The total line count is checked
    expect_equal(file_info$total_line_count, 219680)
    # The mean line length is checked
    expect_equal(file_info$mean_line_length, 135)
    # The total size is checked
    expect_equal(file_info$total_size, "28.5 Mb")
});

test_that("plot_data function works", {
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new("./data/n2.txt")
    # The top features plot is checked
    da$plot_data(da_opts)
    # The coverage plot is checked
    da_opts$type <- "coverage"
    da$plot_data(da_opts)
    # Checks that Rplots.pdf file exists
    expect_true(file.exists("./Rplots.pdf"))
});
