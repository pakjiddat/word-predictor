test_that("Correct file information is returned", {
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new()
    # The file info is fetched
    fi <- da$get_file_info("./data/model/test-clean.txt")
    # The file size is checked
    expect_equal(fi[["file_stats"]][1,6], "647.1 Kb")
});

test_that("plot_data function works", {
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new("./data/model/n2.RDS")
    # The top features plot is checked
    df <- da$plot_n_gram_stats(opts = list(
        "type" = "top_features",
        "n" = 10,
        "save_to" = "png",
        "dir" = "./data/model"
    ))
    # Checks that the top feature is new_york
    expect_equal(df$pre[1], "new_york")
    # Checks that the top feature frequency is 37
    expect_equal(df$freq[1], 37)
    # Check that the plot file was successfully created
    expect_true(file.exists("./data/model/top_features.png"))

    # The coverage plot is checked
    df <- da$plot_n_gram_stats(opts = list(
        "type" = "coverage",
        "n" = 10,
        "save_to" = "png",
        "dir" = "./data/model"
    ))
    # Checks that the most occuring words are those with frequency 1
    expect_equal(df$pre[1], "1")
    # Checks that words with frequency 1 make up 94.36% of all words
    expect_equal(df$freq[1], 94.36)
    # Check that the plot file was successfully created
    expect_true(file.exists("./data/model/coverage.png"))
});

test_that("ngrams with correct frequency are returned", {
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new("./data/model/n2.RDS")
    # Bi-grams starting with "great_" are returned
    df <- da$get_ngrams(fn = "./data/model/n2.RDS", c = 10, pre = "^great_*")
    # The frequency of the bi-gram "great_care"
    f <- as.numeric(df[df$pre == "great_care", "freq"])
    # The frequency is checked
    expect_equal(f, 2)
});
