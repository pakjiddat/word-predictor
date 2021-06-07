test_that("Correct file information is returned", {
    # The required files
    wp$rf <- c("test.txt")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The test file name
    cfn <- paste0(ed, "/test.txt")
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new(ve = wp$ve)
    # The file info is fetched
    fi <- da$get_file_info(cfn)
    # The line count is checked
    expect_equal(fi$file_stats$total_lc, 73)

    # The cleanup action is performed
    source("./cu.R")
})
test_that("plot_data function works", {
    # The required files
    wp$rf <- c("n2.RDS")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The n-gram file name
    nfn <- paste0(ed, "/n2.RDS")
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new(nfn, ve = wp$ve)
    # The top features plot is checked
    df <- da$plot_n_gram_stats(opts = list(
        "type" = "top_features",
        "n" = 10,
        "save_to" = "png",
        "dir" = ed
    ))
    # The image file path
    fp <- paste0(ed, "/top_features.png")
    # Checks that the top feature is in_the
    expect_equal(df$pre[1], "in_the")
    # Checks that the top feature frequency is 4
    expect_equal(df$freq[1], 4)
    # Check that the plot file was successfully created
    expect_true(file.exists(fp))

    # The coverage plot is checked
    df <- da$plot_n_gram_stats(opts = list(
        "type" = "coverage",
        "n" = 10,
        "save_to" = "png",
        "dir" = ed
    ))
    # The image file path
    fp <- paste0(ed, "/coverage.png")
    # Checks that the most occuring words are those with frequency 1
    expect_equal(df$pre[1], "1")
    # Checks that words with frequency 1 make up 95.8% of all words
    expect_equal(round(df$freq[1], 1), 95.8)
    # Check that the plot file was successfully created
    expect_true(file.exists(fp))

    # The cleanup action is performed
    source("./cu.R")
})
test_that("ngrams with correct frequency are returned", {
    # The required files
    wp$rf <- c("n2.RDS")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The n-gram file name
    nfn <- paste0(ed, "/n2.RDS")
    # The DataAnalyzer object is created
    da <- DataAnalyzer$new(nfn, ve = wp$ve)
    # Bi-grams starting with "and_" are returned
    df <- da$get_ngrams(fn = nfn, c = 10, pre = "^and_*")
    # The data frame is sorted by frequency
    df <- df[order(df$freq, decreasing = T), ]
    # The frequency of the bi-gram "and_the"
    f <- as.numeric(df[df$pre == "and_the", "freq"])
    # The frequency is checked
    expect_equal(f, 2)

    # The cleanup action is performed
    source("./cu.R")
})
