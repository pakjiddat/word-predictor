test_that("Word probability is correctly calculated", {
    # The required files
    wp$rf <- c("def-model.RDS")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The model file name
    mfn <- paste0(ed, "/def-model.RDS")
    # ModelPredictor class object is created
    mp <- ModelPredictor$new(mf = mfn, ve = wp$ve)
    # The probability that the next word is "you" given the prev words "how" and
    # "are"
    prob <- mp$get_word_prob(word = "you", pw = c("how", "are"))
    # Check that probability is correct
    expect_equal(prob, 0.0024581)
    # The probability that the next word is "you" given the prev words "how" and
    # "is"
    prob <- mp$get_word_prob(word = "you", pw = c("how", "is"))
    # Check that probability is correct
    expect_equal(prob, 0.0024581)

    # The cleanup action is performed
    source("./cu.R")
})

test_that("The next word is correctly predicted", {
    # The required files
    wp$rf <- c("def-model.RDS")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The model file name
    mfn <- paste0(ed, "/def-model.RDS")
    # ModelPredictor class object is created
    mp <- ModelPredictor$new(mf = mfn, ve = wp$ve)
    # The next word is predicted
    nws <- mp$predict_word("called his", count = 10)
    # Check that the most likely next word is "case"
    expect_equal(nws[["words"]][1], "case")
    # Check that the most likely next word has probability 0.625
    expect_equal(nws[["probs"]][1], 0.05555556)

    # The cleanup action is performed
    source("./cu.R")
})

test_that("Perplexity is correctly calculated", {
    # The required files
    wp$rf <- c("def-model.RDS")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The model file name
    mfn <- paste0(ed, "/def-model.RDS")
    # ModelPredictor class object is created
    mp <- ModelPredictor$new(mf = mfn, ve = wp$ve)
    # The sentence whoose Perplexity is to be calculated
    l <- "last year at this time i was preparing for a trip to rome"
    # The line is split in to words
    w <- strsplit(l, " ")[[1]]
    # The Perplexity of the sentence is calculated
    p <- mp$calc_perplexity(w)
    # Check that perplexity is correct
    expect_equal(p, 1767)

    # The cleanup action is performed
    source("./cu.R")
})
