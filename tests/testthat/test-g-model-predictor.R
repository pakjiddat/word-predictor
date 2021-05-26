test_that("Word probability is correctly calculated", {
    # The model file name
    mfn <- paste0(mdir, "/def-model.RDS")
    # ModelPredictor class object is created
    mp <- ModelPredictor$new(mf = mfn)
    # The probability that the next word is "you" given the previous words "how"
    # and "are"
    prob <- mp$get_word_prob(word = "you", pw = c("how", "are"))
    # Check that probability is correct
    expect_equal(round(prob, 3), 0.571)
    # The probability that the next word is "you" given the previous words "how"
    # and "is"
    prob <- mp$get_word_prob(word = "you", pw = c("how", "is"))
    # Check that probability is correct
    expect_equal(round(prob, 3), 0.001)
})

test_that("The next word is correctly predicted", {
    # The mode file name
    mfn <- paste0(mdir, "/def-model.RDS")
    # ModelPredictor class object is created
    mp <- ModelPredictor$new(mf = mfn)
    # The next word is predicted
    nws <- mp$predict_word("how are", count = 10)
    # Check that the most likely next word is "you"
    expect_equal(nws[["words"]][1], "you")
    # Check that the most likely next word has probability 0.571
    expect_equal(round(nws[["probs"]][1], 3), 0.571)
})

test_that("Perplexity is correctly calculated", {
    # The mode file name
    mfn <- paste0(mdir, "/def-model.RDS")
    # ModelPredictor class object is created
    mp <- ModelPredictor$new(mf = mfn)
    # The sentence whoose Perplexity is to be calculated
    l <- "last year at this time i was preparing for a trip to rome"
    # The line is split in to words
    w <- str_split(l, " ")[[1]]
    # The Perplexity of the sentence is calculated
    p <- mp$calc_perplexity(w)
    # Check that perplexity is correct
    expect_equal(p, 70)
})
