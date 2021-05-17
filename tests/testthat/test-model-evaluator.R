test_that("Intrinsic evaluation works", {
    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(mf = "./data/model/def-model.RDS")
    # The intrinsic evaluation is performed
    stats <- me$intrinsic_evaluation(lc = 20, fn = "./data/model/validate.txt")
    # Check that mean Perplexity is correct
    expect_equal(stats$mean, 19.1)
    # Check that max Perplexity is correct
    expect_equal(stats$max, 110)
    # Check that min Perplexity is correct
    expect_equal(stats$min, 2)
})

test_that("Extrinsic evaluation works", {
    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(mf = "./data/model/def-model.RDS")
    # The intrinsic evaluation is performed
    stats <- me$extrinsic_evaluation(lc = 100, fn = "./data/model/validate.txt")
    # Check that percentage of valid predictions is correct
    expect_equal(stats$valid_perc, 74)
    # Check that percentage of invalid predictions is correct
    expect_equal(stats$invalid_perc, 26)
})


test_that("Performance evaluation works", {
    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(mf = "./data/model/def-model.RDS")
    # The performance evaluation is performed
    stats <- me$evaluate_performance(lc = 20, fn = "./data/model/validate.txt")
    # The Model object is read
    m <- readRDS("./data/model/def-model.RDS")
    # Check that accuracy is correct
    expect_equal(m$pstats$a, 85)
    # Check that mean Perplexity is correct
    expect_equal(m$pstats$p, 19.1)
    # Check that time taken is less than 40 sec
    expect_lt(m$pstats$t, 40)
    # Check that memory used is less than 45 Mb
    expect_lt(as.numeric(m$pstats$m), (4.5*10^7))
})
