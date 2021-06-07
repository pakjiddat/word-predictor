test_that("Intrinsic evaluation works", {
    # The required files
    wp$rf <- c("def-model.RDS", "validate-clean.txt")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The model file name
    mfn <- paste0(ed, "/def-model.RDS")
    # The validation file name
    vfn <- paste0(ed, "/validate-clean.txt")

    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(mf = mfn, ve = wp$ve)
    # The intrinsic evaluation is performed
    stats <- me$intrinsic_evaluation(lc = 20, fn = vfn)
    # The stats are rounded
    stats$mean <- round(stats$mean)
    # Check that mean Perplexity is correct
    expect_equal(stats$mean, 2297)
    # Check that max Perplexity is correct
    expect_equal(stats$max, 8248)
    # Check that min Perplexity is correct
    expect_equal(stats$min, 282)

    # The cleanup action is performed
    source("./cu.R")
})

test_that("Extrinsic evaluation works", {
    # The required files
    wp$rf <- c("def-model.RDS", "validate-clean.txt")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The model file name
    mfn <- paste0(ed, "/def-model.RDS")
    # The validation file name
    vfn <- paste0(ed, "/validate-clean.txt")

    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(mf = mfn, ve = wp$ve)
    # The intrinsic evaluation is performed
    stats <- me$extrinsic_evaluation(lc = 100, fn = vfn)
    # Check that percentage of valid predictions is correct
    expect_equal(round(stats$valid_perc, 2), 1.33)
    # Check that percentage of invalid predictions is correct
    expect_equal(round(stats$invalid_perc, 1), 98.7)

    # The cleanup action is performed
    source("./cu.R")
})


test_that("Performance evaluation works", {
    # The required files
    wp$rf <- c("def-model.RDS", "validate-clean.txt")
    options("wordpredictor" = wp)
    source("./inc.R")

    # The model file name
    mfn <- paste0(ed, "/def-model.RDS")
    # The validation file name
    vfn <- paste0(ed, "/validate-clean.txt")

    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(mf = mfn, ve = wp$ve)
    # The performance evaluation is performed
    stats <- me$evaluate_performance(lc = 20, fn = vfn)
    # The Model object is read
    m <- readRDS(mfn)
    # Check that accuracy is correct
    expect_equal(m$pstats$a, 0)
    # Check that mean Perplexity is correct
    expect_equal(round(m$pstats$p), 2297)
    # Check that time taken is less than 5 sec
    expect_lt(m$pstats$t, 5)
    # Check that memory used is less than 1 Mb
    expect_lt(as.numeric(m$pstats$m), 10^6)

    # The cleanup action is performed
    source("./cu.R")
})

test_that("Performance comparision works", {

    # The required files
    wp$rf <- c("def-model.RDS")
    options("wordpredictor" = wp)
    source("./inc.R")

    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(ve = wp$ve)
    # The performance evaluation is performed
    me$compare_performance(opts = list(
        "save_to" = "png",
        "dir" = ed
    ))
    # The path to the image file
    ifn <- paste0(ed, "/performance.png")
    # The path to the stats file
    sfn <- paste0(ed, "/pstats.RDS")
    # Check that the performance image file exists
    expect_true(file.exists(ifn))
    # Check that the performance stats file exists
    expect_true(file.exists(sfn))

    # The cleanup action is performed
    source("./cu.R")
})
