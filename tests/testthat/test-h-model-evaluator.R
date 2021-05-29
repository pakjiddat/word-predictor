test_that("Intrinsic evaluation works", {
    # The model file name
    mfn <- paste0(mdir, "/def-model.RDS")
    # The validation file name
    vfn <- paste0(mdir, "/validate.txt")

    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(mf = mfn)
    # The intrinsic evaluation is performed
    stats <- me$intrinsic_evaluation(lc = 20, fn = vfn)
    # The stats are rounded
    stats$mean <- round(stats$mean)
    # Check that mean Perplexity is correct
    expect_equal(stats$mean, 436)
    # Check that max Perplexity is correct
    expect_equal(stats$max, 1162)
    # Check that min Perplexity is correct
    expect_equal(stats$min, 69)
})

test_that("Extrinsic evaluation works", {
    # The model file name
    mfn <- paste0(mdir, "/def-model.RDS")
    # The validation file name
    vfn <- paste0(mdir, "/validate.txt")

    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(mf = mfn)
    # The intrinsic evaluation is performed
    stats <- me$extrinsic_evaluation(lc = 100, fn = vfn)
    # Check that percentage of valid predictions is correct
    expect_equal(stats$valid_perc, 5)
    # Check that percentage of invalid predictions is correct
    expect_equal(stats$invalid_perc, 95)
})


test_that("Performance evaluation works", {
    # The model file name
    mfn <- paste0(mdir, "/def-model.RDS")
    # The validation file name
    vfn <- paste0(mdir, "/validate.txt")

    # ModelEvaluator class object is created
    me <- ModelEvaluator$new(mf = mfn)
    # The performance evaluation is performed
    stats <- me$evaluate_performance(lc = 20, fn = vfn)
    # The Model object is read
    m <- readRDS(mfn)
    # Check that accuracy is correct
    expect_equal(m$pstats$a, 5)
    # Check that mean Perplexity is correct
    expect_equal(round(m$pstats$p), 436)
    # Check that time taken is less than 100 sec
    expect_lt(m$pstats$t, 100)
    # Check that memory used is less than 45 Mb
    expect_lt(as.numeric(m$pstats$m), (4.5 * 10^7))
})

test_that("Performance comparision works", {
    # ModelEvaluator class object is created
    me <- ModelEvaluator$new()
    # The performance evaluation is performed
    me$compare_performance(opts = list(
        "save_to" = "png",
        "mdir" = msdir,
        "dir" = sdir
    ))
    # Check that the performance image file exists
    expect_true(file.exists(paste0(sdir, "/performance.png")))
    # Check that the performance stats file exists
    expect_true(file.exists(paste0(sdir, "/pstats.RDS")))
})
