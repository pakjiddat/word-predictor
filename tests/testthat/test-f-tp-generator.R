test_that("The combined transition probabilities file is correctly generated", {
    # The list of output files
    fns <- c("words", "model-4", "tp2", "tp3", "tp4")
    # Each file is removed if it exists
    for (fn in fns) {
        # The file name
        fn <- paste0(ddir2, "/", fn, ".RDS")
        # If the file exists
        if (file.exists(fn)) {
            # The file is removed
            file.remove(fn)
        }
    }

    # The TPGenerator object is created
    tp <- TPGenerator$new(opts = list(n = 4, dir = ddir2))
    # The combined transition probabilities are generated
    tp$generate_tp()
    # Each file is checked
    for (fn in fns) {
        # The file name
        fn <- paste0(ddir2, "/", fn, ".RDS")
        # Check if the file exists
        expect_true(file.exists(fn), label = fn)
    }
});
