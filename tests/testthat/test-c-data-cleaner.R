test_that("A sample text file is cleaned successfully", {
    # The files to clean
    fns <- c("test", "validate")
    # Each file is cleaned
    for (fn in fns) {
        # The cleaned test file name
        cfn <- paste0(ddir2, "/", fn, "-clean.txt")
        # The test file name
        fn <- paste0(ddir2, "/", fn, ".txt")

        # If the test-clean file exists, then it is removed
        if (file.exists(cfn)) {
              file.remove(cfn)
          }

        # The data cleaner object is created
        dc <- DataCleaner$new(fn)
        # The sample file is cleaned
        dc$clean_file()

        # Check that the file exists
        expect_true(file.exists(cfn))
    }
})

test_that("Sample line of text are cleaned as expected", {
    # Test data is read
    l <- c(
        "If you think I’m wrong, send me a link to where it’s happened",
        "We’re about 90percent done with this room",
        '“This isn’t how I wanted it between us.”',
        "Almost any “cute” breed can become ornamental",
        "Once upon a time there was a kingdom with a castle…",
        "That's not a thing any of us are granted'",
        "“Why are you being so difficult?” she asks."
    )
    # The expected results
    res <- c(
        "if you think wrong send me a link to where its happened",
        "were about percent done with this room",
        "this how i wanted it between us",
        "almost any cute breed can become ornamental",
        "once upon a time there was a kingdom with a castle",
        "thats not a thing any of us are granted",
        "why are you being so difficult she asks"
    )
    # The DataCleaner object is created
    dc <- DataCleaner$new()
    # The line is cleaned
    cl <- dc$clean_lines(l)
    # The actual cleaned data is compared with expected data
    expect_equal(cl, res)
})
