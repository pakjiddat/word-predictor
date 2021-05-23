test_that("Performance of token generating function is acceptable", {
    # The test file name
    fn <- paste0(ddir2, "/test-clean.txt")
    # Token generation for each ngram is checked
    for (i in 1:4) {
        # The output ngram file name
        tfn <- paste0(ddir2, "/n", i, ".RDS")
        # If the file exists
        if (file.exists(tfn)) {
            # The file is removed
            file.remove(tfn)
        }
        # The ngram number is set
        tg_opts = list("n" = i, "save_ngrams" = T, "dir" = ddir2)
        # The TokenGenerator object is created
        tg <- TokenGenerator$new(fn, tg_opts)
        # The ngram tokens are generated
        tg$generate_tokens()
        # The existance of the file is checked
        expect_true(file.exists(tfn))
    }
});

test_that("Frequency of the generated tokens is calculated correctly", {
    # Each ngram file is checked
    for (n in 1:4) {
        # The ngram file name
        fn <- paste0(ddir2, "/n", n, ".RDS")
        # The ngram file is read
        df <- readRDS(fn)
        # The input file name
        fn <- paste0(ddir2, "/test-clean.txt")
        # The file connection
        con <- file(fn)
        # The original input text file is read
        lines <- readLines(con)
        # The file connection is closed
        close(con)
        # The word frequencies
        freq <- df$freq
        # The ngram token prefixes
        words <- df$pre
        # The "_" is replaced with " "
        words <- gsub("_", " ", words)
        # 20 random samples are taken
        indexes <- sample(1:20, 20)
        # The words to check
        words <- words[indexes]
        # The frequencies to check
        freq <- freq[indexes]
        # The frequency of each word is checked
        for (i in 1:length(words)) {
            # The regular expression for matching the word
            r <- stringr:::regex(paste0("\\b", words[i], "\\b"), uword = T)
            # The number of occurances of the word
            count <- sum(str_count(lines, r))
            # The number of word occurances should match the frequency
            expect_equal(count, freq[i], label = paste0("word: ", words[i]))
        }
    }
});
