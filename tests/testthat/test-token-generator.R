test_that("Performance of token generating function is acceptable", {
    # Token generation for each ngram is checked
    for (i in 1:4) {
        time_taken <- system.time({
            memory_used <- mem_change({
                # The ngram number is set
                tg_opts = list("n" = i)
                # The TokenGenerator object is created
                tg <- TokenGenerator$new("./data/models/test-clean.txt", tg_opts)
                # The ngram tokens are generated
                tg$generate_tokens()
            });
        });
        # The memory used
        memory_used <- dc$format_size(memory_used)
        # The time taken is tested
        expect_lt(time_taken[[3]], 20)
        # The memory usage is tested
        expect_lt(memory_used, 10)
    }
});

test_that("Frequency of the generated tokens is calculated correctly", {
    # The TokenGenerator object is created
    tg <- TokenGenerator$new("./data/models/test-clean.txt")
    # Each ngram file is checked
    for (n in 1:4) {
        # The ngram file name
        fn <- paste0("./data/models/n", n, ".txt")
        # The ngram file is read
        df <- tg$read_data(fn, "plain", T)
        # The input file name
        fn <- "./data/models/test-clean.txt"
        # The original input text file is read
        lines <- tg$read_data(fn, "plain", F)
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
            r <- regex(paste0("\\b",words[i],"\\b"), uword = T)
            # The number of occurances of the word
            count <- sum(str_count(lines, r))
            # The number of word occurances should match the frequency
            expect_equal(count, freq[i], label = paste0("word: ", words[i]))
        }
    }
});
