test_that("Performance of transition probabilities generator is acceptable", {
    # Token generation for each ngram is checked
    for (i in 1:4) {
        time_taken <- system.time({
            memory_used <- mem_change({
                # The TPGenerator object is created
                tp <- TPGenerator$new()
                # The ngram number is set
                tp_opts$n <- i
                # The output format is set to object
                tp_opts$output_format <- "obj"
                # The transition probabilities are generated
                tp$generate_tp_for_n(tp_opts)
            });
        });
        # The memory used
        memory_used <- dc$format_size(memory_used)
        # The time taken is tested
        expect_lt(time_taken[[3]], 5)
        # The memory usage is tested
        expect_lt(memory_used, 5)
    }
});
