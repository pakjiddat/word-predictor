#' It generates ngrams of given size from an input text file
#'
#' @description
#' It generates ngram tokens along with their frequencies.
#'
#' @details
#' It provides a method for generating ngrams of given size. It saves
#' each ngram along with its frequency to a text file.
TokenGenerator <- R6::R6Class(
    "TokenGenerator",
    inherit = TextFileProcessor,
    public = list(
        #' @field tg_opts The options for the token generator obj.
        #'   n -> The ngram size.
        #'   save_ngrams -> If the ngram data should be saved.
        #'   min_freq -> All ngrams with frequency less than min_freq are
        #'     ignored.
        #'   line_count -> The number of lines to process at a time.
        #'   stem_words -> If words should be converted to their stem.
        #'   dir -> The dir where the output file should be saved.
        #'   format -> The format for the output. There are two options.
        #'     'plain' -> The data is stored in plain text.
        #'     'obj' -> The data is stored as a R obj.
        tg_opts = list(
            "n" = 1,
            "save_ngrams" = F,
            "min_freq" = -1,
            "line_count" = 5000,
            "stem_words" = F,
            "dir" = "./data/models",
            "format" = "obj"
        ),

        #' @description
        #' It initializes the current obj. It is used to set the file name
        #' and verbose options.
        #' @param file_name The path to the input file.
        #' @param opts The options for generating the ngram tokens.
        #'   n -> The ngram size.
        #'   save_ngrams -> If the ngram data should be saved.
        #'   min_freq -> All ngrams with frequency less than min_freq are
        #'     ignored.
        #'   line_count -> The number of lines to process at a time.
        #'   stem_words -> If words should be converted to their stem.
        #'   dir -> The dir where the output file should be saved.
        #'   format -> The format for the output. There are two options.
        #'     'plain' -> The data is stored in plain text.
        #'     'obj' -> The data is stored as a R obj.
        #' @param verbose Indicates if progress information should be displayed.
        initialize = function(file_name = "./data/models/validate-clean.txt",
                              opts = self$tg_opts,
                              verbose = 0) {
            # The given options are merged with the opts attribute
            self$tg_opts <- modifyList(self$tg_opts, opts)
            # The tg_opts is merged with the base class opts attribute
            self$opts <- modifyList(self$opts, self$tg_opts)
            # The base class is initialized
            super$initialize(file_name, self$opts[["line_count"]], verbose)
            # The processed output is initialized
            self$p_output <- NULL
        },

        #' @description
        #' It generates ngram tokens and their frequencies from the given file
        #' name. The tokens may be saved to a text file as plain text or a R
        #' obj.
        #' @return The ngram tokens along with their frequencies.
        generate_tokens = function() {
            # The processed output is initialized
            self$p_output <- NULL
            # The output file name
            fn <- private$get_file_name()
            # If the output file already exists
            if (file.exists(fn)) {
                # The information message
                msg <- paste0("The ", self$opts[["n"]],
                              "-gram file already exists")
                # The information message is shown
                self$display_msg(msg, 1)
                # If the ngram data should not be saved
                if (!self$opts[["save_ngrams"]]) {
                    # The ngrams file is read
                    self$p_output <- self$read_data(
                        fn, self$opts[["format"]], T)
                }
            }
            else {
                # The information message
                msg <- paste0("Generating ",
                              self$opts[["n"]], "-gram tokens...")
                # The information message is shown
                self$display_msg(msg, 1)
                # The base class process_file function is called
                super$process_file(super$pre_process, private$process,
                                   private$post_process)
            }
        }
    ),

    private = list(
        # @description
        # Performs processing for the \code{generate_tokens} function. It
        # processes the given line of text. It converts each line of text into
        # ngrams of the given size. The frequency of each ngram is updated.
        # @param lines The lines of text.
        process = function(lines) {
            # Ngrams are extracted from each line
            ngrams <- private$generate_ngrams(lines)
            # If the processed output is empty
            if (is.null(self$p_output)) {
                # The ngram words are set to the processed output
                self$p_output <- ngrams
            }
            else {
                # The ngram words are appended to the processed output
                self$p_output <- c(self$p_output, ngrams)
            }
        },

        # @description
        # It returns the name of the output ngram file.
        get_file_name = function() {
            # The ngram number
            n <- self$opts[["n"]]
            # The format
            fo <- self$opts[["format"]]
            # The output directory
            dir <- self$opts[["dir"]]
            # The file extension
            if (fo == "plain") ext <- ".txt"
            else ext <- ".RDS"

            # The file name
            file_name <- paste0(dir, "/n", n, ext)

            return(file_name)
        },

        # @description
        # It saves the ngram tokens and their frequencies to a text file.
        post_process = function() {
            # The information message
            msg <- paste0("Calculating ",
                          self$opts[["n"]], "-gram frequencies...")
            # The information message is shown
            self$display_msg(msg, 1)
            # The output is copied to a variable
            df <- data.frame("pre" = self$p_output)
            # A frequency column is added
            df$freq <- 1
            # Each prefix is grouped and summed
            df <- df %>% group_by(pre) %>% summarize_all(sum)
            # If the minimum ngram frequency is given
            if (self$opts[["min_freq"]] > -1) {
                # The information message
                msg <- paste0("Removing low frequency ngrams...")
                # The information message is shown
                self$display_msg(msg, 2)
                # All ngrams with frequency less than min_freq are ignored
                df <- df[df$freq >= self$opts[["min_freq"]], ]
            }
            # The column names are set
            colnames(df) <- c("pre", "freq")
            # The output is set to the updated variable
            self$p_output <- df
            # If the ngram data should be saved
            if (self$opts[["save_ngrams"]]) {
                # The required file name
                fn <- private$get_file_name()
                # The format
                fo <- self$opts[["format"]]
                # The n-gram data frame is written to file
                private$write_data(self$p_output, fn, fo, F)
            }
        },

        # @description
        # It generates ngram frequencies for the given lines of text.
        # @param lines The lines of text to process
        generate_ngrams = function(lines) {
            # The ngram number
            n <- self$opts[["n"]]
            # If n > 1
            if (n > 1) {
                # Trailing and leading white space is removed
                l <- trimws(lines, "both")
                # Start and end of sentence tags are added
                l  <- gsub("(^)(.+)($)", "<s>\\2<e>", l)
                # The lines are split on space
                w <- strsplit(l, " ")
                # The words are converted to an atomic vector
                w <- unlist(w)
                # The index of empty words
                i <- (w == "")
                # The empty words are removed
                w <- w[!i]
                # The indexes for the words
                indexes <- 1:length(w)
                # The ngrams are generated
                l <- sapply(indexes, function(i) {
                    # If the words should be stemmed
                    if (self$tg_opts[["stem_words"]]) {
                        # The ngram prefix words are stemmed. The next word is
                        # not stemmed
                        v <- c(wordStem(w[i:(i+n-2)]), w[(i+n-1)])
                    }
                    else {
                        # The ngram token
                        v <- w[i:(i+n-1)]
                    }
                    # The ngram token
                    v <- paste0(v, collapse = "_")
                    # The ngram token is returned
                    return(v)
                },
                simplify = T)
                # Invalid ngrams need to be removed
                # A logical vector indicating position of invalid ngrams
                i <- grepl(".+<e>.+", l)
                # The list of valid ngrams
                l <- l[!i]
                # The start of sentence tokens are removed
                l <- gsub("<s>", "", l)
                # The end of sentence tokens are removed
                l <- gsub("<e>", "", l)
            }
            else {
                # The line is split on " "
                words <- strsplit(lines, " ")
                # The list of words is converted to atomic vector
                l <- unlist(words)
                # The index of empty words
                i <- l == ""
                # The empty words are removed
                l <- l[!i]
            }

            return(l)
        }
    )
)
