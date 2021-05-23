#' It generates ngrams of given size from an input text file
#'
#' @description
#' It generates ngram tokens along with their frequencies. The data
#' may be saved to a file in plain text format or as a R object.
#'
#' @importFrom SnowballC wordStem
#' @importFrom dplyr group_by summarize_all %>%
TokenGenerator <- R6::R6Class(
    "TokenGenerator",
    inherit = TextFileProcessor,
    public = list(
        #' @description
        #' It initializes the current obj. It is used to set the file name,
        #' tokenization options and verbose option.
        #' @param fn The path to the input file.
        #' @param opts The options for generating the ngram tokens.
        #' * **n**. The ngram size.
        #' * **save_ngrams**. If the ngram data should be saved.
        #' * **min_freq**. All ngrams with frequency less than min_freq are
        #'     ignored.
        #' * **line_count**. The number of lines to process at a time.
        #' * **stem_words**. If words should be transformed to their stems.
        #' * **dir**. The dir where the output file should be saved.
        #' * **format**. The format for the output. There are two options.
        #'     * **plain**. The data is stored in plain text.
        #'     * **obj**. The data is stored as a R obj.
        #' @param ve Indicates if progress information should be displayed.
        #' @export
        initialize = function(fn = NULL, opts = list(), ve = 0) {
            # The given options are merged with the opts attribute
            private$tg_opts <- modifyList(private$tg_opts, opts)
            # The base class is initialized
            super$initialize(fn, private$tg_opts$line_count, ve)
            # The processed output is initialized
            private$p_output <- NULL
        },

        #' @description
        #' It generates ngram tokens and their frequencies from the
        #' given file name. The tokens may be saved to a text file as plain text
        #' or a R object.
        #' @return The data frame containing ngram tokens along with their
        #'   frequencies.
        generate_tokens = function() {
            # The processed output is initialized
            private$p_output <- NULL
            # The output file name
            fn <- private$get_file_name()
            # If the output file already exists
            if (file.exists(fn)) {
                # The information message
                msg <- paste0("The ", private$tg_opts[["n"]],
                              "-gram file already exists")
                # The information message is shown
                private$display_msg(msg, 1)
                # If the ngram data should not be saved
                if (!private$tg_opts[["save_ngrams"]]) {
                    # The ngrams file is read
                    private$p_output <- private$read_data(
                        fn, private$tg_opts[["format"]], T)
                }
            }
            else {
                # The information message
                msg <- paste0("Generating ",
                              private$tg_opts[["n"]], "-gram tokens...")
                # The information message is shown
                private$display_msg(msg, 1)
                # The base class process_file function is called
                private$process_file(private$pre_process, private$process,
                                   private$post_process)
            }
        }
    ),

    private = list(
        # @field tg_opts The options for the token generator obj.
        # * **n**. The ngram size.
        # * **save_ngrams**. If the ngram data should be saved.
        # * **min_freq**. All ngrams with frequency less than min_freq are
        #     ignored.
        # * **stem_words**. If words should be transformed to their stems.
        # * **line_count**. The number of lines to process at a time.
        # * **dir**. The dir where the output file should be saved.
        # * **format**. The format for the output. There are two options.
        #     * **plain**. The data is stored in plain text.
        #     * **obj**. The data is stored as a R obj.
        tg_opts = list(
            "n" = 1,
            "save_ngrams" = F,
            "stem_words" = F,
            "min_freq" = -1,
            "line_count" = 5000,
            "dir" = "./data/model",
            "format" = "obj"
        ),

        # @description
        # Performs processing for the \code{generate_tokens} function. It
        # processes the given line of text. It converts each line of text into
        # ngrams of the given size. The frequency of each ngram is updated.
        # @param lines The lines of text.
        process = function(lines) {
            # Ngrams are extracted from each line
            ngrams <- private$generate_ngrams(lines)
            # The ngram words are appended to the processed output
            private$p_output <- c(private$p_output, ngrams)
        },

        # @description
        # It returns the name of the output ngram file.
        get_file_name = function() {
            # The ngram number
            n <- private$tg_opts[["n"]]
            # The format
            fo <- private$tg_opts[["format"]]
            # The output directory
            dir <- private$tg_opts[["dir"]]
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
                          private$tg_opts[["n"]], "-gram frequencies...")
            # The information message is shown
            private$display_msg(msg, 1)
            # The output is copied to a variable
            df <- data.frame("pre" = private$p_output)
            # A frequency column is added
            df$freq <- 1
            # Each prefix is grouped and summed
            df <- df %>% group_by(pre) %>% summarize_all(sum)
            # If the minimum ngram frequency is given
            if (private$tg_opts[["min_freq"]] > -1) {
                # The information message
                msg <- paste0("Removing low frequency ngrams...")
                # The information message is shown
                private$display_msg(msg, 2)
                # All ngrams with frequency less than min_freq are ignored
                df <- df[df$freq >= private$tg_opts[["min_freq"]], ]
            }
            # The column names are set
            colnames(df) <- c("pre", "freq")
            # The output is set to the updated variable
            private$p_output <- df
            # If the ngram data should be saved
            if (private$tg_opts[["save_ngrams"]]) {
                # The required file name
                fn <- private$get_file_name()
                # The format
                fo <- private$tg_opts[["format"]]
                # The n-gram data frame is written to file
                private$write_data(private$p_output, fn, fo, F)
            }
            # If ngram data should not be saved
            else {
                return(private$p_output)
            }
        },

        # @description
        # It generates ngram frequencies for the given lines of text.
        # @param lines The lines of text to process
        generate_ngrams = function(lines) {
            # The ngram number
            n <- private$tg_opts[["n"]]
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
                    if (private$tg_opts[["stem_words"]]) {
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
