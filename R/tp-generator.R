#' It is used to generate the transition probabilities
#'
#' @description
#' It implements Markov Chains for ngrams. It generates transition
#' probabilities for ngrams.
#'
#' @details
#' It reads ngram frequencies from an input text file. It parses each
#' ngram into a prefix, a next word and the next word frequency. The prefix is
#' converted to a numeric hash using the digest2int function from the digest
#' package. The next word is replaced with the position of the next word in the
#' list of all words. The data is stored in a data frame. It may be saved to a
#' file.
TPGenerator <- R6::R6Class(
    "TPGenerator",
    inherit = TextFileProcessor,
    public = list(
        #' @description
        #' It initializes the current obj. It is used to set the verbose option.
        #' @param verbose If progress information should be displayed.
        initialize = function(verbose = 0) {
            # The base class is initialized
            super$initialize(NULL, NULL, verbose)
            # The processed output is initialized
            self$p_output <- data.frame()
        },

        #' @description
        #' It groups the given tp data by prefix. Each prefix has
        #' the top c next words with the highest probabilities. All other next
        #' words are removed. The trimmed tp data may be saved to a file or
        #' returned. The file is saved with the suffix -min.
        #' e.g model-4-min.RDS.
        #' @param opts The options for trimming the tp data.
        #'   save_tp -> If the data should be saved.
        #'   dir -> The dir where the output file should be saved.
        #'   c -> The top c next words per ngrams prefix.
        #'   m -> The maximum ngram number supported by the model.
        trim_tp = function(opts) {
            # The model file name
            fn <- paste0(opts$dir, "/model-", opts$m, ".RDS")
            # The model file is read to a data frame
            df <- self$read_obj(fn)
        },

        #' @description
        #' It generates the transition probabilities for the given
        #' ngram numbers It first generates the transition probabilities for
        #' each ngram number. The transition probabilities are then combined
        #' into a single data frame. The data frame may be saved to a file.
        #' @param opts The options for generating the transition probabilities.
        #'   save_tp -> If the data should be saved.
        #'   n_range -> The range of ngram numbers.
        #'   dir -> The dir where the output file should be saved.
        #'   format -> The format for the output. There are two options.
        #'     'plain' -> The data is stored in plain text.
        #'     'obj' -> The data is stored as a R obj.
        generate_tp = function(opts) {
            # The opts is merged with the tp_opts attribute
            private$tp_opts = modifyList(private$tp_opts, opts)
            # The tp_opts is merged with the base class opts attribute
            self$opts = modifyList(self$opts, private$tp_opts)
            # The information message
            msg <- paste0(
                "Generating Transition Probabilities for n = ",
                min(self$opts[["n_range"]]),
                ":",
                max(self$opts[["n_range"]])
            )
            # Information message is shown
            self$display_msg(msg, 1)
            # The processed output is cleared
            self$p_output <- data.frame()
            # The ngram number range
            n_range <- self$opts[["n_range"]]
            # The minimum ngram number
            nmin <- min(n_range)
            # The maximum ngram number
            nmax <- max(n_range)
            # The output format
            fo <- self$opts[["format"]]
            # The file extension
            if (fo == "plain") ext <- ".txt"
            else ext <- ".RDS"
            # The options for generating transition probabilities
            tp_opts <- list(
                n = 1,
                format = fo,
                save_tp = T,
                dir = self$opts[["dir"]]
            )
            # The combined tp data
            c_pre <- c_nw <- c_prob <- c()
            # For each ngram number, the transition probabilities data is
            # generated.
            for (n in n_range) {
                # The value of n is set
                tp_opts$n <- n
                # The transition probabilities or word list is generated
                self$generate_tp_for_n(tp_opts)
                # If n == 1, then word list data is saved
                if (n == 1) {
                    # The combined tp data is saved
                    private$save_data()
                }
                else {
                    # c_pre is updated
                    c_pre <- c(c_pre, self$p_output$pre)
                    # c_nw is updated
                    c_nw <- c(c_nw, self$p_output$nw)
                    # c_prob is updated
                    c_prob <- c(c_prob, self$p_output$prob)
                    # The processed output is cleared
                    self$p_output <- data.frame()
                }
            }
            # The processed output is set to the combined tp data
            self$p_output <-
                data.frame("pre" = c_pre,
                           "nw" = c_nw,
                           "prob" = c_prob)
            # The combined tp data is saved
            private$save_data(paste0("model-", nmax, ext))
        },

        #' @description
        #' It reads ngram token frequencies from an input text
        #' file. It generates a data frame containing the prefix, next word
        #' and next word frequency. The data frame may be saved to a file as
        #' plain text or as a R obj. For n = 1, the list of words is saved.
        #' @param opts The options for generating the transition probabilities.
        #'   save_tp -> If the data should be saved.
        #'   n -> The ngram number
        #'   dir -> The location of  the input and output files.
        #'   format -> The format of the input and output files. Options are:
        #'     'plain' -> The data is stored in plain text.
        #'     'obj' -> The data is stored as a R obj.
        generate_tp_for_n = function(opts) {
            # The opts is merged with the tp_opts attribute
            private$tp_opts = modifyList(private$tp_opts, opts)
            # The tp_opts is merged with the base class opts attribute
            self$opts = modifyList(self$opts, private$tp_opts)
            # The output format
            fo <- self$opts[["format"]]
            # The ngram number
            n <- self$opts[["n"]]
            # The output file name
            fn <- private$get_file_name(T)
            # If the output file already exists
            if (file.exists(fn)) {
                # The information message
                msg <- paste0("The file: ", fn, " already exists")
                # The information message is shown
                self$display_msg(msg, 1)
                # The file is read
                data <- private$read_data(fn, fo, T)
                # If n = 1
                if (n == 1) {
                    # The word list is set to the data
                    private$wl <- data
                }
                else {
                    # The processed output is set to the data
                    self$p_output <- data
                }
            }
            else {
                # The information message
                msg <- paste0("Generating Transition Probabilities for n=", n)
                # Information message is shown
                self$display_msg(msg, 1)

                # The input file name
                self$file_name <- private$get_file_name(F)
                # The data is read
                df <- private$read_data(self$file_name, fo, T)
                # If n = 1
                if (n == 1) {
                    # The word list is set to the data frame
                    private$wl <- df
                    # A probabilities column is added
                    private$wl$prob = (private$wl$freq/sum(private$wl$freq))
                    # The probabilities are rounded to 8 decimal places
                    private$wl$prob = round(private$wl$prob, 8)
                    # The frequency column is removed
                    private$wl$freq <- NULL
                }
                else {
                    # The 1-gram words are read
                    private$read_words()
                    # The lines are split on "prefix_nextword:frequency"
                    m <- str_match(df$pre, "(.+)_(.+)")
                    # The hash of the prefix is taken
                    np <- digest2int(m[,2])
                    # The next word id based on index position
                    nw <- match(m[,3], private$wl$pre)
                    # The next word frequencies
                    nf <- df$freq
                    # The data is added to a data frame
                    df <- data.frame("pre" = np,
                                     "nw" = nw,
                                     "freq" = nf)
                    # The processed output is set to the data frame
                    self$p_output <- df
                    # The next word probabilities are generated
                    private$generate_probs()
                    # The frequency column is removed
                    self$p_output$freq <- NULL
                    # If the data should be saved
                    if (self$opts[["save_tp"]]) {
                        private$save_data()
                    }
                }
            }
        }
    ),

    private = list(
        # @field tp_opts The options for generating the transition
        #   probabilities.
        #   save_tp -> If the data should be saved.
        #   n -> The ngram number
        #   dir -> The dir where the output file should be saved.
        #   format -> The format for the output. There are two options.
        #     'plain' -> The data is stored in plain text.
        #     'obj' -> The data is stored as a R obj.
        tp_opts = list(
            "save_tp" = F,
            "n" = 1,
            "dir" = "./data/models",
            "format" = "obj"
        ),

        # @field The list of unique words and their frequencies
        wl = data.frame(),

        # @description
        # It calculates the next word probabilities and optionally
        # saves the transition probability data to a file.
        generate_probs = function() {
            # Information message is shown
            self$display_msg("Generating Transition Probabilities...", 1)
            # The ngram number
            n <- self$opts[["n"]]
            # If n > 1
            if (n > 1) {
                # The output is copied to a variable
                df <- self$p_output
                # A new probability column is added. It is set to the sum of
                # frequency column for each prefix group.
                df <- df %>%
                    group_by(pre) %>%
                    mutate(prob = sum(freq))
                # Each frequency is divided by the sum to give the probability.
                df$prob <- round(df$freq/df$prob, 8)
                # The output is set to the updated variable
                self$p_output <- df
            }
        },

        # @description
        # It returns the name of the output or input file.
        # @param is_output If the output file name is required.
        get_file_name = function(is_output) {
            # The ngram number
            n <- self$opts[["n"]]
            # The directory
            od <- self$opts[["dir"]]
            # The format
            fo <- self$opts[["format"]]
            # The file extension
            if (fo == "plain") ext <- ".txt"
            else ext <- ".RDS"
            # If the output file name is required
            if (is_output) {
                # If n = 1
                if (n == 1) {
                    # The file name
                    fn <- paste0(od, "/words", ext)
                }
                # If n > 1
                else if (n > 1) {
                    # The file name
                    fn <- paste0(od, "/tp", n, ext)
                }
            }
            else {
                # The file name
                fn <- paste0(od, "/n", n, ext)
            }

            return(fn)
        },

        # @description
        # It saves the transition probabilities or word list
        # depending on the n options, to a file in plain format or as a R obj.
        # If the file name is not given, then it is generated using the current
        # object attributes.
        # @param file_name The file name to use.
        save_data = function(file_name = NULL) {
            # The ngram number
            n <- self$opts[["n"]]
            # The directory
            od <- self$opts[["dir"]]
            # The format
            fo <- self$opts[["format"]]
            # If n = 1
            if (n == 1) {
                # The data to save
                data <- private$wl
            }
            # If n > 1
            else if (n > 1) {
                # The data to save
                data <- self$p_output
            }
            # If the file name is given as parameter then it is used
            if (!is.null(file_name)) fn <- paste0(od, "/", file_name)
            else fn <- private$get_file_name(T)
            # The data is written
            private$write_data(data, fn, fo, F)
        },

        # @description
        # It reads the list of 1-gram words.
        read_words = function() {
            # If the word list has not been read
            if (nrow(private$wl) == 0) {
                # The format
                fo <- self$opts[["format"]]
                # The file extension
                if (fo == "plain") ext <- ".txt"
                else ext <- ".RDS"
                # The 1-gram words file name
                fn <- paste0(self$opts[["dir"]], "/words", ext)
                # The words are read
                private$wl <- private$read_data(fn, self$opts[["format"]], F)
            }
        }
    )
)
