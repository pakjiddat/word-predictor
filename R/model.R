#' Represents n-gram models
#'
#' @description
#' The Model class represents n-gram models. An instance of the class is a
#' single n-gram model. The attributes of this class are used to store n-gram
#' model information. The class provides methods for loading and saving the
#' model.
#'
#' @details
#' The attributes of this class are used to store n-gram model information such
#' as model name, model description, model file name, n-gram size, transition
#' probabilities data, default probability for words, data cleaning and
#' tokenization options, word list, model path, data directory path and
#' performance stats. The model is saved to a single file as a R object.
#'
#' A model file contains all the information required by the model. The model
#' object is used as input by classes that perform operations on the model such
#' as evaluation of model performance, text predictions and comparison of model
#' performance.
Model <- R6::R6Class(
    "Model",
    inherit = Base,
    public = list(
        #' @field pstats The performance stats for the model.
        pstats = list(),
        #' @field name The model name.
        name = NULL,
        #' @field desc The model description.
        desc = NULL,

        #' @description
        #' It initializes the current object. It is used to set the
        #' maximum n-gram number, sample size, input file name, data cleaner
        #' options, tokenization options, combined transition probabilities file
        #' name and verbose.
        #' @param name The model name.
        #' @param desc The model description.
        #' @param fn The model file name.
        #' @param df The name of the file used to generate the model.
        #' @param n The maximum n-gram number supported by the model.
        #' @param ssize The sample size as a proportion of the input file.
        #' @param dir The directory containing the model files.
        #' @param dc_opts The data cleaner options.
        #' @param tg_opts The token generator options.
        #' @param ve The level of detail in the information messages.
        #' @export
        initialize = function(name = NULL,
                              desc = NULL,
                              fn = NULL,
                              df = NULL,
                              n = 4,
                              ssize = 0.3,
                              dir = ".",
                              dc_opts = list(),
                              tg_opts = list(),
                              ve = 0) {

            # The base class is initialized
            super$initialize(NULL, NULL, ve)

            # If the output file name is not given
            if (is.null(fn)) {
                # Error message is shown
                private$dm("Output file name was not given", md = -1, ty = "e")
            }

            # The path to the data file
            dfp <- paste0(dir, "/", df)
            # If the data file does not exist, then an error is thrown
            if (!file.exists(dfp)) {
                # Error message is shown
                private$dm("Invalid input file: ", dfp, md = -1, ty = "e")
            }
            # If the directory does not exist, then an error is thrown
            if (!dir.exists(dir)) {
                private$dm(
                    "The dir: ", dir, " does not exist !",
                    md = -1, ty = "e"
                )
            }

            # An object of class EnvManager is created
            em <- EnvManager$new(ve)
            # The dict words file is checked
            dc_opts[["dict_file"]] <- em$get_data_fn(
                dc_opts[["dict_file"]], "dict-no-bad.txt"
            )

            # The model name is set
            self$name <- name
            # The model description is set
            self$desc <- desc
            # The n-gram number is set
            private$n <- n
            # The sample size is set
            private$ssize <- ssize
            # The directory name is set
            private$dir <- dir
            # The input file name is set
            private$df <- df
            # The word list file name is set
            private$wlf <- paste0(dir, "/words.RDS")
            # The model file name is set
            private$fn <- fn
            # If the dc_opts are given
            if (length(dc_opts) > 0) {
                # The custom dc_opts are merged with the default dc_opts
                private$dc_opts <- modifyList(private$dc_opts, dc_opts)
            }
            # If the tg_opts are given
            if (length(tg_opts) > 0) {
                # The custom tg_opts are merged with the default tg_opts
                private$tg_opts <- modifyList(private$tg_opts, tg_opts)
            }
        },

        #' @description
        #' It loads the model using the given information
        load_model = function() {
            # The tp file name
            fn <- paste0(private$dir, "/model-", private$n, ".RDS")
            # The tp file is read
            private$tp <- private$read_obj(fn)
            # The wl file is read
            private$wl <- private$read_obj(private$wlf)
            # The dictionary file name
            fn <- private$dc_opts[["dict_file"]]
            # The file contents
            dict <- private$read_file(fn, F)
            # The information message is shown
            private$dh("Calculating default probability", "-", md = 1)
            # The number of words in the dictionary file. It is used to
            # calculate Perplexity.
            vc <- length(dict)
            # The path to the input data file
            dfp <- paste0(private$dir, "/", private$df)
            # The data file is read
            data <- private$read_file(dfp, F)
            # The words are split on " "
            w <- strsplit(data, " ")
            # The words are converted to atomic list
            w <- unlist(w)
            # The number of words
            n <- length(w)
            # The default probability is set
            private$dp <- 1 / (n + vc)
            # The information message is shown
            private$dh("DONE", "=", md = 1)
        },

        #' @description
        #' It returns the given configuration data
        #' @param cn The name of the required configuration.
        #' @return The configuration value.
        get_config = function(cn) {
            # The required configuration value
            cv <- private[[cn]]

            return(cv)
        },

        #' @description
        #' It returns the size of the current object. The object
        #' size is calculated as the sum of sizes of the object attributes.
        #' @return The size of the object in bytes.
        get_size = function() {
            # The required object size
            s <- 0
            # The tp size is added
            s <- s + as.numeric(object.size(private$tp))
            # The wl size is added
            s <- s + as.numeric(object.size(private$wl))
            # The dc_opts size is added
            s <- s + as.numeric(object.size(private$dc_opts))
            # The tg_opts size is added
            s <- s + as.numeric(object.size(private$tg_opts))
            # The pstats size is added
            s <- s + as.numeric(object.size(self$pstats))
            return(s)
        }
    ),
    private = list(
        # @field fn The path to the model file.
        fn = NULL,
        # @field wlf The path to the word list file.
        wlf = NULL,
        # @field df The short name of the input file.
        df = NULL,
        # @field tp The transition probabilities data frame.
        tp = NULL,
        # @field wl The list of unique words.
        wl = NULL,
        # @field dp The default probability is equal to 1/(N+V), where N is the
        #   number of words in the sentence, V is the number of words in the
        #   vocabulary.
        dp = NULL,
        # @field n The maximum number of n-grams supported by the model.
        n = 4,
        # @field dc_opts The options for the data cleaner object.
        # * **min_words**. The minimum number of words per sentence.
        # * **line_count**. The number of lines to read and clean at a time.
        # * **sw_file**. The stop words file path.
        # * **dict_file**. The dictionary file path.
        # * **bad_file**. The bad words file path.
        # * **to_lower**. If the words should be converted to lower case.
        # * **remove_stop**. If stop words should be removed.
        # * **remove_punct**. If punctuation symbols should be removed.
        # * **remove_non_dict**. If non dictionary words should be removed.
        # * **remove_non_alpha**. If non alphabet symbols should be removed.
        # * **remove_extra_space**. If leading, trailing and double spaces
        #     should be removed.
        # * **remove_bad**. If bad words should be removed
        dc_opts = list(
            "min_words" = 2,
            "line_count" = 1000,
            "sw_file" = NULL,
            "dict_file" = NULL,
            "bad_file" = NULL,
            "to_lower" = T,
            "remove_stop" = F,
            "remove_punc" = T,
            "remove_non_dict" = T,
            "remove_non_alpha" = T,
            "remove_extra_space" = T,
            "remove_bad" = F
        ),
        # @field tg_opts The options for the token generator obj.
        # * **n**. The n-gram size.
        # * **save_ngrams**. If the n-gram data should be saved.
        # * **min_freq**. All n-grams with frequency less than min_freq are
        #     ignored.
        # * **line_count**. The number of lines to process at a time.
        # * **stem_words**. If words should be converted to their stem.
        # * **dir**. The dir where the output file should be saved.
        # * **format**. The format for the output. There are two options.
        # ** **plain**. The data is stored in plain text.
        # ** **obj**. The data is stored as a R obj.
        tg_opts = list(
            "min_freq" = -1,
            "n" = 1,
            "save_ngrams" = T,
            "min_freq" = -1,
            "line_count" = 5000,
            "stem_words" = F,
            "dir" = "./data/models",
            "format" = "obj"
        ),
        # @field ssize The sample size as a proportion of the input file.
        ssize = 0.3,
        # @field dir The folder containing the model related files.
        dir = "./data"
    )
)
