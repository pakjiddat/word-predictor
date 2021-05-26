#' The Model class represents n-gram models. An instance of the class is a
#' single n-gram model.
#'
#' @description
#' The attributes of this class are used to store n-gram model
#' information. The class provides methods for loading and saving the model.
#'
#' @details
#' The attributes of this class are used to store n-gram model
#' information such as model name, model description, model file name, n-gram
#' number, transition probabilities data, default probability, n-gram
#' configuration options such as data cleaning and tokenization options, word
#' list, model path, data directory path and performance stats. The model is
#' saved to a single file as a R object. A model file contains all the
#' information required by the model. The model object is used by other model
#' classes that perform operations on the model such as evaluation of model
#' performance, making word predictions based on the model and plotting model
#' performance stats.
Model <- R6::R6Class(
    "Model",
    inherit = TextFileProcessor,
    public = list(
        #' @field pstats The performance stats for the model.
        pstats = list(),
        #' @field name The model name.
        name = NULL,
        #' @field desc The model description.
        desc = NULL,

        #' @description
        #' It initializes the current object. It is used to set the
        #' maximum ngram number, sample size, input file name, data cleaner
        #' options, tokenization options, combined transition probabilities file
        #' name and verbose.
        #' @param name The model name.
        #' @param desc The model description.
        #' @param fn The model file name.
        #' @param df The path of the file used to generate the model. If
        #'   the data was cleaned, then df is the path to the cleaned
        #'   file.
        #' @param n The maximum ngram number supported by the model.
        #' @param ssize The sample size in Mb.
        #' @param ddir The data directory.
        #' @param mdir The model directory.
        #' @param dc_opts The data cleaner options.
        #' @param tg_opts The token generator options.
        #' @param ve If progress information should be displayed.
        #' @export
        initialize = function(name = NULL,
                              desc = NULL,
                              fn = NULL,
                              df = NULL,
                              n = 4,
                              ssize = 30,
                              ddir = "./data",
                              mdir = "./models",
                              dc_opts = list(),
                              tg_opts = list(),
                              ve = 0) {

            # The base class is initialized
            super$initialize(NULL, NULL, ve)

            # If the input file name is not given
            if (is.null(df)) {
                # The default training data file name
                df <- paste0(private$ddir, "/train.txt")
            }

            # If the output file name is not given
            if (is.null(fn)) {
                # The default output file name is used
                fn <- paste0(private$mdir, "/model.RDS")
            }

            # The path to the data file
            dfp <- paste0(ddir, "/", df)
            # If the data file does not exist, then an error is thrown
            if (!file.exists(dfp)) {
                  stop(paste0("The file: ", dfp, " does not exist !"))
              }
            # If the data directory does not exist, then an error is thrown
            if (!dir.exists(ddir)) {
                  stop(paste0("The dir: ", ddir, " does not exist !"))
              }
            # If the model directory does not exist, then an error is thrown
            if (!dir.exists(mdir)) {
                  stop(paste0("The dir: ", mdir, " does not exist !"))
              }
            # The dict words file is checked
            dc_opts[["dict_file"]] <- private$check_file(
                dc_opts[["dict_file"]], "dict-no-bad.txt"
            )

            # The model name is set
            self$name <- name
            # The model description is set
            self$desc <- desc
            # The ngram number is set
            private$n <- n
            # The sample size is set
            private$ssize <- ssize
            # The data directory name is set
            private$ddir <- ddir
            # The model directory name is set
            private$mdir <- mdir
            # The input file name is set
            private$df <- df
            # The word list file name is set
            private$wlf <- paste0(mdir, "/words.RDS")
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
            fn <- paste0(private$mdir, "/model-", private$n, ".RDS")
            # The tp file is read
            private$tp <- private$read_obj(fn)
            # The wl file is read
            private$wl <- private$read_obj(private$wlf)
            # The dictionary file name
            fn <- private$dc_opts[["dict_file"]]
            # The file contents
            dict <- private$read_file(fn, F)
            # The information message is shown
            private$display_msg("Calculating default probability...", 1)
            # The number of words in the dictionary file. It is used to
            # calculate Perplexity.
            vc <- length(dict)
            # The path to the input data file
            dfp <- paste0(private$ddir, "/", private$df)
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
        },

        #' @description
        #' It returns the given configuration data
        #' @param cn The name of the required configuration.
        #' @return The configuration value.
        get_config = function(cn) {
            # The required configuration value
            cv <- private[[cn]]

            return(cv)
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
        # @field n The maximum number of ngrams supported by the model.
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
        # * **n**. The ngram size.
        # * **save_ngrams**. If the ngram data should be saved.
        # * **min_freq**. All ngrams with frequency less than min_freq are
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
        # @field ssize The sample size in Mb.
        ssize = 30,
        # @field ddir The folder containing the data files
        ddir = "./data",
        # @field mdir The folder containing the model files
        mdir = "./models"
    )
)
