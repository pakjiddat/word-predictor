#' It is used to generate n-gram models for the given data file.
#'
#' @description
#' It provides methods that are used for generating n-gram models.
#' The n-gram models may be customized by specifying the data cleaning and
#' tokenization options.
#'
#' @details
#' It provides a method that generates a n-gram model. The n-gram model
#' may be customized by specifying the data cleaning and tokenization options.
#' The data cleaning options include removal of punctuation, stop words, extra
#' space, non-dictionary words and bad words. The tokenization options include
#' n-gram number and word stemming.
Generator <- R6::R6Class(
    "Generator",
    inherit = TextFileProcessor,
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the
        #' maximum ngram number, sample size, input file name, data cleaner,
        #' tokenization and verbose option.
        #' @param n The n-gram size for the model.
        #' @param ssize The sample size in Mb.
        #' @param ddir The data directory.
        #' @param mdir The model directory.
        #' @param dc_opts The data cleaner options.
        #' @param tg_opts The token generator options.
        #' @param verbose If progress information should be displayed.
        #' @export
        initialize = function(n = 4,
                              ssize = 30,
                              ddir = "./data",
                              mdir = "./models",
                              dc_opts = list(),
                              tg_opts = list(),
                              verbose = 0) {

            # The base class is initialized
            super$initialize(NULL, NULL, verbose)

            # The n-gram size is set
            private$n <- n
            # The sample size is set
            private$ssize <- ssize
            # The data directory name is set
            private$ddir <- ddir
            # The model directory name is set
            private$mdir <- mdir
            # If the dc_opts are given
            if (length(dc_opts) > 0) {
                # The custom dc_opts are merged with the default dc_opts
                private$dc_opts = modifyList(private$dc_opts, dc_opts)
            }
            # If the tg_opts are given
            if (length(tg_opts) > 0) {
                # The custom tg_opts are merged with the default tg_opts
                private$tg_opts = modifyList(private$tg_opts, tg_opts)
            }
        },

        #' @description
        #' It generates the model for the given name, description,
        #' n-gram size, data sample size, data cleaning options and input file.
        #' @param name The model name.
        #' @param desc The model description.
        #' @param ifn The input file name. If not given then the train.txt file
        #'   in the data directory folder is used.
        #' @param ofn The output file name. If not given, then "model.RDS" is
        #'   used as the file name .
        generate_model = function(name, desc, ifn = NULL, ofn = NULL) {
            # If the input file name is not given
            if (is.null(ifn)) {
                # The default training data file name
                ifn <- paste0(private$ddir, "/train.txt")
            }
            # If the input file does not exist
            if (!file.exists(ifn)) {
                # An error is thrown
                stop("The file: ", ifn, " does not exist !")
            }
            # If the output file name is not given
            if (is.null(ofn)) {
                # The default output file name is used
                ofn <- paste0(private$mdir, "/model.RDS")
            }
            # The data analyzer object is created
            da <- DataAnalyzer$new(fn, private$verbose)
            # The training, testing and validation data sets are generated
            da$generate_data(private$ddir,
                             list(train = .8, test = .1, validate = .1))
            # The object size is formatted
            obj_size <- file.size(ifn)/10^6
            # The proportion of data to sample
            prop <- (private$ssize/obj_size)
            # Random sample is taken and cleaned
            self$generate_sample(prop, T, 'tr')
            # The model directory is set
            private$tg_opts$dir <- private$mdir
            # The clean train data file name
            fn <- paste0(private$mdir, "/train-clean.txt")
            # For each ngram number, the ngram token file is generated
            for (i in 1:private$n) {
                # The ngram number is set
                private$tg_opts$n <- i
                # The TokenGenerator object is created
                tg <- TokenGenerator$new(fn, private$tg_opts, private$verbose)
                # The ngram tokens are generated
                tg$generate_tokens()
            }
            # The TPGenerator object is created
            tp <- TPGenerator$new()
            # The options for generating combined transition probabilities
            tp_opts <- list(
                "n" = private$n,
                "save_tp" = T,
                "format" = "obj",
                "dir" = private$mdir
            )
            # The transition probabilities are generated
            tp$generate_tp(tp_opts)
            # The information message is shown
            private$display_msg("Saving model...", 1)
            # An object of class Model is created
            m <- Model$new(
                name = name,
                desc = desc,
                data_file = fn,
                wl_file = paste0(private$mdir, "/words.RDS"),
                model = private$n,
                ssize = private$ssize,
                ddir = private$ddir,
                mdir = private$mdir,
                dc_opts = private$dc_opts,
                tg_opts = private$tg_opts,
                verbose = private$verbose
            )
            # The model object is loaded
            m$load_model()
            # The model object is saved to the models folder using the output
            # file name
            private$save_obj(m, ofn)
        }
    ),

    private = list(
        # @field tp The transition probabilities data frame.
        tp = NULL,
        # @field wl The list of unique words.
        wl = NULL,
        # @field dp The default probability is equal to 1/(N+V), where N is the
        #   number of words in the sentence, V is the number of words in the
        #   vocabulary.
        dp = NULL,
        # @field n The n-gram size.
        n = 4,
        # @field dc_opts The options for the data cleaner object.
        #   min_words -> The minimum number of words per sentence.
        #   line_count -> The number of lines to read and clean at a time.
        #   sw_file -> The stop words file path.
        #   dict_file -> The dictionary file path.
        #   bad_file -> The bad words file path.
        #   to_lower -> If the words should be converted to lower case.
        #   remove_stop -> If stop words should be removed.
        #   remove_punct -> If punctuation symbols should be removed.
        #   remove_non_dict -> If non dictionary words should be removed.
        #   remove_non_alpha -> If non alphabet symbols should be removed.
        #   remove_extra_space -> If leading, trailing and double spaces
        #     should be removed.
        #   remove_bad -> If bad words should be removed
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
        #   n -> The ngram size.
        #   save_ngrams -> If the ngram data should be saved.
        #   min_freq -> All ngrams with frequency less than min_freq are
        #     ignored.
        #   line_count -> The number of lines to process at a time.
        #   stem_words -> If words should be converted to their stem.
        #   dir -> The dir where the output file should be saved.
        #   format -> The format for the output. There are two options.
        #     'plain' -> The data is stored in plain text.
        #     'obj' -> The data is stored as a R obj.
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
        mdir = "./models")
)
