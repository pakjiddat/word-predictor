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
ModelGenerator <- R6::R6Class(
    "ModelGenerator",
    inherit = TextFileProcessor,
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the
        #' maximum ngram number, sample size, input file name, data cleaner
        #' options, tokenization options, combined transition probabilities file
        #' name and verbose.
        #' @param name The model name.
        #' @param desc The model description.
        #' @param fn The model file name. If not set, then model.RDS is used.
        #' @param df The path of the file used to generate the model. If the
        #'   data was cleaned, then df is the path to the cleaned file. It
        #'   should be the short file name. It should be present in the data
        #'   directory.
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
            # An object of class Model is created
            private$m <- Model$new(
                name = name,
                desc = desc,
                fn = fn,
                df = df,
                n = n,
                ssize = ssize,
                ddir = ddir,
                mdir = mdir,
                dc_opts = dc_opts,
                tg_opts = tg_opts,
                ve = ve
            )
        },

        #' @description
        #' It generates the model using the current object's attributes.
        generate_model = function() {
            # The information message is displayed
            private$display_msg("Generating n-gram model...", 1)

            # All files in the model directory are removed
            ir <- private$remove_model_files()
            # If the model file already exists
            if (!ir) return(FALSE)
            # The cleaned sampled data file is generated
            private$generate_sample()
            # The data files are generated
            private$generate_data_files()
            # The n-gram tokens are generated
            private$generate_ngram_tokens()
            # The tp data is generated
            private$generate_tp_data()
            # The model is saved
            private$save_model()
        }
    ),

    private = list(
        # @field m The model object.
        m = NULL,

        # @description
        # Saves the model to a file
        save_model = function() {
            # The model directory path
            mdir <- private$m$get_config("mdir")
            # The model file name
            ofn <- private$m$get_config("fn")
            # The output  file path
            ofp <- paste0(mdir, "/", ofn)
            # The information message is shown
            private$display_msg("Saving model...", 1)
            # The model object is loaded
            private$m$load_model()
            # The model object is saved to the models folder using the output
            # file name
            private$save_obj(private$m, ofp)
        },

        # @description
        # Removes all files in the model directory.
        # @return TRUE is returned if model files were removed. FALSE is
        #   returned if the model file already exists.
        remove_model_files = function() {
            # The model directory path
            mdir <- private$m$get_config("mdir")
            # The model file name
            ofn <- private$m$get_config("fn")
            # The output  file path
            ofp <- paste0(mdir, "/", ofn)
            # If the model file already exists
            if (file.exists(ofp)) {
                # The information message
                msg <- paste0("The model file: ", ofp, " already exists.")
                # The information message is displayed
                private$display_msg(msg, 1)
                # The function returns FALSE
                return(F)
            }

            # The information message is displayed
            private$display_msg(
                "Removing all files in the model directory...", 1)
            # Each file in the model directory is deleted
            for (fn in dir(mdir, full.names = T)) {
                # The file is removed
                file.remove(fn)
            }
            # The function returns TRUE
            return(T)
        },

        # @description
        # Generates a cleaned sample file of given size from the
        # given input data file. The name of the output file is train-clean.txt.
        # The file is saved to the data directory.
        generate_sample = function() {
            # The input data file name
            df <- private$m$get_config("df")
            # The sample size
            ssize <- private$m$get_config("ssize")
            # The data directory path
            ddir <- private$m$get_config("ddir")
            # The model directory path
            mdir <- private$m$get_config("mdir")
            # The path to the input data file name
            dfp <- paste0(ddir, "/", df)
            # The object size is formatted
            obj_size <- file.size(dfp)/10^6
            # The proportion of data to sample
            prop <- (ssize/obj_size)
            # The DataSampler object is created
            ds <- DataSampler$new(ddir = ddir, mdir = ddir,  ve = private$ve)
            # Sample is taken and cleaned
            ds$generate_sample(df, prop, T, F, 'tr', T)
        },

        # @description
        # Generates test, train and validation files from the
        # cleaned sample file, which is train-clean.txt. The name of the
        # output files are train.txt, test.txt and validation.txt. The files
        # are saved to the model directory.
        generate_data_files = function() {
            # The data directory path
            ddir <- private$m$get_config("ddir")
            # The model directory path
            mdir <- private$m$get_config("mdir")
            # The DataSampler object is created
            ds <- DataSampler$new(ddir = ddir, mdir = mdir,  ve = private$ve)
            # The training, testing and validation data sets are generated
            ds$generate_data("train-clean.txt", mdir, list(
                train = .8,
                test = .1,
                validate = .1
            ))
        },

        # @description
        # Generates transition probabilities data from n-gram token
        # file. The transition probabilties data is saved as files.
        generate_tp_data = function() {
            # The n-gram number
            n <- private$m$get_config("n")
            # The model directory path
            mdir <- private$m$get_config("mdir")
            # The options for generating combined transition probabilities
            tp_opts <- list(
                "n" = n,
                "save_tp" = T,
                "format" = "obj",
                "dir" = mdir
            )
            # The TPGenerator object is created
            tp <- TPGenerator$new(tp_opts)
            # The transition probabilities are generated
            tp$generate_tp()
        },

        # @description
        # Generates n-gram tokens from the cleaned data input file.
        # The n-gram tokens are saved as files.
        generate_ngram_tokens = function() {
            # The n-gram number
            n <- private$m$get_config("n")
            # The model directory path
            mdir <- private$m$get_config("mdir")
            # The TokenGenerator object options
            tg_opts <- private$m$get_config("tg_opts")
            # The model directory is set
            tg_opts$dir <- mdir
            # The clean train data file name
            fn <- paste0(mdir, "/train.txt")
            # For each ngram number, the ngram token file is generated
            for (i in 1:n) {
                # The ngram number is set
                tg_opts$n <- i
                # The TokenGenerator object is created
                tg <- TokenGenerator$new(fn, tg_opts, private$ve)
                # The ngram tokens are generated
                tg$generate_tokens()
            }
        }
    )
)
