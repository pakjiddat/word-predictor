#' Generates data samples from text files
#'
#' @description
#' It provides a method for generating training, testing and validation data
#' sets from a given input text file.
#'
#' It also provides a method for generating a sample file of given size or
#' number of lines from an input text file. The contents of the sample file may
#' be cleaned or randomized.
DataSampler <- R6::R6Class(
    "DataSampler",
    inherit = Base,
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the
        #' verbose option.
        #' @param dir The directory for storing the input and output files.
        #' @param ve The level of detail in the information messages.
        #' @export
        initialize = function(dir = ".", ve = 0) {
            # The directory name is set
            private$dir <- dir
            # The base class is initialized
            super$initialize(NULL, NULL, ve)
        },

        #' @description
        #' Generates a sample file of given size from the given input file. The
        #' file is saved to the directory given by the dir object attribute.
        #' Once the file has been generated, its contents may be cleaned or
        #' randomized.
        #' @param fn The input file name. It is the short file name relative to
        #'   the dir attribute.
        #' @param ss The number of lines or proportion of lines to sample.
        #' @param ic If the sample file should be cleaned.
        #' @param ir If the sample file contents should be randomized.
        #' @param ofn The output file name. It will be saved to the dir.
        #' @param is If the sampled data should be saved to a file.
        #' @param dc_opts The options for cleaning the data.
        #' @examples
        #' # Start of environment setup code
        #' # The level of detail in the information messages
        #' ve <- 0
        #' # The name of the folder that will contain all the files. It will be
        #' # created in the current directory. NULL implies tempdir will be used
        #' fn <- NULL
        #' # The required files. They are default files that are part of the
        #' # package
        #' rf <- c("input.txt")
        #' # An object of class EnvManager is created
        #' em <- EnvManager$new(ve = ve, rp = "./")
        #' # The required files are downloaded
        #' ed <- em$setup_env(rf, fn)
        #' # End of environment setup code
        #'
        #' # The sample file name
        #' sfn <- paste0(ed, "/sample.txt")
        #' # An object of class DataSampler is created
        #' ds <- DataSampler$new(dir = ed, ve = ve)
        #' # The sample file is generated
        #' ds$generate_sample(
        #'     fn = "input.txt",
        #'     ss = 0.5,
        #'     ic = FALSE,
        #'     ir = FALSE,
        #'     ofn = "sample.txt",
        #'     is = TRUE
        #' )
        #'
        #' # The test environment is removed. Comment the below line, so the
        #' # files generated by the function can be viewed
        #' em$td_env()
        generate_sample = function(fn, ss, ic, ir, ofn, is, dc_opts = NULL) {
            # The full path to the input file
            fn <- paste0(private$dir, "/", fn)
            # If the input file does not exist
            if (!file.exists(fn)) {
                # The error message
                private$dm("The input file: ",
                    fn,
                    " does not exist\n",
                    md = -1,
                    ty = "e"
                )
            }
            # The output file name path
            of <- paste0(private$dir, "/", ofn)
            # The sample file is generated from the given file
            data <- private$generate_sf_from_f(fn, ss, ic, ir, of, is, dc_opts)
            # If the data should not be saved
            if (!is) {
                # The data is returned
                return(data)
            }
        },

        #' @description
        #' It generates training, testing and validation data sets
        #' from the given input file. It first reads the file given as a
        #' parameter to the current object. It partitions the data into
        #' training, testing and validation sets, according to the perc
        #' parameter. The files are named train.txt, test.txt and va.txt and are
        #' saved to the given output folder.
        #' @param fn The input file name. It should be relative to the dir
        #'   attribute.
        #' @param percs The size of the training, testing and validation sets.
        #' @examples
        #' # Start of environment setup code
        #' # The level of detail in the information messages
        #' ve <- 0
        #' # The name of the folder that will contain all the files. It will be
        #' # created in the current directory. NULL implies tempdir will be
        #' # used
        #' fn <- NULL
        #' # The required files. They are default files that are part of the
        #' # package
        #' rf <- c("input.txt")
        #' # An object of class EnvManager is created
        #' em <- EnvManager$new(ve = ve)
        #' # The required files are downloaded
        #' ed <- em$setup_env(rf, fn)
        #' # End of environment setup code
        #'
        #' # The files to clean
        #' fns <- c("train", "test", "validate")
        #' # An object of class DataSampler is created
        #' ds <- DataSampler$new(dir = ed, ve = ve)
        #' # The train, test and validation files are generated
        #' ds$generate_data(
        #'     fn = "input.txt",
        #'     percs = list(
        #'         "train" = 0.8,
        #'         "test" = 0.1,
        #'         "validate" = 0.1
        #'     )
        #' )
        #'
        #' # The test environment is removed. Comment the below line, so the
        #' # files generated by the function can be viewed
        #' em$td_env()
        generate_data = function(fn, percs) {
            # The directory containing the input and output files
            dir <- private$dir
            # The information message is shown
            private$dm(
                "Generating training,",
                "testing and validation data sets\n",
                md = 1
            )

            # The input file path is generated
            fn <- paste0(private$dir, "/", fn)
            # If the input file does not exist
            if (!file.exists(fn)) {
                # The error message
                private$dm("The input file: ",
                    fn,
                    " does not exist",
                    md = -1,
                    ty = "e"
                )
            }
            # If the train, test and validation files already exist
            if (file.exists(paste0(dir, "/train.txt")) &&
                file.exists(paste0(dir, "/test.txt")) &&
                file.exists(paste0(dir, "/validate.txt"))) {
                # The information message is shown
                private$dm(
                    "The train, test and validate files already exist\n",
                    md = 1,
                    ty = "w"
                )
            } else {
                # The input file is read
                data <- private$read_file(fn, F)
                # The number of lines in the data
                lc <- length(data)
                # The required data
                rd <- data[1:lc]

                # The number of lines in train set
                tr_lc <- round(lc * percs[["train"]])
                # The number of lines in test set
                te_lc <- round(lc * percs[["test"]])
                # The number of lines in validate set
                va_lc <- round(lc * percs[["validate"]])

                # The training set data
                train_ds <- rd[1:tr_lc]
                # The testing set data
                test_ds <- rd[tr_lc:(tr_lc + te_lc)]
                # The validation set data
                validate_ds <- rd[(tr_lc + te_lc):(tr_lc + te_lc + va_lc)]
                # The training data is written to file
                private$write_file(train_ds, paste0(dir, "/train.txt"), F)
                # The testing data is written to file
                private$write_file(test_ds, paste0(dir, "/test.txt"), F)
                # The validation data is written to file
                private$write_file(validate_ds, paste0(dir, "/validate.txt"), F)
            }
            # The information message is shown
            private$dh("DONE", "=", md = 1)
        }
    ),
    private = list(
        # @field dir The folder containing the input and output files.
        dir = ".",
        # @description Generates a sample file of given size from the given
        # input file. The file is optionally cleaned and saved.
        # @param fn The input file name. It is the short file name relative to
        #   the dir. If not given, then the file name is auto generated from
        #   the type parameter.
        # @param ss The number of lines or proportion of lines to sample.
        # @param ic If the sample file should be cleaned.
        # @param ir If the sample file contents should be randomized.
        # @param of The output file path.
        # @param is If the sampled data should be saved to a file.
        # @param dc_opts The options for cleaning the data.
        # @return The sampled data is returned
        generate_sf_from_f = function(fn = NULL, ss, ic, ir, of, is, dc_opts) {
            # The information message is shown
            private$dh("Generating sample file", "-", md = 1)
            # The input file is read
            data <- private$read_file(fn, F)
            # The number of lines in the main file
            lc <- length(data)
            # If the data should be randomized
            if (ir) {
                # The random indexes
                i <- sample(1:lc, size = lc)
                # The randomized data
                data <- data[i]
            }
            # If the sample size is less than 1
            if (ss < 1) {
                # The number of lines in the sample file
                lc <- round(lc * ss)
            } else {
                lc <- ss
            }
            # The sample file data
            data <- data[1:lc]
            # If the data should be saved
            if (is) {
                # The sample file data is saved
                private$write_file(data, of, F)
            }

            # If the sample file should be cleaned
            if (ic) {
                # If the data should be saved
                dc_opts[["save_data"]] <- is
                # The data cleaner object is created
                dc <- DataCleaner$new(of, dc_opts, ve = private$ve)
                # The sample file is cleaned
                data <- dc$clean_file()
            }
            # The information message is shown
            private$dh("DONE", "=", md = 1)

            return(data)
        }
    )
)
