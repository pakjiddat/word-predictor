#' It is used to generate data samples from text files.
#'
#' @description
#' It provides a method for generating training, testing and
#' validation data sets from a given input text file. It also provides a method
#' for generating a sample file of given size or number of lines from an input
#' text file.
DataSampler <- R6::R6Class(
    "DataSampler",
    inherit = TextFileProcessor,
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the
        #' verbose option.
        #' @param ddir The data directory.
        #' @param mdir The model directory.
        #' @param ve If progress information should be displayed.
        initialize = function(ddir = "./data",
                              mdir = "./models",
                              ve = 0) {
            # The data directory name is set
            private$ddir <- ddir
            # The model directory name is set
            private$mdir <- mdir
            # The base class is initialized
            super$initialize(NULL, NULL, ve)
        },

        #' @description Generates a sample file of given size from the given
        #' input file. The file is optionally cleaned and saved.
        #' @param fn The input file name. It is the short file name relative to
        #'   the ddir. If not given, then the file name is auto generated from
        #'   the type parameter.
        #' @param ss The number of lines or proportion of lines to sample.
        #' @param ic If the sample file should be cleaned.
        #' @param t The type of sample. It can be:
        #'   'tr' -> training.
        #'   'te' -> testing.
        #'   'va' -> validation.
        #' @param is If the sampled data should be saved to a file.
        generate_sample = function(fn = NULL, ss, ic, t, is) {
            # If the type is 'tr'
            if (t == 'tr') sfn <- 'train'
            # If the type is 'te'
            else if (t == 'te') sfn <- 'test'
            # If the type is 'va'
            else if (t == 'va') sfn <- 'validate'
            # If the input file name is not given
            if (is.null(fn)) {
                # The input file name
                fn <- paste0(private$ddir, "/", sfn, ".txt")
            }
            # If the input file name is given
            else {
                # The full path to the input file
                fn <- paste0(private$ddir, "/", fn)
            }
            # If the input file does not exist
            if (!file.exists(fn)) {
                # The information message
                msg <- paste0("The input file: ", fn, " does not exist")
                # An error is thrown
                stop(msg)
            }
            # The sample file name
            sf <- paste0(private$mdir, "/", sfn, ".txt")
            # The clean sample file name
            csf <- paste0(private$mdir, "/", sfn, "-clean.txt")
            # If the cleaned sample file already exists
            if (file.exists(csf)) {
                # The information message
                msg <- paste0("The cleaned sample file: ", csf,
                              " already exists")
                # Information message is shown
                private$display_msg(msg, 2)
            }
            else {
                # If the sample file does not exist
                if (!file.exists(sf)) {
                    # The information message
                    msg <- paste0("Generating sample file from the file: ", fn)
                    # Information message is shown
                    private$display_msg(msg, 2)
                    # The input file is read
                    data <- private$read_file(fn, F)
                    # If the sample size is less than 1
                    if (ss < 1) {
                        # The number of lines in the main file
                        lc <- length(data)
                        # The number of lines in the sample file
                        lc <- round(lc*ss)
                    }
                    else {
                        lc <- ss
                    }
                    # The sample file data
                    data <- data[1:lc]
                    # If the data should be saved
                    if (is) {
                        # The sample file data is saved
                        private$write_file(data, sf, F)
                    }
                }
                # If the sample file exists
                else {
                    # The information message
                    msg <- paste0("The sample file: ", sf, " already exists")
                    # Information message is shown
                    private$display_msg(msg, 2)
                }
                # If the sample file should be cleaned
                if (ic) {
                    # The line count is set to 5000
                    opts[["line_count"]] <- 5000
                    # The output file name
                    opts[["output_file"]] <- csf
                    # If the data should be saved
                    opts[["save_data"]] <- is
                    # The data cleaner object is created
                    dc <- DataCleaner$new(sf, opts, ve = private$ve)
                    # The sample file is cleaned
                    data <- dc$clean_file()
                }
            }
            # If the data should not be saved
            if (!is) {
                # The data is returned
                return(data)
            }
        },

        #' @description
        #' It generates training, testing and validation data sets
        #' from the given input file. It first reads the file given as a
        #' parameter to the current object. It generates random indexes for the
        #' data. It partitions the data into training, testing and validation
        #' sets, according to the given parameters. The files are named
        #' train.txt, test.txt and va.txt. The files are saved to the given
        #' output folder.
        #' @param fn The input file name. It should be relative to the ddir.
        #' @param dir The name of the output folder.
        #' @param percs The size of the training, testing and validation sets.
        generate_data = function(fn, dir, percs) {
            # The information message is shown
            private$display_msg(
                "Generating training, testing and validation data sets...", 1)
            # The input file path is generated
            fn <- paste0(private$ddir, "/", fn)
            # If the input file does not exist
            if (!file.exists(fn)) {
                # The information message
                msg <- paste0("The input file: ", fn, " does not exist")
                # An error is thrown
                stop(msg)
            }
            # If the train, test and validation files already exist
            if (file.exists(paste0(dir, "/train.txt")) &&
                file.exists(paste0(dir, "/test.txt")) &&
                file.exists(paste0(dir, "/validate.txt"))) {
                # The information message
                msg <- "The train, test and validate files already exist"
                # The information message is shown
                private$display_msg(msg, 1)
            }
            else {
                # The input file is read
                data <- private$read_file(fn, F)
                # The number of lines in the data
                lc <- length(data)
                # Random indexes are generated
                indexes <- sample(1:lc, lc)
                # The randomized data
                rd <- data[indexes]
                # The training set data
                train_ds <- rd[1:round(lc*percs[["train"]])]
                # The testing set data
                test_ds <- rd[1:round(lc*percs[["test"]])]
                # The validation set data
                validate_ds <- rd[1:round(lc*percs[["validate"]])]
                # The training data is written to file
                private$write_file(train_ds, paste0(dir, "/train.txt"), F)
                # The testing data is written to file
                private$write_file(test_ds, paste0(dir, "/test.txt"), F)
                # The validation data is written to file
                private$write_file(validate_ds, paste0(dir, "/validate.txt"), F)
            }
        }
    ),

    private = list(
        # @field ddir The folder containing the data files
        ddir = "./data",
        # @field mdir The folder containing the model files
        mdir = "./models"
    )
)
