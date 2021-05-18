#' Base class for text file processing
#'
#' @description
#' Provides basic structure for processing text files.
#'
#' @details
#' It provides pre-processing, processing and post-processing functions, which
#' need to be overridden by derived classes. The pre-processing function is
#' called before reading a file. The process function is called for processing a
#' line. The post processing function is called on the processed data. It also
#' provides a method for generating a sample file from an input text file
#' @importFrom pryr object_size
TextFileProcessor <- R6::R6Class(
    "TextFileProcessor",
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the file name
        #' and verbose options.
        #' @param fn The path to the file to clean.
        #' @param lc The number of lines to read and clean at a time.
        #' @param ve Indicates if progress information should be displayed.
        initialize = function(fn = NULL, lc = 100, ve = 2) {
            # If the given file name is not NULL and is not valid
            if (!is.null(fn) && !file.exists(fn))
                stop("The given file name is not valid")

            # The base class attributes are set
            # The file name is set
            private$fn <- fn
            # The verbose option is set
            private$ve <- ve
            # The line count is set
            private$lc <- lc
            # The processed output is set
            private$p_output <- NULL
        }
    ),
    private = list(
        # @field opts The list of file processing options.
        #   save_data -> If the combined processed lines should be saved.
        #   ret_data -> If the data should be returned.
        #   output_file -> Name of the output file used to store the data.
        opts = list(
            "save_data" = F,
            "ret_data" = F,
            "output_file" = NULL
        ),

        # @field lc The number of lines to read and process at a time.
        lc = 100,

        # @field p_output The output of the processing step
        p_output = NULL,

        # @field fn The name of the text file to process.
        fn = NULL,

        # @field ve Indicates if progress data should be printed.
        ve = 0,

        # @field con The input file connection
        con = NULL,

        # @description
        # Reads the contents of the given file. Loads the file
        # contents to a R object, a data frame or character vector.
        # @param fn The file name.
        # @param format The file format. 'plain' or 'obj'
        # @param opts Options for reading the file.
        # @return The required data.
        read_data = function(fn, format, opts) {
            # If the format is plain
            if (format == "plain") {
                # The file is read
                data <- private$read_file(fn, opts)
            }
            # If the format is obj
            else if (format == "obj") {
                # The file is read
                data <- private$read_obj(fn)
            }

            return(data)
        },

        # @description
        # Writes the given data to a file. The data may be a R object, a
        # character vector or a data frame.
        # @param fn The file name.
        # @param format The file format. 'plain' or 'obj'
        # @param opts Options for writting to the file.
        # @return The required data.
        write_data = function(data, fn, format, opts) {
            # If the format is plain
            if (format == "plain") {
                # The data is written to a file
                private$write_file(data, fn, opts)
            }
            # If the format is obj
            if (format == "obj") {
                # The R object is saved
                private$save_obj(data, fn)
            }
        },

        # @description
        #' Reads the given file one line at a time. It runs the given
        #' pre-processing function before reading the file. It runs the given
        # line processing function for each line. It optionally saves the
        # output of line processing after reading the file or after processing
        # certain number of lines.
        # @param pre_process The pre-processing function.
        # @param process The function used to process each line.
        # @param post_process The function used to perform post processing.
        # @return The combined processed data
        process_file = function(pre_process, process, post_process) {
            # Pre-processing is done
            pre_process()
            # The file is opened
            private$con <- file(private$fn)
            # The connection is opened for reading
            open(private$con)
            # The lines to be read,
            lines <- c()
            # The loop counter
            c <- 0
            # Indicates that data should not be appended
            is_app <- F
            # The output file name
            of <- private$opts[["output_file"]]
            # All lines are read
            while (TRUE) {
                # The lines are read
                lines <- readLines(private$con, n = private$lc,
                                   skipNul = TRUE)
                # If all the lines have been read
                if (length(lines) == 0) break
                # The lines are processed
                p_lines <- process(lines)
                # If the processed lines are NULL
                if(is.null(p_lines)) next
                # If the data should be saved
                if (private$opts[["save_data"]]) {
                    # The cleaned data is written to file
                    private$write_file(p_lines, of, is_app)
                    # Debug message
                    private$display_msg(
                        paste(length(p_lines), "lines were written"), 1)
                    # Indicates that data should be appended
                    is_app <- T
                }
                # If the processed data should be returned
                if (private$opts[["ret_data"]]) {
                    # The processed output is merged
                    private$p_output <- c(private$p_output, p_lines)
                }
                # The loop counter is increased by 1
                c <- c + 1
                # Debug message
                private$display_msg(
                    paste(private$lc*c, "lines have been processed"), 1)

                # if (c == 2) break;
            }
            # The file connection is closed if it is open
            close(private$con)
            # Post processing is performed
            post_process()
            # If the data should be returned
            if (private$opts[["ret_data"]]) {
                # The processed output is returned
                return(private$p_output)
            }
        },

        # @description
        # Reads the given file and returns its contents.
        # @param fn The name of the file to read.
        # @param is_csv If the data is a csv file
        # @return The file data
        read_file = function(fn, is_csv) {
            # The information message
            msg <- paste0("Reading file: ", fn)
            # Information message is shown
            private$display_msg(msg, 1)
            # If the file is not a csv file
            if (!is_csv) {
                # File is opened for reading
                con <- file(fn)
                # The file contents are read
                data <- readLines(con, skipNul = TRUE)
                # The file connection is closed
                close(con)
            }
            else {
                data <- read.csv(fn)
            }
            # The data is returned
            return (data)
        },

        # @description
        # Reads the given number of lines from the given file.
        # @param fn The name of the file to read.
        # @param lc The number of lines to read.
        # @return The file data
        read_lines = function(fn, lc) {
            # The information message
            msg <- paste0("Reading file: ", fn)
            # Information message is shown
            private$display_msg(msg, 1)
            # File is opened for reading
            con <- file(fn)
            # The file contents are read
            data <- readLines(con, n = lc, skipNul = TRUE)
            # The file connection is closed
            close(con)
            # The data is returned
            return (data)
        },

        # @description
        # Writes the given data to the given file. The data may be appended to
        # an existing file.
        # @param data The data to be written.
        # @param fn The name of the file.
        # @param is_append Indicates if data should be saved.
        write_file = function(data, fn, is_append) {
            # The information message
            msg <- paste0("Saving file: ", fn)
            # Information message is shown
            private$display_msg(msg, 1)
            # If the given data is a data frame
            if ("data.frame" %in% class(data)) {
                # The data frame is  written to a file
                write.csv(data, fn, row.names = F)
            }
            else {
                # The file open mode
                mode <- "w"
                # If the data should be appended
                if (is_append) mode <- "a"
                # The output file is opened for writing
                con <- file(fn, open = mode)
                # The data is written to the output file
                writeLines(data, con)
                # The file connection is closed
                close(con)
            }
        },

        # @description
        # Calculates the size of the given object or formats the given bytes
        # object as a number without units. The returned number is the size in
        # Mb.
        # @param obj An object of class bytes or an object whoose size is to be
        #   found.
        # @return The size formatted as a string.
        format_size = function(obj) {
            # If the obj is not of class bytes
            if (!("bytes" %in% class(obj))) {
                # The object size
                obj_size <- object_size(obj)
            }
            else {
                obj_size <- obj
            }
            # The bytes obj is formatted as a string
            obj_size <- utils:::format.object_size(obj_size, units = "Mb")
            # The Mb suffix is removed
            obj_size <- sub(" Mb", "\\1", obj_size)
            # The object size is converted to a number
            obj_size <- as.numeric(obj_size)

            return(obj_size)
        },

        # @description
        # Saves the given object as a file.
        # @param obj The object to save.
        # @param fn The file name.
        save_obj = function(obj, fn) {
            # The information message
            msg <- paste0("Saving file: ", fn)
            # Information message is shown
            private$display_msg(msg, 1)
            # The object is saved to a file
            saveRDS(obj, fn)
        },

        # @description
        # Reads the contents of the given file. Loads the file
        # contents to a R object.
        # @param fn The file name.
        # @return The loaded R obj.
        read_obj = function(fn) {
            # The information message
            msg <- paste0("Reading file: ", fn)
            # Information message is shown
            private$display_msg(msg, 1)
            # If the file does not exist
            if (!file.exists(fn)) {
                # The information message
                msg <- paste0("The file: ", fn, " cannot be read !")
                # Program execution is stopped
                stop(msg)
            }
            else {
                # The object is saved
                obj <- readRDS(fn)
            }

            return(obj)
        },

        # @description
        # Prints the given message depending on verbose settings.
        # @param msg The message to be printed.
        # @param min_debug The minimum debugging level
        display_msg = function(msg, min_debug) {
            # If verbose is >= min_debug , then message is displayed
            if (private$ve >= min_debug) {
                print(msg)
            }
        },

        # @description
        # Performs processing on the data. It should be
        # overriden by a derived class.
        # @param lines The lines to process
        process = function(lines) {

        },

        # @description
        # Performs post-processing on the processed data. It should be
        # overriden by a derived class.
        post_process = function() {

        },

        # @description
        # Performs pre-processing on the processed data. It should be
        # overriden by a derived class.
        pre_process = function() {
            return(NULL)
        },

        # @description
        # Checks if the given file exists. If it does not exist,
        # then it tried to load the file from the external data folder of the
        # package. It throws an error if the file was not found
        # @param fn The file name.
        # @param dfn The name of the default file in the external data folder of
        #   the package.
        # @return The name of the file if it exists, or the full path to the
        #   default file.
        check_file = function(fn, dfn) {
            # The required file name
            rfn <- fn
            # If the file is not given
            if (is.null(fn)) {
                # The file path is set to the default file
                # included with the wordpredictor package
                rfn <- system.file("extdata", dfn, package = "wordpredictor")
                # If the file was not found
                if (!file.exists(rfn))
                    stop(paste0("The file: ", rfn," does not exist !"))
            }
            # If the file name is given but the file does not exist
            else if (!file.exists(fn)) {
                # An error message is shown
                stop(paste0("The file: ", fn," does not exist !"))
            }
            return (rfn)
        }
    )
)
