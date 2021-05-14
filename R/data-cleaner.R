#' Provides data cleaning functionality
#'
#' @description
#' Allows removing unneeded characters from text files.
#'
#' @details
#' It provides a method for cleaning text files. It allows removing bad words,
#' stop words, non dictionary words, extra space, punctuation and non-alphabet
#' characters. Converting text to lower case. It supports large text files.
DataCleaner <- R6::R6Class(
    "DataCleaner",
    inherit = TextFileProcessor,
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the file name
        #' and verbose options.
        #' @param file_name The path to the file to clean.
        #' @param opts The options for data cleaning.
        #'   min_words -> The minimum number of words per sentence.
        #'   line_count -> The number of lines to read and clean at a time.
        #    save_data -> If the combined processed lines should be saved.
        #    output_file -> Name of the output file used to store the data.
        #'   sw_file -> The stop words file path.
        #    dict_file -> The dictionary file path.
        #'   bad_file -> The bad words file path.
        #'   to_lower -> If the words should be converted to lower case.
        #'   remove_stop -> If stop words should be removed.
        #'   remove_punct -> If punctuation symbols should be removed.
        #'   remove_non_dict -> If non dictionary words should be removed.
        #'   remove_non_alpha -> If non alphabet symbols should be removed.
        #'   remove_extra_space -> If leading, trailing and double spaces
        #'     should be removed.
        #'   remove_bad -> If bad words should be removed
        #' @param verbose Indicates if progress information should be displayed.
        #' @export
        initialize = function(file_name = NULL,
                              opts = list(),
                              verbose = 0) {
            # The stop words file is checked
            opts[["sw_file"]] <- private$check_file(
                opts[["sw_file"]], "stop-words.txt")
            # The bad words file is checked
            opts[["bad_file"]] <- private$check_file(
                opts[["bad_file"]], "bad-words.txt")
            # The dict words file is checked
            opts[["dict_file"]] <- private$check_file(
                opts[["dict_file"]], "dict-no-bad.txt")
            # The given options are merged with the opts attribute
            private$dc_opts <- modifyList(private$dc_opts, opts)
            # The save_data option of base class is set
            private$opts[["save_data"]] <- private$dc_opts[["save_data"]]
            # The output_file option of base class is set
            private$opts[["output_file"]] <- private$dc_opts[["output_file"]]
            # The stop words file is read
            private$sw <- private$read_file(private$opts[["sw_file"]], F);
            # The dictionary file is read
            private$dw <- private$read_file(private$opts[["dict_file"]], F);
            # The bad word file is read
            private$bw <- private$read_file(private$opts[["bad_file"]], F);
            # The base class is initialized
            super$initialize(file_name, private$opts[['line_count']], verbose)
        },

        #' @description
        #' It removes unneeded characters from the given text with
        #' several options. It allows removing punctuations, numbers, symbols,
        #' urls and separators. It allows removing bad words, stop words and
        #' words not in the given dictionary file. It reads the given file one
        #' line at a time, removing unneeded characters. After every line_count
        #' number of lines, the cleaned lines are saved to the output file.
        clean_file = function() {
            # The information message
            msg <- paste0("Cleaning the sample file...", private$file_name)
            # The information message is shown
            private$display_msg(msg, 1)
            # The base class process_file function is called
            private$process_file(private$pre_process, private$process,
                               private$post_process)
            # If the data should not be saved
            if (!private$opts[["save_data"]]) {
                # The processed output is returned
                return(private$p_output)
            }
        },

        #' @description
        #' It cleans the given lines of text using the options
        #' passed to the current object.
        #' @param lines The input sentences.
        #' @return The cleaned lines of text.
        clean_lines = function(lines) {
            # The lines to clean
            l <- lines
            # If a line does not end with a ".", then "." is appended to the
            # line
            l <- gsub("(.+[^\\.])$", "\\1.", l)
            # The "." character is replaced with the string "specialdotsep"
            l <- gsub("\\.", " specialdotsep ", l)
            # If the words should be converted to lower case
            if (private$opts[["to_lower"]]) {
                # The information message
                private$display_msg("Converting lines to lower case...", 3)
                # The line is converted to lower case
                l <- tolower(l)
            }
            # If punctuation symbols should be removed
            if (private$opts[["remove_punct"]]) {
                # The information message
                private$display_msg("Removing punctuation symbols...", 3)
                # The pattern for removing all punctuation symbols
                l <- gsub("[[:punct:]]+", "", l)
            }
            # If non alphabet symbols should be removed
            if (private$opts[["remove_non_alpha"]]) {
                # The information message
                private$display_msg("Removing non alphabet symbols...", 3)
                # Words containing non alphabetical characters are removed
                l <- gsub("([^[:alpha:]\\s])", "", l, perl = T)
            }

            # If stop words should be removed
            if (private$opts[["remove_stop"]]) {
                # The information message
                private$display_msg("Removing stop words..", 3)
                # Stop words are collapsed
                sw <- paste(private$sw, collapse = "|")
                swp <- paste("\\b(", sw, ")\\b", sep = "")
                # The stop words are removed
                l <- gsub(swp, "", l)
            }
            # If extra spaces should be removed
            if (private$opts[["remove_extra_space"]]) {
                # The information message
                private$display_msg("Removing extra space...", 3)
                # Multiple spaces are replaced by single space
                l = gsub("\\s{2,}", " ", l)
                # Leading and trailing whitespaces are removed
                l = trimws(l)
            }

            # The words in the lines are extracted
            words <- strsplit(l, split = " ")
            # The words are converted to an atomic list
            words <- unlist(words)
            # If non dictionary words should be removed
            if (private$opts[["remove_non_dict"]]) {
                # The "specialdotsep" string is added to list of dictionary
                # words
                dw <- c(private$dw, "specialdotsep")
                # The non dictionary words are removed from the data
                words <- words[words %in% dw]
                # All 1 length words except for 'a' and 'i' are removed
                # The indexes position of all words that are "a" or "i"
                i1 <- (words == "a" | words == "i")
                # The index position of words of length 2 or more
                i2 <- (nchar(words) > 1)
                # The list of all words of length 2 or more including "a" and
                # "i"
                words <- words[i1 | i2]
            }
            # If bad words should be removed
            if (private$opts[["remove_bad"]]) {
                # The "specialdotsep" string is added to list of bad words
                bw <- c(private$bw, "specialdotsep")
                # The bad words are removed from the data
                words <- words[!words %in% private$bw]
            }
            # The words are combined with space
            l <- paste(words, collapse = " ")
            # The "specialdotsep" string is replaced with "."
            l <- gsub("specialdotsep", ".", l)
            # The sentences in the lines are extracted
            l <- strsplit(l, split = "\\.")
            # The sentences are converted to an atomic list
            l <- unlist(l)
            # If extra spaces should be removed
            if (private$opts[["remove_extra_space"]]) {
                # Multiple spaces are replaced by single space
                l = gsub("\\s{2,}", " ", l)
                # Leading and trailing whitespaces are removed
                l = trimws(l)
            }

            # If each sentence should have a minimum number of words
            if (private$opts[["min_words"]] > -1) {
                # The number of words in each sentence
                wc <- str_count(l, pattern = boundary("word"))
                # The lines containing less than min_words number of words are
                # removed
                l <- l[wc >= private$opts[["min_words"]]]
            }

            # Consecutive 'a' and 'i' are replaced with single 'a' or 'i'
            l <- gsub("(a\\s){2,}", "\\1 ", l)
            l <- gsub("(i\\s){2,}", "\\1 ", l)
            l <- gsub("a$", "", l)

            return(l)
        }
    ),

    private = list(
        # @field dc_opts The options for the data cleaner object.
        #   min_words -> The minimum number of words per sentence.
        #   line_count -> The number of lines to read and clean at a time.
        #   save_data -> If the combined processed lines should be saved.
        #   output_file -> Name of the output file used to store the data.
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
            "save_data" = T,
            "output_file" = "./data/sample-clean.txt",
            "sw_file" = NULL,
            "dict_file" = NULL,
            "bad_file" = NULL,
            "to_lower" = T,
            "remove_stop" = F,
            "remove_punct" = T,
            "remove_non_dict" = T,
            "remove_non_alpha" = T,
            "remove_extra_space" = T,
            "remove_bad" = F
        ),

        # @field sw The list of stop words.
        sw = list(),

        # @field bw The list of bad words.
        bw = list(),

        # @field dw The list of dictionary words.
        dw = list(),

        # @description
        # Performs processing for the \code{clean_files} function.
        # It processes the given lines of text. It divides the given lines of
        # text into sentences by spliting on '.'. Each sentence is then cleaned
        # using \code{clean_line}. If the number of words in the cleaned
        # sentence is less than min_words, then the sentence is rejected.
        # @param lines The lines of text to clean.
        # @return The processed line is returned.
        process = function(lines) {
            # The sentence is cleaned
            cl <- self$clean_lines(lines)

            return(cl)
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
