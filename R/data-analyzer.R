#' It is used to analyze text data
#'
#' @description
#' It provides information on text files, such as number of lines
#' and number of words. It allows generating sample file from an input text
#' file. It displays bar plots showing word frequencies.
#'
#' @details
#' It provides a method that returns text file information. The text
#' file information includes total number of lines, max, min and mean line
#' length and file size. It also provides a method that takes random samples of
#' lines in an input text file. It provides a method that reads an input text
#' file containing token frequencies. It displays the most occuring tokens.
DataAnalyzer <- R6::R6Class(
    "DataAnalyzer",
    inherit = TextFileProcessor,
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the file name
        #' and verbose options.
        #' @param file_name The path to the input file.
        #' @param line_count The number of lines to read at a time.
        #' @param verbose If progress information should be displayed.
        initialize = function(file_name = "./data/n1.txt",
                              verbose = 0) {
            # The file name is set
            self$file_name <- file_name
            # The processed output is initialized
            self$p_output <- data.frame()
            # The verbose options is set
            self$verbose = verbose
        },

        #' @description
        #' It reads ngram token frequencies from an input text file. The ngram
        #' frequencies are then displayed in a bar plot. The type of plot is
        #' specified by the type option. 'top_features' displays the top n most
        #' occuring tokens along with their frequencies. 'coverage' displays
        #' the number of words along with their frequencies.
        #' @param opts The options for analyzing the data.
        #'   type -> The type of plot to display. The options are:
        #'     'top_features', 'coverage'.
        #'   n -> For 'top_features', it is the number of top most occuring
        #'     tokens.
        plot_data = function(opts) {
            # The opts is merged with the da_opts attribute
            private$da_opts = modifyList(private$da_opts, opts)
            # The da_opts is merged with the base class opts attribute
            self$opts = modifyList(self$opts, private$da_opts)
            # The ngram data is read
            df <- self$read_file(self$file_name, T)
            # The information message is shown
            self$display_msg("Displaying Plot...", 1)
            # If the coverage option was specified
            if (self$opts[["type"]] == "coverage") {
                # The y values
                y <- as.character(1:self$opts[["n"]])
                # The x values
                x <- numeric()
                # The percentage frequencies is calculated
                for (i in 1:self$opts[["n"]]) {
                    # The percentage of tokens with frequency i
                    x[i] <- round(100*(nrow(df[df$freq == i,])/nrow(df)), 2)
                }
                # A data frame is created
                df <- data.frame("freq" = x, "pre" = y)
                # The plot labels
                labels <- list(
                    y = "Percentage of total",
                    x = "Word Frequency",
                    title = "Coverage")

                # The chart is plotted
                private$display_plot(df, labels)
            }
            # If the top_features option was specified
            else if (self$opts[["type"]] == "top_features") {
                # The plot labels
                labels <- list(
                    y = "Frequency",
                    x = "Feature",
                    title = paste("Top", self$opts[["n"]], "Features"))

                # The chart is plotted
                private$display_plot(df, labels)
            }
        },

        #' @description
        #' Generates and returns information about the given text files.
        #' @param file_list The list of text files to check.
        #' @return A data frame containing the overall file statistics.
        get_file_info = function(file_list = list()) {
            # If the file list is empty, then the file name passed to the
            # current objet is used.
            if (length(file_list) == 0)
                file_list = list(self$file_name)

            # Empty list. Used to store information about each file
            stats <- data.frame(
                "total_line_count" = 0,
                "max_line_length" = 0,
                "min_line_length" = 0,
                "mean_line_length" = 0,
                "total_size" = 0)
            # Temporary variables for calculating max, min, mean line length
            temp_max <- temp_min <- temp_mean <- 0

            # For each file in the list
            for (file_name in file_list) {
                # The file is read
                lines <- self$read_file(file_name, F)

                # The file stats are updated
                stats["total_size"] <-
                    stats["total_size"] + file.size(file_name)
                stats["total_line_count"] <-
                    stats["total_line_count"] + length(lines)

                # The temporary variables are updated
                temp_max <- max(nchar(lines))
                temp_min <- min(nchar(lines))
                temp_mean <- mean(nchar(lines))

                if (temp_max > stats["max_line_length"])
                    stats["max_line_length"] <- temp_max
                if (temp_min > stats["min_line_length"])
                    stats["min_line_length"] <- temp_min
                if (temp_mean > stats["mean_line_length"])
                    stats["mean_line_length"] <- round(temp_mean)
            }
            # The total size is formatted
            stats["total_size"] <- utils:::format.object_size(
                stats["total_size"], "auto")

            # The required data is returned
            return(stats)
        },

        #' @description
        #' It generates training, testing and validation data sets
        #' from the given input file. It first reads the file given as a
        #' parameter to the current object. It generates random indexes for the
        #' data. It partitions the data into training, testing and validation
        #' sets, according to the given parameters. The files are named
        #' train.txt, test.txt and va.txt. The files are saved to the given
        #' output folder.
        #' @param dir The name of the output folder.
        #' @param percs The size of the training, testing and validation sets.
        generate_data = function(dir, percs) {
            # The information message is shown
            self$display_msg(
                "Generating training, testing and validation data sets...", 1)
            # If the train, test and validation files already exist
            if (file.exists(paste0(dir, "/train.txt")) &&
                file.exists(paste0(dir, "/test.txt")) &&
                file.exists(paste0(dir, "/validate.txt"))) {
                # The information message
                msg <- "The train, test and validate files already exist"
                # The information message is shown
                self$display_msg(msg, 1)
            }
            else {
                # The input file is read
                data <- self$read_file(self$file_name, F)
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
                self$write_file(train_ds, paste0(dir, "/train.txt"), F)
                # The testing data is written to file
                self$write_file(test_ds, paste0(dir, "/test.txt"), F)
                # The validation data is written to file
                self$write_file(validate_ds, paste0(dir, "/validate.txt"), F)
            }
        },

        #' @description
        #' Returns the given number of ngrams and their
        #' frequencies. If the prefix parameter is not given, then the ngrams
        #' are randomly chosen. Otherise ngrams starting with the given regular
        #' expression are returned.
        #' @param fn The ngram file name.
        #' @param c The number of ngrams to return.
        #' @param pre The ngram prefix, given as a regular expression.
        get_ngrams = function(fn, c = NULL, pre = NULL) {
            # The data is read
            df <- self$read_obj(fn)
            # If the prefix is not given
            if (is.null(pre)) {
                # The sample indexes
                i <- sample(1:nrow(df), c)
                # The ngram samples
                s <- df[i,]
            }
            else {
                # The ngram samples
                s <- df[grepl(pre, df$pre), ]
            }
            return(s)
        }
    ),

    private = list(
        # @field da_opts The options for data analyzer object.
        #   type -> The type of plot to display. The options are:
        #     'top_features', 'coverage'.
        #   n -> For 'top_features', it is the number of top most occuring
        #     tokens.
        da_opts = list(
            "type" = "top_features",
            "n" = 10
        ),

        # @description
        # Displays a plot using ggplot2. The plot is a horizontal
        # bar plot filled with red. It has the given labels and main title
        # @param df The data to plot. It is a data frame with prefix and freq
        #   columns.
        # @param labels The main title, x and y axis labels
        display_plot = function(df, labels) {
            # The freq column is converted to numeric
            df$freq <- as.numeric(df$freq)
            # The pre column is converted to character
            df$pre <- as.character(df$pre)
            # The data frame is sorted in descending order
            df <- (df[order(df$freq, decreasing = T),])
            # The top n terms are extracted
            df <- df[1:self$opts[["n"]], ]
            # The ngram names and their frequencies are plotted
            g <- ggplot(data = df, aes(x = reorder(pre, freq), y = freq)) +
                geom_bar(stat = "identity", fill = "red") +
                ggtitle(labels[["title"]]) +
                coord_flip() +
                ylab(labels[["y"]]) +
                xlab(labels[["x"]])
            print(g)
        }
    )
)
