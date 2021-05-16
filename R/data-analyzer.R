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
#' @importFrom ggplot2 ggplot geom_bar ggtitle coord_flip ylab xlab aes, ggsave
DataAnalyzer <- R6::R6Class(
    "DataAnalyzer",
    inherit = TextFileProcessor,
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the file name
        #' and verbose options.
        #' @param fn The path to the input file.
        #' @param ve If progress information should be displayed.
        #' @export
        initialize = function(fn = NULL, ve = 0) {
            # The file name is set
            private$fn <- fn
            # The processed output is initialized
            private$p_output <- data.frame()
            # The verbose options is set
            private$ve = ve
        },

        #' @description
        #' It reads ngram token frequencies from an input text
        #' file. The ngram frequencies are then displayed in a bar plot. The
        #' type of plot is specified by the type option. 'top_features' displays
        #' the top n most occuring tokens along with their frequencies.
        #' 'coverage' displays the number of words along with their frequencies.
        #' The plot stats are returned by the function as a data frame.
        #' @param opts The options for analyzing the data.
        #' type -> The type of plot to display. The options are:
        #'   'top_features', 'coverage'.
        #' n -> For 'top_features', it is the number of top most occuring
        #'   tokens. For 'coverage' it is the first n frequencies.
        #' save_to -> The graphics devices to save the plot to.
        #'   NULL implies plot is printed.
        #' dir -> The output directory where the plot will be saved.
        #' @return A data frame containing the stats.
        plot_n_gram_stats = function(opts) {
            # The opts is merged with the da_opts attribute
            private$da_opts = modifyList(private$da_opts, opts)
            # The da_opts is merged with the base class opts attribute
            private$opts = modifyList(private$opts, private$da_opts)
            # The ngram data is read
            df <- private$read_obj(private$fn)
            # The information message is shown
            private$display_msg("Displaying Plot...", 1)
            # If the coverage option was specified
            if (private$opts[["type"]] == "coverage") {
                # The y values
                y <- as.character(1:private$opts[["n"]])
                # The x values
                x <- numeric()
                # The percentage frequencies is calculated
                for (i in 1:private$opts[["n"]]) {
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
            }
            # If the top_features option was specified
            else if (private$opts[["type"]] == "top_features") {
                # The plot labels
                labels <- list(
                    y = "Frequency",
                    x = "Feature",
                    title = paste("Top", private$opts[["n"]], "Features"))
            }
            # The freq column is converted to numeric
            df$freq <- as.numeric(df$freq)
            # The pre column is converted to character
            df$pre <- as.character(df$pre)
            # The data frame is sorted in descending order
            df <- (df[order(df$freq, decreasing = T),])
            # The top n terms are extracted
            df <- df[1:private$opts[["n"]], ]
            # The chart is plotted
            g <- private$display_plot(df, labels)

            # If the save_to and dir options are not NULL
            if (!is.null(opts[["save_to"]]) && !is.null(opts[["dir"]])) {
                # The file name for the plot
                fn <- paste0(opts[["type"]], ".", opts[["save_to"]])
                # The plot object is saved
                ggsave(
                    filename = fn,
                    plot = g,
                    device = opts[["save_to"]],
                    path = opts[["dir"]],
                    width = 7,
                    height = 7,
                    units = "in"
                )
            }
            else {
                # The plot is printed
                print(g)
            }

            return(df)
        },

        #' @description
        #' Generates and returns information about text files.
        #' @param res The name of a directory or a file name.
        #' @return A data frame containing the file statistics.
        get_file_info = function(res) {
            # The list of files to check
            file_list <- NULL
            # If a directory name was passed
            if (dir.exists(res)) {
                # All files in the directory are fetched
                file_list = dir(res, full.names = T)
            }
            # If a file name was passed
            else if (file.exists(res)) {
                # The file name is set
                file_list <- res
            }

            # Used to store overall information about files
            ostats <- data.frame(
                "total_lc" = 0,
                "max_ll" = 0,
                "min_ll" = 0,
                "mean_ll" = 0,
                "total_s" = 0
            )

            # Used to store information about each file
            fstats <- tstats <- data.frame()

            # Temporary variables for calculating max, min, mean line length
            temp_max <- temp_min <- temp_mean <- 0

            # For each file in the list
            for (fn in file_list) {
                # The file is read
                lines <- private$read_file(fn, F)
                # The line count
                lc <- length(lines)
                # The file size
                size <- file.size(fn)
                # The file stats are updated
                ostats[["total_s"]] <- ostats[["total_s"]] + size
                ostats[["total_lc"]] <- ostats[["total_lc"]] + lc

                # The temporary variables are updated
                temp_max <- max(nchar(lines))
                temp_min <- min(nchar(lines))
                temp_mean <- round(mean(nchar(lines)))

                # The file stats are updated
                tstats <- data.frame(
                    "fn" = fn,
                    "total_lc" = lc,
                    "max_ll" = temp_max,
                    "min_ll" = temp_min,
                    "mean_ll" = temp_mean,
                    "size" = size
                )
                # The size is formatted
                tstats["size"] <-
                    utils:::format.object_size(tstats["size"], "auto")

                # The file stats are appended
                fstats <- rbind(fstats, tstats)

                if (temp_max > ostats["max_ll"])
                    ostats["max_ll"] <- temp_max
                if (temp_min > ostats["min_ll"])
                    ostats["min_ll"] <- temp_min
                if (temp_mean > ostats["mean_ll"])
                    ostats["mean_ll"] <- temp_mean
            }
            # The total size is formatted
            ostats["total_s"] <-
                utils:::format.object_size(ostats["total_s"], "auto")

            # The required stats
            stats = list("file_stats" = fstats, "overall_stats" = ostats)
            # The required stats are returned
            return(stats)
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
            df <- private$read_obj(fn)
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
        # @param labels The main title, x and y axis labels.
        # @return The ggplot object is returned.
        display_plot = function(df, labels) {
            # The ngram names and their frequencies are plotted
            g <- ggplot(data = df, aes(x = reorder(pre, freq), y = freq)) +
                geom_bar(stat = "identity", fill = "red") +
                ggtitle(labels[["title"]]) +
                coord_flip() +
                ylab(labels[["y"]]) +
                xlab(labels[["x"]])
            return(g)
        }
    )
)
