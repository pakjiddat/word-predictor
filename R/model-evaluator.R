#' Evaluates performance of n-gram models
#'
#' @description
#' It provides methods for performing extrinsic and intrinsic
#' evaluation of a n-gram model. It also provides a method for comparing
#' performance of multiple n-gram models.
#'
#' Intrinsic evaluation is based on calculation of Perplexity. Extrinsic
#' evaluation involves determining the percentage of correct next word
#' predictions.
#'
#' @details
#' Before performing the intrinsic and extrinsic model evaluation, a validation
#' file must be first generated. This can be done using the DataSampler class.
#'
#' Each line in the validation file is evaluated. For intrinsic evaluation
#' Perplexity for the line is calculated. An overall summary of the Perplexity
#' calculations is returned. It includes the min, max and mean Perplexity.
#'
#' For extrinsic evaluation, next word prediction is performed on each line. If
#' the actual next word is one of the three predicted next words, then the
#' prediction is considered to be accurate. The extrinsic evaluation returns the
#' percentage of correct and incorrect predictions.
#' @importFrom patchwork plot_annotation
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth coord_cartesian labs
ModelEvaluator <- R6::R6Class(
    "ModelEvaluator",
    inherit = Base,
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the
        #' model file name and verbose options.
        #' @param mf The model file name.
        #' @param ve The level of detail in the information messages.
        #' @export
        initialize = function(mf = NULL, ve = 0) {
            # The base class is initialized
            super$initialize(NULL, NULL, ve)
            # If the model file is not NULL
            if (!is.null(mf)) {
                # If the model file name is not valid, then an error is thrown
                if (!file.exists(mf)) {
                    private$dm("Invalid model file: ", mf, md = -1, ty = "e")
                } else {
                    # The model file is set
                    private$mf <- mf
                    # The ModelPredictor class object is created
                    mp <- ModelPredictor$new(private$mf, ve = private$ve)
                    # The ModelPredictor object is set
                    private$mp <- mp
                }
            }
        },

        #' @description
        #' It compares the performance of the models in the given folder.
        #'
        #' The performance of the model is compared for the 4 metric which are
        #' time taken, memory used, Perplexity and accuracy. The performance
        #' comparison is displayed on plots.
        #'
        #' 4 plots are displayed. One for each performance metric. A fifth plot
        #' shows the variation of Perplexity with accuracy. All 5 plots are
        #' plotted on one page.
        #' @param opts The options for comparing model performance.
        #' * **save_to**. The graphics device to save the plot to.
        #'     NULL implies plot is printed.
        #' * **dir**. The directory containing the model file, plot and stats.
        #' @examples
        #' # Start of environment setup code
        #' # The level of detail in the information messages
        #' ve <- 0
        #' # The name of the folder that will contain all the files. It will be
        #' # created in the current directory. NULL implies tempdir will be
        #' # used.
        #' fn <- NULL
        #' # The required files. They are default files that are part of the
        #' # package
        #' rf <- c("def-model.RDS")
        #' # An object of class EnvManager is created
        #' em <- EnvManager$new(ve = ve, rp = "./")
        #' # The required files are downloaded
        #' ed <- em$setup_env(rf, fn)
        #' # End of environment setup code
        #
        #' # ModelEvaluator class object is created
        #' me <- ModelEvaluator$new(ve = ve)
        #' # The performance evaluation is performed
        #' me$compare_performance(opts = list(
        #'     "save_to" = NULL,
        #'     "dir" = ed
        #' ))
        #'
        #' # The test environment is removed. Comment the below line, so the
        #' # files generated by the function can be viewed
        #' em$td_env()
        compare_performance = function(opts) {
            # A data frame containing the combined performance stats
            cps <- data.frame()
            # The list of model files in the given directory
            fl <- dir(opts[["dir"]], full.names = T)
            # Each model file in the directory is read
            for (fn in fl) {
                # If the file name does not contain .RDS
                if (!endsWith(fn, ".RDS")) next
                # The model file is read
                m <- private$read_obj(fn)
                # The performance stats for the model
                pstats <- m$pstats
                # The memory used by the object is formated
                mu <- pstats$m / (10^6)
                # The temporary performance stats
                tstats <- data.frame(
                    "n" = m$name,
                    "m" = mu,
                    "t" = pstats$t,
                    "p" = pstats$p,
                    "a" = pstats$a
                )
                # The combined performance stats are updated
                cps <- rbind(cps, tstats)
            }
            # The combined performance stats are plotted
            g <- self$plot_stats(cps)
            # If the save_to and dir options are not NULL
            if (!is.null(opts[["save_to"]])) {
                # The file name for the plot
                fn <- paste0("performance.", opts[["save_to"]])
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
            } else {
                # The plot is printed
                print(g)
            }
            # If the directory path was given
            if (dir.exists(opts[["dir"]])) {
                # The performance stats file name
                fn <- paste0(opts[["dir"]], "/pstats.RDS")
                # The combined performance stats are save
                private$save_obj(cps, fn)
            }
        },

        #' @description
        #' It plots the given stats on 5 plots. The plots are displayed on a
        #' single page.
        #'
        #' The 4 performance metrics which are time taken, memory, Perplexity
        #' and accuracy are plotted against the model name. Another plot
        #' compares Perplexity with accuracy for each model.
        #' @param data The data to plot
        #' @return The ggplot object is returned.
        plot_stats = function(data) {
            # The information message is shown
            private$dm("Plotting performance stats", md = 1)

            # The x values. Each value in the range is the model number
            x_vals <- seq_len(length(data$n))
            # The data frames
            df1 <- data.frame(x = x_vals, y = data$m)
            df2 <- data.frame(x = x_vals, y = data$t)
            df3 <- data.frame(x = x_vals, y = data$p)
            df4 <- data.frame(x = x_vals, y = data$a)
            df5 <- data.frame(x = data$a, y = data$p)
            # The options for plot 1
            popts <- list(
                "x_lab" = "model",
                "y_lab" = "memory"
            )
            # Plot 1
            p1 <- private$plot_graph(df1, popts)
            # The options for plot 2
            popts <- list(
                "x_lab" = "model",
                "y_lab" = "time"
            )
            # Plot 2
            p2 <- private$plot_graph(df2, popts)
            # The options for plot 3
            popts <- list(
                "x_lab" = "model",
                "y_lab" = "perplexity"
            )
            # Plot 3
            p3 <- private$plot_graph(df3, popts)
            # The options for plot 4
            popts <- list(
                "x_lab" = "model",
                "y_lab" = "accuracy"
            )
            # Plot 4
            p4 <- private$plot_graph(df4, popts)
            # The options for plot 5
            popts <- list(
                "x_lab" = "accuracy",
                "y_lab" = "perplexity"
            )
            # Plot 5
            p5 <- private$plot_graph(df5, popts)
            # The plots are displayed on a single page
            patchwork <- p1 + p2 + p3 + p4 + p5
            # The model names
            mn <- paste0(data$n, collapse = ", ")
            # The subtitle
            st <- paste0(
                "The performance of following models is compared: ", mn
            )
            # Main title is added
            p <- (patchwork + plot_annotation(
                "title" = "Performance comparison of n-gram models",
                "subtitle" = st
            ))
            # The information message is shown
            private$dm(" \u2714\n", md = 1)

            return(p)
        },


        #' @description
        #' It performs intrinsic and extrinsic evaluation for the given model
        #' and validation text file. The given number of lines in the validation
        #' file are used in the evaluation
        #'
        #' It performs two types of evaluations. One is intrinsic evaluation,
        #' based on Perplexity, the other is extrinsic evaluation based on
        #' accuracy.
        #'
        #' It returns the results of evaluation. 4 evaluation metrics are
        #' returned. Perplexity, accuracy, memory and time taken. Memory is the
        #' size of the model object. Time taken is the time needed for
        #' performing both evaluations.
        #'
        #' The results of the model evaluation are saved within the model object
        #' and also returned.
        #' @param lc The number of lines of text in the validation file to be
        #'   used for the evaluation.
        #' @param fn The name of the validation file. If it does not exist, then
        #'   the default file validation-clean.txt is checked in the models
        #'   folder
        #' @return The performance stats are returned.
        #' @examples
        #' # Start of environment setup code
        #' # The level of detail in the information messages
        #' ve <- 0
        #' # The name of the folder that will contain all the files. It will be
        #' # created in the current directory. NULL implies tempdir will be used
        #' fn <- NULL
        #' # The required files. They are default files that are part of the
        #' # package
        #' rf <- c("def-model.RDS", "validate-clean.txt")
        #' # An object of class EnvManager is created
        #' em <- EnvManager$new(ve = ve, rp = "./")
        #' # The required files are downloaded
        #' ed <- em$setup_env(rf, fn)
        #' # End of environment setup code
        #'
        #' # The model file name
        #' mfn <- paste0(ed, "/def-model.RDS")
        #' # The validation file name
        #' vfn <- paste0(ed, "/validate-clean.txt")
        #'
        #' # ModelEvaluator class object is created
        #' me <- ModelEvaluator$new(mf = mfn, ve = ve)
        #' # The performance evaluation is performed
        #' stats <- me$evaluate_performance(lc = 20, fn = vfn)
        #' # The evaluation stats are printed
        #' print(stats)
        #'
        #' # The test environment is removed. Comment the below line, so the
        #' # files generated by the function can be viewed
        #' em$td_env()
        evaluate_performance = function(lc, fn) {
            # The information message is shown
            private$dh("Evaluating model performance", "-", md = 1)
            # The Model class object is fetched
            m <- private$mp$get_model()
            # The performance stats
            pstats <- list("m" = NULL, "t" = NULL, "p" = NULL, "a" = NULL)
            # The time taken is checked
            tt <- system.time({
                # Intrinsic evaluation is performed
                istats <- self$intrinsic_evaluation(lc, fn)
                # Extrinsic evaluation is performed
                estats <- self$extrinsic_evaluation(lc, fn)
            })

            # The y-axis values are updated
            pstats[["m"]] <- m$get_size()
            pstats[["t"]] <- tt[[3]]
            pstats[["p"]] <- istats$mean
            pstats[["a"]] <- estats$valid_perc

            # The performance stats are saved
            m$pstats <- pstats
            # The information message is shown
            private$dm("Saving stats to model file\n", md = 1)
            # The model is saved
            private$save_obj(m, private$mf)
            # The information message is shown
            private$dh("DONE", "=", md = 1)
            # The performance stats are returned
            return(pstats)
        },

        #' @description
        #' Evaluates the model using intrinsic evaluation based on
        #' Perplexity. The given number of sentences are taken from the
        #' validation file. For each sentence, the Perplexity is calculated.
        #' @param lc The number of lines of text in the validation file to be
        #'   used for the evaluation.
        #' @param fn The name of the validation file. If it does not exist, then
        #'   the default file validation-clean.txt is checked in the models
        #'   folder
        #' @return The min, max and mean Perplexity score.
        #' @examples
        #' # Start of environment setup code
        #' # The level of detail in the information messages
        #' ve <- 0
        #' # The name of the folder that will contain all the files. It will be
        #' # created in the current directory. NULL implies tempdir will be used
        #' fn <- NULL
        #' # The required files. They are default files that are part of the
        #' # package
        #' rf <- c("def-model.RDS", "validate-clean.txt")
        #' # An object of class EnvManager is created
        #' em <- EnvManager$new(ve = ve, rp = "./")
        #' # The required files are downloaded
        #' ed <- em$setup_env(rf, fn)
        #' # End of environment setup code
        #'
        #' # The model file name
        #' mfn <- paste0(ed, "/def-model.RDS")
        #' # The validation file name
        #' vfn <- paste0(ed, "/validate-clean.txt")
        #'
        #' # ModelEvaluator class object is created
        #' me <- ModelEvaluator$new(mf = mfn, ve = ve)
        #' # The intrinsic evaluation is performed
        #' stats <- me$intrinsic_evaluation(lc = 20, fn = vfn)
        #' # The evaluation stats are printed
        #' print(stats)
        #'
        #' # The test environment is removed. Comment the below line, so the
        #' # files generated by the function can be viewed
        #' em$td_env()
        intrinsic_evaluation = function(lc, fn) {
            # The information message is shown
            private$dh("Performing intrinsic evaluation", "-", md = 1)
            # The validation data is read
            data <- private$read_lines(fn, lc)
            # The list of perplexities
            pl <- c()
            # The loop counter
            c <- 1
            # The Perplexity of each sentence in the test data is calculated
            for (line in data) {
                # The line is split on space
                words <- strsplit(line, " ")[[1]]
                # The perplexity for the line is calculated
                p <- private$mp$calc_perplexity(words)
                # The information message
                msg <- paste0(
                    "Perplexity of the sentence '", line, "' is: ", p, "\n"
                )
                # The information message is shown
                private$dm(msg, md = 2)
                # The list of perplexities is updated
                pl <- c(pl, p)
                # If the counter is divisible by 10
                if (c %% 10 == 0) {
                    # The information message is shown
                    private$dm(c, " lines have been processed\n", md = 1)
                }
                # The counter is increased by 1
                c <- c + 1
            }
            # The perplexity stats
            stats <- list(
                "min" = min(pl),
                "max" = max(pl),
                "mean" = mean(pl)
            )
            # The information message is shown
            private$dh("DONE", "=", md = 1)

            return(stats)
        },

        #' @description
        #' Evaluates the model using extrinsic evaluation based on
        #' Accuracy. The given number of sentences are taken from the validation
        #' file.
        #'
        #' For each sentence, the model is used to predict the next word.
        #' The accuracy stats are returned. A prediction is considered to be
        #' correct if one of the predicted words matches the actual word.
        #' @param lc The number of lines of text in the validation file to be
        #'   used for the evaluation.
        #' @param fn The name of the validation file.
        #' @return The number of correct and incorrect predictions.
        #' @examples
        #' # Start of environment setup code
        #' # The level of detail in the information messages
        #' ve <- 0
        #' # The name of the folder that will contain all the files. It will be
        #' # created in the current directory. NULL implies tempdir will be used
        #' fn <- NULL
        #' # The required files. They are default files that are part of the
        #' # package
        #' rf <- c("def-model.RDS", "validate-clean.txt")
        #' # An object of class EnvManager is created
        #' em <- EnvManager$new(ve = ve, rp = "./")
        #' # The required files are downloaded
        #' ed <- em$setup_env(rf, fn)
        #' # End of environment setup code
        #'
        #' # The model file name
        #' mfn <- paste0(ed, "/def-model.RDS")
        #' # The validation file name
        #' vfn <- paste0(ed, "/validate-clean.txt")
        #'
        #' # ModelEvaluator class object is created
        #' me <- ModelEvaluator$new(mf = mfn, ve = ve)
        #' # The intrinsic evaluation is performed
        #' stats <- me$extrinsic_evaluation(lc = 100, fn = vfn)
        #' # The evaluation stats are printed
        #' print(stats)
        #'
        #' # The test environment is removed. Comment the below line, so the
        #' # files generated by the function can be viewed
        #' em$td_env()
        extrinsic_evaluation = function(lc, fn) {
            # The information message is shown
            private$dh("Performing extrinsic evaluation", "-", md = 1)
            # The Model class object is fetched
            m <- private$mp$get_model()
            # The TokenGenerator object options
            tg_opts <- m$get_config("tg_opts")

            # The validation data is read
            data <- private$read_lines(fn, lc)
            # The statistics
            stats <- list("valid" = 0, "invalid" = 0)
            # The loop counter
            c <- 1
            # The last word for each sentence is predicted
            for (line in data) {
                # The line is split on space
                words <- strsplit(line, " ")[[1]]
                # The word to predict
                w <- words[length(words)]
                # The previous words used to predict the word
                pw <- words[seq_len(length(words) - 1)]
                # If the words should be stemmed
                if (tg_opts[["stem_words"]]) {
                    # The previous words are stemmed
                    pw <- wordStem(pw)
                }
                # The next word is predicted
                res <- private$mp$predict_word(pw, F)
                # If the predicted word matches the actual word
                if (w %in% res["words"]) {
                    stats[["valid"]] <- stats[["valid"]] + 1
                    # The information message
                    private$dm(
                        "The word: ", w, " was predicted\n",
                        md = 3
                    )
                } else { # If the predicted word does not match
                    stats[["invalid"]] <- stats[["invalid"]] + 1
                    # The information message
                    private$dm(
                        "The word: ", w, " could not be predicted\n",
                        md = 3
                    )
                }
                # The counter is increased by 1
                c <- c + 1
                # If the counter is divisible by 10
                if (c %% 10 == 0) {
                    # The information message is shown
                    private$dm(
                        c, " sentences have been processed\n",
                        md = 1
                    )
                }
            }

            # The valid stats
            v <- stats[["valid"]]
            # The invalid stats
            i <- stats[["invalid"]]

            # The precentage of valid
            stats[["valid_perc"]] <- (v / (v + i)) * 100
            # The precentage of invalid
            stats[["invalid_perc"]] <- 100 - stats[["valid_perc"]]

            # The information message is shown
            private$dh("DONE", "=", md = 1)

            return(stats)
        }
    ),
    private = list(
        # @field mf The model file name.
        mf = NULL,
        # @field mp The ModelPredictor class object.
        mp = NULL,

        # @description
        # It creates a single plot based on ggplot2.
        # @param data A data frame containing the data to be plotted. It should
        #   have 2 variables, x and y.
        # @param opts The options for plotting the data. It contains:
        # * **x_lab**. The x-axis label.
        # * **y_lab**. The y-axis label.
        # @return A ggplot object representing the plot.
        plot_graph = function(data, opts) {
            # y-max
            y_max <- max(data$y)
            # The graph is plotted
            p <- ggplot(data, aes(x, y)) +
                geom_point() +
                geom_smooth(method = "lm", formula = y ~ x) +
                labs(x = opts[["x_lab"]], y = opts[["y_lab"]]) +
                coord_cartesian(ylim = c(0, y_max))
            return(p)
        }
    )
)
