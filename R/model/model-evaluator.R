#' It is used to perform extrinsic and intrinsic evaluation of a model.
#'
#' @description
#' It provides methods for performing extrinsic and intrinsic
#' evaluation. Intrinsic evaluation is based on calculation of Perplexity.
#' Extrinsic evaluation is based on accuracy. It involves determining the
#' percentage of correct next word predictions.
#'
#' @details
#' Before performing the intrinsic and extrinsic model evaluation, a
#' validation file must be first generated. This can be done using the Generator
#' class. Each line in the validation file is evaluated. For intrinsic
#' evaluation Perplexity for the line is calculated. An overall summary of the
#' Perplexity calculations is returned. It includes the min, max and mean
#' Perplexity. For extrinsic evaluation, next word prediction is performed on
#' each line. If the actual next word is one of the three predicted next words,
#' then the prediction is considered to be accurate. The extrinsic evaluation
#' returns the percentage of correct and incorrect predictions.
#'
#' @importFrom pryr object_size mem_change
ModelEvaluator <- R6::R6Class(
    "ModelEvaluator",
    inherit = TextFileProcessor,
    public = list(
        #' @description
        #' It initializes the current object. It is used to set the
        #' model file name and verbose options.
        #' @param mf The model file name.
        #' @param ve If progress information should be displayed.
        #' @export
        initialize = function(mf, ve = 0) {
            # The base class is initialized
            super$initialize(NULL, NULL, ve)
            # If the model file name is not valid, then an error is thrown
            if (!file.exists(mf))
                stop(paste0("Invalid model file: ", mf))
            else {
                # The model file is set
                private$mf <- mf
            }
        },

        #' @description
        #' It performs intrinsic and extrinsic evaluation for the
        #' given model. It also measures the memory usage and time taken. The
        #' performance metrics are displayed in 5 plots on one page. Performance
        #' statistics are saved to the model object.
        #' @param lc The number of lines of text in the validation file to be
        #'   used for the evaluation.
        #' @param fn The name of the validation file. If it does not exist, then
        #'   the default file validation-clean.txt is checked in the models
        #'   folder
        evaluate_performance = function(lc, fn) {
            # The performance stats
            pstats <- list("m" = NULL, "t" = NULL, "p" = NULL, "a" = NULL)
            # The time taken is checked
            tt <- system.time({
                mu <- mem_change({
                    # Intrinsic evaluation is performed
                    istats <- self$intrinsic_evaluation(lc, fn)
                    # Extrinsic evaluation is performed
                    estats <- self$extrinsic_evaluation(lc, fn)
                })
            })
            # The memory used
            mu <- private$format_size(mu)
            # The y-axis values are updated
            pstats[["m"]] <- mu
            pstats[["t"]] <- tt[[3]]
            pstats[["p"]] <- istats$mean
            pstats[["a"]] <- estats$valid_perc
            # The performance stats are saved
            private$m$pstats <- pstats
            # The model file name
            mfn <- private$m$get_config("fn")
            # The information message is shown
            private$display_msg("Saving model...", 1)
            # The model is saved
            private$save_obj(private$m, mfn)
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
        intrinsic_evaluation = function(lc, fn) {
            # The ModelPredictor class object is created
            mp <- ModelPredictor$new(private$mf, ve = private$ve)
            # The validation data is read
            data <- private$read_lines(fn, lc)
            # The list of perplexities
            pl <- c();
            # The loop counter
            c <- 1
            # The Perplexity of each sentence in the test data is calculated
            for (line in data) {
                # The line is split on space
                words <- str_split(line, " ")[[1]]
                # The perplexity for the line is calculated
                p <- mp$calc_perplexity(words)
                # The information message
                msg <- paste0(
                    "Perplexity of the sentence '", line, "' is: ", p)
                # The information message is shown
                private$display_msg(msg, 2)
                # The list of perplexities is updated
                pl <- c(pl, p);
                # If the counter is divisible by 10
                if (c %% 10 == 0) {
                    # The information message
                    msg <- paste0(c, " lines have been processed")
                    # The information message is shown
                    private$display_msg(msg, 1)
                }
                # The counter is increased by 1
                c <- c + 1
            }
            # The perplexity stats
            stats <- list(
                "min" = min(pl),
                "max" = max(pl),
                "mean" = mean(pl));

            return(stats);
        },

        #' @description
        #' Evaluates the model using extrinsic evaluation based on
        #' Accuracy. The given number of sentences are taken from the validation
        #' file. For each sentence, the model is used to predict the next word.
        #' The accuracy stats are returned. A prediction is considered to be
        #' correct if one of the predicted words matches the actual word.
        #' @param lc The number of lines of text in the validation file to be
        #'   used for the evaluation.
        #' @param fn The name of the validation file.
        #' @return The number of correct and incorrect predictions.
        extrinsic_evaluation = function(lc, fn) {
            # The ModelPredictor class object is created
            mp <- ModelPredictor$new(private$mf, ve = private$ve)
            # The Model class object is fetched
            m <- mp$get_model()
            # The TokenGenerator object options
            tg_opts <- private$m$get_config("tg_opts")

            # The validation data is read
            data <- private$read_lines(fn, lc)
            # The statistics
            stats <- list("valid" = 0, "invalid" = 0)
            # The loop counter
            c <- 1
            # The last word for each sentence is predicted
            for (line in data) {
                # The line is split on space
                words <- str_split(line, " ")[[1]]
                # The word to predict
                w <- words[length(words)]
                # The previous words used to predict the word
                pw <- words[1:length(words)-1]
                # If the words should be stemmed
                if (tg_opts[["stem_words"]]) {
                    # The previous words are stemmed
                    pw <- wordStem(pw)
                }
                # The next word is predicted
                res <- mp$predict_word(pw, F)
                # If the predicted word matches the actual word
                if (w %in% res["words"]) {
                    stats[["valid"]] <- stats[["valid"]] + 1;
                    # The information message
                    private$display_msg(
                        paste0("The word: ", w, " was predicted"), 3)
                }
                # If the predicted word does not match
                else {
                    stats[["invalid"]] <- stats[["invalid"]] + 1;
                    # The information message
                    private$display_msg(
                        paste0("The word: ", w, " could not be predicted"), 3)
                }
                # The counter is increased by 1
                c <- c + 1
                # If the counter is divisible by 10
                if (c %% 10 == 0) {
                    # The information message
                    msg <- paste0(c, " sentences have been processed")
                    # The information message is shown
                    private$display_msg(msg, 1)
                }
            }

            # The precentage of valid
            stats[["valid_perc"]] <-
                (stats[["valid"]]/(stats[["valid"]] + stats[["invalid"]]))*100
            # The precentage of invalid
            stats[["invalid_perc"]] <- 100-stats[["valid_perc"]]

            return(stats)
        }
    ),

    private = list(
        # @field mf The model file name.
        mf = NULL
    )
)
