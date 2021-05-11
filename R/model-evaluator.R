#' It is used to evaluate the accuracy and performance of the model.
#'
#' @description
#' It provides methods that perform extrinsic and intrinsic model
#' evaluation. It also provides methods for determining the memory and time
#' requirements for generating the model. It also provides a method for
#' determining how much memory is used by the final model.
#'
#' @details
#' It provides a method that performs intrinsic model evaluation based
#' on Perplexity. It also provides a method that performs extrinsic model
#' evalation based on accuracy. It provides a method for determining how much
#' memory and time is needed to generate a model for different input data
#' sizes. It provides a method for determining how much memory is needed by
#' the final model.
ModelEvaluator <- R6::R6Class(
    "ModelEvaluator",
    inherit = TextFileProcessor,
    public = list(
        #' @field tp The transition probabilities data frame.
        tp = NULL,

        #' @field wl The list of unique words.
        wl = NULL,

        #' @field dp The default probability is equal to 1/(N+V), where N is the
        #'   number of words in the sentence, V is the number of words in the
        #'   vocabulary.
        dp = NULL,

        #' @field model The maximum number of ngrams supported by the model.
        model = 4,

        #' @field dc_opts The options for the data cleaner object.
        #'   min_words -> The minimum number of words per sentence.
        #'   line_count -> The number of lines to read and clean at a time.
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
        dc_opts = list(
            "min_words" = 2,
            "line_count" = 1000,
            "sw_file" = "./data/stop-words.txt",
            "dict_file" = "./data/dict-no-bad.txt",
            "bad_file" = "./data/bad-words.txt",
            "to_lower" = T,
            "remove_stop" = F,
            "remove_punc" = T,
            "remove_non_dict" = T,
            "remove_non_alpha" = T,
            "remove_extra_space" = T,
            "remove_bad" = F
        ),

        #' @field tg_opts The options for the token generator obj.
        #'   n -> The ngram size.
        #'   save_ngrams -> If the ngram data should be saved.
        #'   min_freq -> All ngrams with frequency less than min_freq are
        #'     ignored.
        #'   line_count -> The number of lines to process at a time.
        #'   stem_words -> If words should be converted to their stem.
        #'   dir -> The dir where the output file should be saved.
        #'   format -> The format for the output. There are two options.
        #'     'plain' -> The data is stored in plain text.
        #'     'obj' -> The data is stored as a R obj.
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

        #' @field ssize The sample size in Mb.
        ssize = 30,

        #' @field ddir The folder containing the data files
        ddir = "./data",

        #' @field mdir The folder containing the model files
        mdir = "./data/models",

        #' @description
        #' It initializes the current object. It is used to set the
        #' maximum ngram number, sample size, input file name, data cleaner
        #' options and verbose option.
        #' @param model The maximum ngram number supported by the model.
        #' @param ssize The sample size in Mb.
        #' @param ddir The data directory.
        #' @param mdir The model directory.
        #' @param dc_opts The data cleaner options.
        #' @param tg_opts The token generator options.
        #' @param verbose If progress information should be displayed.
        initialize = function(model = 4,
                              ssize = 30,
                              ddir = "./data",
                              mdir = "./data/models",
                              dc_opts = list(),
                              tg_opts = list(),
                              verbose = 0) {

            # The base class is initialized
            super$initialize(NULL, NULL, verbose)

            # The model number is set
            self$model <- model
            # The sample size is set
            self$ssize <- ssize
            # The data directory name is set
            self$ddir <- ddir
            # The model directory name is set
            self$mdir <- mdir
            # If the dc_opts are given
            if (length(dc_opts) > 0) {
                # The custom dc_opts are merged with the default dc_opts
                self$dc_opts = modifyList(self$dc_opts, dc_opts)
            }
            # If the tg_opts are given
            if (length(tg_opts) > 0) {
                # The custom tg_opts are merged with the default tg_opts
                self$tg_opts = modifyList(self$tg_opts, tg_opts)
            }
            # The transition probabilities data is initialized
            self$tp <- NULL
        },

        #' @description
        #' It compares the performance of the specified models by
        #' plotting the performance statistics. The models are specified with
        #' the type parameter.
        #' @param type The models to compare. It can be:
        #' 'basic' -> One model for each ngram.
        #' 'grouped' -> One model for each group and ngram. For
        #'   e.g For data size 5 Mb, there are 3 models for the ngrams 2:4.
        #' @param opts The options for plotting the data.
        #'   'group' -> The field to group by.
        #'   'title' -> The main plot title.
        #'   'subtitle' -> The plot sub title.
        #' @return The performance stats.
        performance_comparision = function(type, opts) {
            # If the type is 'basic'
            if (type == 'basic') {
                # The pstats file name
                fn <- paste0(self$mdir, "/pstats.RDS")
                # The stats are read
                pstats <- self$read_obj(fn)
                # The config file name
                fn <- paste0(self$mdir, "/config.RDS")
                # The config file is read
                config <- self$read_obj(fn)
                # The y-axis values
                y <- list("m" = NULL, "t" = NULL, "p" = NULL, "a" = NULL)
                # For each model the performance metrics are measured
                for (i in 1:(config$model-1)) {
                    # The memory value
                    m <- self$format_size(pstats[[i]][["memory"]])
                    # The y-axis values are updated
                    y[["m"]] <- c(y[["m"]],  m)
                    y[["t"]] <- c(y[["t"]], pstats[[i]][["time"]])
                    y[["p"]] <- c(y[["p"]],
                                  pstats[[i]][["intrinsic"]][["mean"]])
                    y[["a"]] <- c(y[["a"]],
                                  pstats[[i]][["extrinsic"]][["valid_perc"]])
                }
                # The data frame containing the data to be plotted
                df <- data.frame(
                    "n" = 2:(length(pstats) + 1),
                    "m" = y[["m"]],
                    "t" = y[["t"]],
                    "p" = y[["p"]],
                    "a" = y[["a"]]
                )
                # The options for plotting
                opts <- list(
                    "type" = "basic",
                    "title" = "Variation of performance with ngram size",
                    "subtitle" = opts[['subtitle']])
            }
            # If the type is 'grouped'
            else if (type == 'grouped') {
                # The average performance stats for each data size
                y <- list("m" = NULL, "t" = NULL, "p" = NULL, "a" = NULL)
                # The different items in the group. Each folder is a group item
                f <- list.dirs(self$mdir, recursive = F, full.names = F)
                # The output data frame
                df <- data.frame()
                # For each folder, the performance stats for all ngram models is
                # calculated.
                for (i in f) {
                    # The performance stats file name
                    fn <- paste0(self$mdir, "/", i, "/pstats.RDS")
                    # The stats are read
                    pstats <- self$read_obj(fn)
                    # The config file name
                    fn <- paste0(self$mdir, "/", i, "/config.RDS")
                    # The stats are read
                    config <- self$read_obj(fn)
                    # The memory usage for each ngram
                    m <- private$process_stats(pstats, "memory", config$model)
                    # The time taken for each ngram
                    t <- private$process_stats(pstats, "time", config$model)
                    # The perplexity for each ngram
                    p <- private$process_stats(pstats, "perplexity", config$model)
                    # The accuracy score for each ngram
                    a <- private$process_stats(pstats, "accuracy", config$model)
                    # The temprary data frame for the current data size
                    tdf <- data.frame(
                        rep(i, (config$model-1)),
                        m, t, p, a, 2:(length(pstats) + 1))
                    # The column names are set
                    names(tdf) <- c(opts[["group"]], "m", "t", "p", "a", "n")
                    # The temprary data frame is appended to the output data
                    # frame
                    df <- rbind(df, tdf)
                }
                # The group name
                g <- opts[["group"]]
                # The options for plotting
                opts <- list(
                    "type" = "grouped",
                    "group" = g,
                    "title" = opts[['title']],
                    "subtitle" = opts[['subtitle']])
                # If the group column is numeric
                if (is.numeric(as.numeric(as.character(df[[g]])))) {
                    # The group column is converted to numeric
                    df[[g]] <- as.numeric(as.character(df[[g]]))
                    # The data is ordered by group
                    df <- df[order(df[[opts[["group"]]]]), ]
                    # The group column is converted to factor
                    df[[g]] <- as.factor(df[[g]])
                }
            }
            # The performance stats are plotted
            private$plot_stats(df, opts)
            # The performance stats are returned
            return(df)
        },

        #' @description
        #' For each model it performs intrinsic and extrinsic
        #' evaluation. It also measures the memory usage and time taken. The
        #' performance metrics are displayed in 5 plots on one page. Performance
        #' statistics are saved to the file pstats.RDS.
        performance_evaluation = function() {
            # The performance stats
            pstats <- list()
            # The maximum model number
            mmax <- self$model
            # The y-axis values
            y <- list("m" = NULL, "t" = NULL, "p" = NULL, "a" = NULL)
            # For each model the performance metrics are measured
            for (m in 2:mmax) {
                # The transition probabilities data is initialized
                self$tp <- NULL
                # The model number is set
                self$model <- m
                # The intrinsic and extrinsic evaluation is performed
                time_taken <- system.time({
                    memory_used <- mem_change({
                        # intrinsic evaluation is performed
                        istats <- self$intrinsic_evaluation()
                        # Extrinsic evaluation is performed
                        estats <- self$extrinsic_evaluation()
                    })
                })
                # The memory used
                memory_used <- object_size(self$tp)
                # The performance stats are updated
                pstats[[m-1]] <- list(
                    "intrinsic" = istats,
                    "extrinsic" = estats,
                    "memory" = memory_used,
                    "time" = time_taken[[3]]
                )
                # The y-axis values are updated
                y[["m"]] <- c(y[["m"]], self$format_size(memory_used))
                y[["t"]] <- c(y[["t"]], time_taken[[3]])
                y[["p"]] <- c(y[["p"]], istats$mean)
                y[["a"]] <- c(y[["a"]], estats$valid_perc)
            }

            # The information message is shown
            self$display_msg("Saving model performance stats...", 1)
            # The performance stats file name
            fn <- paste0(self$mdir, "/pstats.RDS")
            # The performance stats are saved
            self$save_obj(pstats, fn)
            # The data to be plotted
            df <- data.frame(
                "n" = 2:(length(pstats)+1),
                "m" = y[["m"]],
                "t" = y[["t"]],
                "p" = y[["p"]],
                "a" = y[["a"]]
            )
            # The options for plotting
            opts <- list("type" = "basic")
            # The performance stats are plotted
            private$plot_stats(df, opts)
        },

        #' @description
        #' It loads the given model configuration file.
        #' @param fn The config file name.
        load_config = function(fn) {
            # The model configuration is read
            config <- self$read_obj(fn)
            # The configuration is copied to the object attributes
            self$model <- config$model
            self$ssize <- config$ssize
            self$dc_opts <- config$dc_opts
            self$ddir <- config$ddir
            self$mdir <- config$mdir
        },

        #' @description
        #' It loads the model located at the mdir location
        load_model = function() {
            # The configuration file name
            fn <- paste0(self$mdir, "/config.RDS")
            # The model configuration is loaded to the current object
            self$load_config(fn)
            # The tp data for the specified ngrams is loaded
            private$read_tp_data(self$model)
        },

        #' @description
        #' It saves the model configuration to the models
        #' subdirectory of the given directory.
        save_config = function() {
            # The information message is shown
            self$display_msg("Saving model configuration...", 1)
            # The model configuration
            config <- list(
                "model" = self$model,
                "ssize" = self$ssize,
                "ddir" = self$ddir,
                "mdir" = self$mdir,
                "dc_opts" = self$dc_opts,
                "tg_opts" = self$tg_opts
            )
            # The configuration file
            fn <- paste0(self$mdir, "/config.RDS")
            # The configuration is saved
            self$save_obj(config, fn)
        },

        #' @description
        #' It generates the model for the given ngram number,
        #' data sample size, data cleaning options and input file.
        generate_model = function() {
            # The data analyzer object is created
            da <- DataAnalyzer$new(
                paste0(self$ddir, "/train.txt"), self$verbose)
            # The training, testing and validation data sets are generated
            da$generate_data(self$ddir,
                             list(train = .8, test = .1, validate = .1))
            # The object size is formatted
            obj_size <- file.size(paste0(self$ddir, "/train.txt"))/10^6
            # The proportion of data to sample
            prop <- (self$ssize/obj_size)
            # Random sample is taken and cleaned
            self$generate_clean_sample(prop, T, 'tr')
            # The model directory is set
            self$tg_opts$dir <- self$mdir
            # For each ngram number, the ngram token file is generated
            for (i in 1:self$model) {
                # The clean train data file name
                fn <- paste0(self$mdir, "/train-clean.txt")
                # The ngram number is set
                self$tg_opts$n <- i
                # The TokenGenerator object is created
                tg <- TokenGenerator$new(fn, self$tg_opts, self$verbose)
                # The ngram tokens are generated
                tg$generate_tokens()
            }
            # For each ngram number, the transition probabilities are generated
            for (i in 2:self$model) {
                # The TPGenerator object is created
                tp <- TPGenerator$new()
                tp_opts <- list(
                    "n_range" = 1:i,
                    "save_tp" = T,
                    "format" = "obj",
                    "dir" = self$mdir
                )
                # The transition probabilities are generated
                tp$generate_tp(tp_opts)
            }
            # The model config is saved
            self$save_config()
        },

        #' @description
        #' Evaluates the model using intrinsic evaluation based on
        #' Perplexity. First a validation data set containing 1000 lines is
        #' generated. It is then cleaned. 20 random sentences are taken. For
        #' each sentence, Perplexity of all the words is calculated.
        #' @return The minumum, maximum and mean Perplexity score.
        intrinsic_evaluation = function() {
            # The transition probabilities data is read
            private$read_tp_data(self$model)
            # Random sample is taken and cleaned
            self$generate_clean_sample(1000, T, 'va')
            # The validation sample data is read
            data <- self$read_file(
                paste0(self$mdir,"/validate-clean.txt"), F)
            # A sample of 20 sentences is taken
            data <- data[1:20]
            # The information message
            msg <- paste0(
                "Calculating Perplexity for ", length(data), " sentences")
            # The information message is shown
            self$display_msg(msg, 1)
            # The list of perplexities
            pl <- c();
            # The loop counter
            c <- 1
            # The Perplexity of each sentence in the test data is calculated
            for (line in data) {
                # The line is split on space
                words <- str_split(line, " ")[[1]]
                # The perplexity for the line is calculated
                p <- self$calc_perplexity(words)
                # The information message
                msg <- paste0(
                    "Perplexity of the sentence '",
                    line, "' is: ", p)
                # The information message is shown
                self$display_msg(msg, 2)
                # The list of perplexities is updated
                pl <- c(pl, p);
                # If the counter is divisible by 10
                if (c %% 10 == 0) {
                    # The information message
                    msg <- paste0(c, " lines have been processed")
                    # The information message is shown
                    self$display_msg(msg, 1)
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
        #' Accuracy. First a validation data set containing 1000 lines is
        #' generated. It is then cleaned. 20 random sentences are taken. For
        #' each sentence, the model is used to predict the next word. The
        #' accuracy stats are returned. A prediction is considered to be correct
        #' if one of the predicted words matched the actual word.
        #' @return The number of correct and incorrect predictions.
        extrinsic_evaluation = function() {
            # The transition probabilities data is read
            private$read_tp_data(self$model)
            # Random sample is taken and cleaned
            self$generate_clean_sample(1000, T, 'va')
            # The validation sample data is read
            data <- self$read_file(
                paste0(self$mdir,"/validate-clean.txt"), F)
            # A Random sample of 100 sentences is taken
            data <- data[1:100]
            # The information message
            msg <- paste0(
                "Predicting the next word for ", length(data), " sentences")
            # The information message is shown
            self$display_msg(msg, 1)
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
                if (self$tg_opts[["stem_words"]]) {
                    # The previous words are stemmed
                    pw <- wordStem(pw)
                }
                # The next word is predicted
                res <- self$predict_word(pw, F)
                # If the predicted word matches the actual word
                if (w %in% res["words"]) {
                    stats[["valid"]] <- stats[["valid"]] + 1;
                    # The information message
                    self$display_msg(
                        paste0("The word: ", w, " was predicted"), 3)
                }
                # If the predicted word does not match
                else {
                    stats[["invalid"]] <- stats[["invalid"]] + 1;
                    # The information message
                    self$display_msg(
                        paste0("The word: ", w, " could not be predicted"), 3)
                }
                # The counter is increased by 1
                c <- c + 1
                # If the counter is divisible by 10
                if (c %% 10 == 0) {
                    # The information message
                    msg <- paste0(c, " sentences have been processed")
                    # The information message is shown
                    self$display_msg(msg, 1)
                }
            }

            # The precentage of valid
            stats[["valid_perc"]] <-
                (stats[["valid"]]/(stats[["valid"]] + stats[["invalid"]]))*100
            # The precentage of invalid
            stats[["invalid_perc"]] <- 100-stats[["valid_perc"]]

            return(stats)
        },

        #' @description
        #' Predicts the new word given a list of 1, 2 or 3 previous
        #' words. It checks the given n words in the transition probabilities
        #' data. If there is a match, the top 3 next words with highest
        #' probabilities are returned. If there is no match, then the last n-1
        #' previous words are checked. This process is continued until the last
        #' word is checked. If there is no match, then empty result is returned.
        #' @param words A character vector of previous words or a single vector
        #'   containing the previous word text.
        #' @param count The number of results to return.
        #' @param dc A DataCleaner object. If it is given, then the given words
        #'   are cleaned. If the stem_words option was set in the TokenGenerator
        #'   object configuration for the current model, then the words are
        #'   converted to their stems.
        #' @return The top 3 predicted words along with their probabilities.
        predict_word = function(words, count = 3, dc = NULL) {
            # The transition probabilities data is read
            private$read_tp_data(self$model)
            # The words are assigned to temp variable
            w <- words
            # If the DataCleaner obj was specified
            if (!is.null(dc)) {
                # If the words is a set of vectors
                if (length(w) > 1) {
                    # The words are converted to a single line of text
                    w <- paste0(w, collapse = " ")
                }
                # The words are cleaned
                w <- dc$clean_lines(w)
                # If the words should be stemmed
                if (self$tg_opts[["stem_words"]]) {
                    # The previous words are stemmed
                    w <- wordStem(w)
                }
            }

            # If the words are in the form of a line
            if (length(w) == 1) {
                # The words are split on space
                w <- strsplit(w, " ")[[1]]
            }

            # The length of previous words
            pwl <- length(w)
            # The loop counter
            c <- 1
            # Indicates if the word was found
            found <- FALSE
            # The result
            result <- list("found" = F, "words" = "", "probs" = "")
            # If the previous words length is 0
            if (pwl == 0)
                return(result)
            # The last 3 words are extracted.
            # If the previous word length is more than 3
            if (pwl > 3) {
                pw <- w[(pwl-2):pwl]
            }
            else {
                pw <- w
            }
            # The length of previous words
            pwl <- length(pw)
            # Each ngram in the previous word list is checked starting from
            # largest ngram
            for (i in pwl:1) {
                # The previous words to check
                tpw <- pw[c:pwl]
                # The key to use for the transition probabilities data
                k <- paste(tpw, collapse = "_")
                # The key is converted to a numeric hash
                h <- digest2int(k)
                # The transition probabilities data is checked
                res <- self$tp[self$tp$pre == h, ]
                # If the prefix was found
                if (nrow(res) > 0) {
                    # The word was found
                    found <- TRUE
                    # The result is sorted by probability
                    sres <- res[order(res$prob, decreasing = T),]
                    # The number of rows in the result set
                    rcount <- nrow(sres)
                    # If the number of results is more than the required number
                    # of results
                    if (rcount > count) {
                        # The result count is set to the required number of
                        # results
                        rc <- count
                    }
                    else {
                        # The result count is set to the number of results
                        rc <- rcount
                    }
                    # The required word probabilities
                    probs <- sres$prob[1:rc]
                    # The next words indexes
                    ind <- sres$nw[1:rc]
                    # The required words
                    nw <- as.character(self$wl$pre[ind])

                    # The result is updated
                    result[["words"]] <- nw
                    result[["probs"]] <- probs
                    result[["found"]] <- T
                    # The information message
                    msg <- paste0("The ngram key: ", k, " was found")
                    # Information message is shown
                    self$display_msg(msg, 3)
                    # The loop ends
                    break;
                }
                else {
                    # The information message
                    msg <- paste0("The ngram key: ", k, " was not found")
                    # Information message is shown
                    self$display_msg(msg, 3)
                    # The result is updated
                    result[["found"]] <- F
                }
                # The information message
                msg <- paste0("Backing off to ", (i) ,"-gram")
                # Information message is shown
                self$display_msg(msg, 3);
                # The counter is increased by 1
                c <- c + 1;
            }

            return(result);
        },

        #' @description
        #' Calculates the probability of the given word given the
        #' previous model-1 words, where model is the maximum ngram number. It
        #' looks up the probability of a word given n previous words. The
        #' previous n words are converted to numeric hash using digest2int
        #' function. The hash is looked up in a data frame of transition
        #' probabilities. The word is converted to a number by checking its
        #' position in a list of unique words. If the hash and the word position
        #' were found, then the probability of the previous word and hash is
        #' returned. If it was not found, then the hash of the n-1 previous
        #' words is taken and the processed is repeated. If the data was not
        #' found in the data frame, then the word probability is returned. This
        #' is known as backoff. If the word probability could not be found then
        #' the default probability is returned. The default probability is
        #' calculated as 1/(N+V), Where N = number of words in corpus and V is
        #' the number of dictionary words.
        #' @param word The word whoose probability is to be calculated.
        #' @param pw The previous words.
        #' @return The probability of the word given the previous words.
        get_word_prob = function(word, pw) {
            # If the default probability is not set, then an error is raised
            if (is.null(self$dp))
                stop("The default probability is not set !")
            # The length of previous words
            pwl <- length(pw)
            # The probability of the word given the previous words. It is
            # initialized to the default probability, which should be 1/(N+V)
            prob <- self$dp
            # The loop counter
            c <- 1
            # Indicates if the word was found
            found <- FALSE
            # The next word id
            nw <- match(word, self$wl$pre)
            # If the next word was not found
            if (is.na(nw)) {
                # The information message
                msg <- paste0(
                    "The next word: ", word, " was not found")
                # Information message is shown
                self$display_msg(msg, 3)
            }
            # If the previous word count is more than 0
            else if (pwl > 0) {
                # The previous words are checked
                for (i in pwl:1) {
                    # The previous words to check
                    tpw <- pw[c:pwl]
                    # The key to use for the transition matrix
                    k <- paste(tpw, collapse = "_")
                    # The key is converted to a numeric hash
                    h <- digest2int(k)
                    # The transition probabilities data is checked
                    res <- self$tp[self$tp$pre == h & self$tp$nw == nw, ]
                    # If the prefix was found
                    if (nrow(res) > 0) {
                        # The word was found
                        found <- TRUE
                        # The probability is set
                        prob <- as.numeric(res$prob)
                        # The information message
                        msg <- paste0("The ngram key: ",
                                      k, " and the next word: ",
                                      word, " were found")
                        # Information message is shown
                        self$display_msg(msg, 3)
                        # The loop ends
                        break
                    }
                    else {
                        # The information message
                        msg <- paste0("The ngram key: ",
                                      k, " and the next word: ",
                                      word, " were not found")
                        # Information message is shown
                        self$display_msg(msg, 3)
                    }
                    # The information message
                    msg <- paste0("Backing off to ", (i) ,"-gram")
                    # Information message is shown
                    self$display_msg(msg, 3)
                    # The counter is increased by 1
                    c <- c + 1
                }
            }
            # If the word was not found then the probability of the word is
            # checked in the n1-gram
            if (!found) {
                # If the word was not found
                if (sum(self$wl$pre == word) == 0) {
                    # Information message is shown
                    self$display_msg("Using default probability", 3)
                }
                else {
                    # The word probability
                    prob <- as.numeric(self$wl[self$wl$pre == word, "prob"])
                }
            }

            return(prob)
        },

        #' @description
        #' The Perplexity for the given sentence is calculated. For
        #' each word, the probability of the word given the previous words is
        #' calculated. The probabilities are multiplied and then inverted. The
        #' nth root of the result is the perplexity, where n is the number of
        #' words in the sentence. If the stem_words tokenization option was
        #' specified, then the previous words are converted to their stem.
        #' @param words The list of words.
        #' @return The perplexity of the given list of words.
        calc_perplexity = function(words) {
            # The number of words in the sentence
            wl <- length(words)
            # The product of the word probabilities
            prob_prod <- 1
            # For each word, the probability of the word is calculated
            for (i in 1:wl) {
                # The word
                word <- words[i]
                # The list of previous words
                pw <- NULL
                # If i is more than 1
                if (i > 1) {
                    # The start index
                    start <- 1
                    # If i > self$model
                    if (i > self$model) start <- i-(self$model-1)
                    # The list of previous words
                    pw <- words[start:(i-1)]
                    # If the words should be stemmed
                    if (self$tg_opts[["stem_words"]]) {
                        # The previous words are stemmed
                        pw <- wordStem(pw)
                    }
                }
                # The word probability
                prob <- self$get_word_prob(word, pw)
                # The probability product is updated
                prob_prod <- prob_prod * prob
            }
            # The nth root of the inverse of the probability product is taken
            p <- round((1/prob_prod)^(1/wl), 0)

            return(p)
        },

        #' @description
        #' Generates a sample file of given size from the main
        #' train.txt file file name. The file is cleaned and saved.
        #' @param ss The number of lines or proportion of lines to sample.
        #' @param ic If the sample file should be cleaned.
        #' @param t The type of sample. It can be:
        #'   'tr' -> training
        #'   'te' -> testing
        #'   'va' -> validation
        generate_clean_sample = function(ss, ic, t) {
            # If the type is 'tr'
            if (t == 'tr') sfn <- 'train'
            # If the type is 'te'
            else if (t == 'te') sfn <- 'test'
            # If the type is 'va'
            else if (t == 'va') sfn <- 'validate'
            # The sample file name
            fn <- paste0(self$ddir, "/", sfn, ".txt")
            # The sample file name
            sf <- paste0(self$mdir, "/", sfn, ".txt")
            # The clean sample file name
            csf <- paste0(self$mdir, "/", sfn, "-clean.txt")
            # If the cleaned sample file already exists
            if (file.exists(csf)) {
                # The information message
                msg <- paste0("The cleaned sample file: ", csf,
                              " already exists")
                # Information message is shown
                self$display_msg(msg, 2)
            }
            else {
                # If the sample file does not exist
                if (!file.exists(sf)) {
                    # The information message
                    msg <- paste0("Generating sample file from the file: ", fn)
                    # Information message is shown
                    self$display_msg(msg, 2)
                    # The input file is read
                    data <- self$read_file(fn, F)
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
                    # The sample file data is saved
                    self$write_file(data, sf, F)
                }
                # If the sample file should be cleaned
                if (ic) {
                    # The options for cleaning the data
                    opts <- self$dc_opts
                    # The line count is set to 5000
                    opts[["line_count"]] <- 5000
                    # The output file name
                    opts[["output_file"]] <- csf
                    # The data cleaner object is created
                    dc <- DataCleaner$new(sf, opts, verbose = self$verbose)
                    # The sample file is cleaned
                    dc$clean_file()
                }
            }
        }
    ),

    private = list(

        # @description It creates a single plot based on ggplot2. Depending on
        # the opt parameter, the plot may contain groups or it may be a simple
        # plot.
        # @param data A data frame containing the data to be plotted. It should
        #   have 2 variables, x and y.
        # @param opts The options for plotting the data. It contains:
        #   'x_lab' -> The x-axis label.
        #   'y_lab' -> The y-axis label.
        #   'type' -> The type of plot. It can be 'basic' or 'grouped'.
        #   'group' -> The field to group by.
        # @return A ggplot object representing the plot.
        plot_graph = function(data, opts) {
            # y-max
            y_max <- max(as.numeric(as.character(data$y)))
            # If the type is basic
            if (opts[['type']] == 'basic') {
                # The graph is plotted
                p <- ggplot(data, aes(x, y)) + geom_point() +
                    geom_smooth(method='lm', formula= y~x) +
                    labs(x = opts[["x_lab"]], y = opts[["y_lab"]]) +
                    xlim(min(data$x), max(data$x)) +
                    coord_cartesian(ylim = c(0,y_max))
            }
            # If the type is grouped
            else if (opts[['type']] == 'grouped') {
                # The data is duplicated to prevent the warnings
                data <- rbind(data, data)
                # The group field data
                g <- opts$group
                # The graph is plotted
                p <- ggplot(
                    data,
                    aes_string("x", "y", group = g, col = g)) +
                    geom_point() +
                    geom_smooth(method = "loess", formula = y ~ x, span = 1.4, se = FALSE) +
                    labs(x = opts[["x_lab"]], y = opts[["y_lab"]]) +
                    xlim(min(data$x), max(data$x)) +
                    coord_cartesian(ylim = c(0,y_max))
            }
            return(p)
        },

        # @description
        # It plots the given model performance stats. The stats
        # are plotted on one page. 5 stats are plotted. The first 4 plots are
        # of memory, time taken, mean Perplexity and accuracy. The fifth plot
        # is of Perplexity vs accuracy.
        # @param data The data to plot.
        # @param opts The options for plotting the data. It contains:
        #   'type' -> The type of plot. It can be 'basic' or 'grouped'.
        #   'group' -> The field to group by.
        #   'title' -> The main plot title.
        #   'subtitle' -> The plot sub title.
        plot_stats = function(data, opts) {
            # The information message is shown
            self$display_msg("Plotting model performance stats...", 1)
            # If the type is basic
            if (opts[["type"]] == 'basic') {
                # The data frames
                df1 <- data.frame(x = data$n, y = data$m)
                df2 <- data.frame(x = data$n, y = data$t)
                df3 <- data.frame(x = data$n, y = data$p)
                df4 <- data.frame(x = data$n, y = data$a)
                df5 <- data.frame(x = data$a, y = data$p)
                # The options for plot 1
                popts <- list("x_lab" = "ngram",
                              "y_lab" = "memory",
                              "type" = opts[["type"]])
                # Plot 1
                p1 <- private$plot_graph(df1, popts)
                # The options for plot 2
                popts <- list("x_lab" = "ngram",
                              "y_lab" = "time",
                              "type" = opts[["type"]])
                # Plot 2
                p2 <- private$plot_graph(df2, popts)
                # The options for plot 3
                popts <- list("x_lab" = "ngram",
                              "y_lab" = "perplexity",
                              "type" = opts[["type"]])
                # Plot 3
                p3 <- private$plot_graph(df3, popts)
                # The options for plot 4
                popts <- list("x_lab" = "ngram",
                              "y_lab" = "accuracy",
                              "type" = opts[["type"]])
                # Plot 4
                p4 <- private$plot_graph(df4, popts)
                # The options for plot 5
                popts <- list("x_lab" = "accuracy",
                              "y_lab" = "perplexity",
                              "type" = "basic")
                # Plot 5
                p5 <- private$plot_graph(df5, popts)
                # The plots are displayed on a single page
                patchwork <- p1 + p2 + p3 + p4 + p5
                # Main title is added
                print(patchwork + plot_annotation(
                    "title" = opts[["title"]],
                    "subtitle" = opts[["subtitle"]]))
            }
            # If the type is grouped
            else if (opts[["type"]] == 'grouped') {
                # The group name
                g <- opts[["group"]]
                # The data frames
                df1 <- data.frame(data$n, data$m, data[[g]])
                df2 <- data.frame(data$n, data$t, data[[g]])
                df3 <- data.frame(data$n, data$p, data[[g]])
                df4 <- data.frame(data$n, data$a, data[[g]])
                # The column names
                n <- c("x", "y", g)
                # The column names are set
                names(df1) <- n
                names(df2) <- n
                names(df3) <- n
                names(df4) <- n
                # The options for plot 1
                popts <- list("x_lab" = "ngram",
                              "y_lab" = "memory",
                              "group" = opts[["group"]],
                              "type" = opts[["type"]])
                # Plot 1
                p1 <- private$plot_graph(df1, popts)
                # The options for plot 2
                popts <- list("x_lab" = "ngram",
                              "y_lab" = "time",
                              "group" = opts[["group"]],
                              "type" = opts[["type"]])
                # Plot 2
                p2 <- private$plot_graph(df2, popts)
                # The options for plot 3
                popts <- list("x_lab" = "ngram",
                              "y_lab" = "perplexity",
                              "group" = opts[["group"]],
                              "type" = opts[["type"]])
                # Plot 3
                p3 <- private$plot_graph(df3, popts)
                # The options for plot 4
                popts <- list("x_lab" = "ngram",
                              "y_lab" = "accuracy",
                              "group" = opts[["group"]],
                              "type" = opts[["type"]])
                # Plot 4
                p4 <- private$plot_graph(df4, popts)
                # The plots are displayed on a single page
                patchwork <- p1 + p2 + p3 + p4
                # Main title is added
                # Main title is added
                print(patchwork + plot_annotation(
                    "title" = opts[["title"]],
                    "subtitle" = opts[["subtitle"]]))
            }
        },

        # @description
        # Reads the model file and sets the current objects attributes.
        # @param model The model number. It is the maximum ngrams supported by
        #   the model.
        read_tp_data = function(model) {
            # If the model has already been loaded then function returns
            if (!is.null(self$tp)) return()
            # The information message
            msg <- paste0("Loading model ", model, "...")
            # The information message is shown
            self$display_msg(msg, 1)
            # The model config file name
            fn <- paste0(self$mdir, "/config.RDS")
            # The config file is loaded
            self$load_config(fn)
            # The model file name
            fn <- paste0(self$mdir, "/model-", model, ".RDS")
            # The model is read to a data frame
            self$tp <- self$read_obj(fn)
            # The words file name
            fn <- paste0(self$mdir, "/words.RDS")
            # The list of words is read
            self$wl <- self$read_obj(fn)
            # The dictionary file name
            fn <- self$dc_opts[["dict_file"]]
            # The file contents
            dict <- self$read_file(fn, F)
            # The information message is shown
            self$display_msg("Calculating default probability...", 1)
            # The number of words in the dictionary file. It is used to
            # calculate Perplexity.
            vc <- length(dict)
            # The cleaned training file is read
            fn <- paste0(self$mdir, "/train-clean.txt")
            # The model is read to a data frame
            data <- self$read_file(fn, F)
            # The words are split on " "
            w <- strsplit(data, " ")
            # The words are converted to atomic list
            w <- unlist(w)
            # The number of words
            n <- length(w)
            # The default probability is set
            self$dp <- 1/(n + vc)
        },

        # @description
        # It returns the average value for the required stats, for
        # the given performance stats.
        # @param pstats The performance stats.
        # @param type The type of stats. It can be:
        #   'memory' -> The mean memory used.
        #   'time_taken' -> The mean time taken by the intrinsic and
        #     extrinsic evaluation tests.
        #   'perplexity' -> The mean Perplexity score.
        #   'accuracy' -> The mean accuracy score.
        # @param nmax -> The maximum ngram size.
        process_stats = function(pstats, type, nmax) {
            # The stats
            s <- NULL
            # The average memory for each ngram model
            for (n in 1:(nmax-1)) {
                # If the stats is memory
                if (type == "memory") {
                    # The stats value for the current ngram number
                    v <- pstats[[n]][["memory"]]
                    # The stats are formated
                    v <- self$format_size(v)
                }
                # If the stats is perplexity
                else if (type == "perplexity") {
                    # The stats value for the current ngram number
                    v <- pstats[[n]][["intrinsic"]][["mean"]]
                }
                # If the stats is accuracy
                else if (type == "accuracy") {
                    # The stats value for the current ngram number
                    v <- pstats[[n]][["extrinsic"]][["valid_perc"]]
                }
                # If the stats is time
                else if (type == "time") {
                    # The stats value for the current ngram number
                    v <- pstats[[n]][["time"]]
                }
                # The stats are updated
                s <- c(s, v)
            }

            return(s)
        }
    )
)
