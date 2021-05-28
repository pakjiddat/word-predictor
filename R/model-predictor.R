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
#' evaluation based on accuracy. It provides a method for determining how much
#' memory and time is needed to generate a model for different input data sizes.
#' It provides a method for determining how much memory is needed by the final
#' model.
#' @importFrom digest digest2int
#' @importFrom SnowballC wordStem
ModelPredictor <- R6::R6Class(
    "ModelPredictor",
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
            if (!file.exists(mf)) {
                stop(paste0("Invalid model file: ", mf))
            } else {
                # The model object is read
                private$m <- private$read_obj(mf)
            }
        },

        #' @description
        #' Returns the Model class object.
        #' @return The Model class object is returned.
        get_model = function() {
            # The model object is returned
            return(private$m)
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
            # The model size
            n <- private$m$get_config("n")
            # The options for token generation
            tg_opts <- private$m$get_config("tg_opts")

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
                    if (i > n) start <- i - (n - 1)
                    # The list of previous words
                    pw <- words[start:(i - 1)]
                    # If the words should be stemmed
                    if (tg_opts[["stem_words"]]) {
                        # The previous words are stemmed
                        pw <- wordStem(pw)
                    }
                }
                # The word probability
                prob <- self$get_word_prob(word, pw)
                # The probability product is updated
                prob_prod <- prob_prod * prob
            }
            # The inverse of the number of words
            iwl <- 1 / wl
            # The nth root of the inverse of the probability product is taken
            p <- (1 / prob_prod)
            p <- p^iwl
            p <- round(p)

            return(p)
        },

        #' @description
        #' Predicts the new word given a list of 1, 2 or 3 previous words. It
        #' checks the given n words in the transition probabilities data. If
        #' there is a match, the top 3 next words with highest probabilities are
        #' returned. If there is no match, then the last n-1 previous words are
        #' checked. This process is continued until the last word is checked. If
        #' there is no match, then empty result is returned. The given words may
        #' optionally be stemmed.
        #' @param words A character vector of previous words or a single vector
        #'   containing the previous word text.
        #' @param count The number of results to return.
        #' @param dc A DataCleaner object. If it is given, then the given words
        #   are cleaned
        #' @return The top 3 predicted words along with their probabilities.
        predict_word = function(words, count = 3, dc = NULL) {
            # The tp data is fetched from the model object
            tp <- private$m$get_config("tp")
            # The loop counter
            c <- 1
            # The required results
            result <- list("found" = F, "words" = "", "probs" = "")
            # The previous words are fetched
            pw <- private$get_prev_words(words, dc)
            # If the previous words are NULL
            if (is.null(pw)) {
                return(result)
            }
            # The length of previous words
            pwl <- length(pw)
            # Each n-gram in the previous word list is checked starting from
            # largest n-gram
            for (i in pwl:1) {
                # The previous words to check
                tpw <- pw[c:pwl]
                # The key to use for the transition probabilities data
                k <- paste(tpw, collapse = "_")
                # The key is converted to a numeric hash
                h <- digest2int(k)
                # The transition probabilities data is checked
                res <- tp[tp$pre == h, ]
                # The results are checked
                result <- private$check_results(res, count, k)
                # If the data was found
                if (result[["found"]]) break
                # The information message
                msg <- paste0("Backing off to ", (i), "-gram")
                # Information message is shown
                private$display_msg(msg, 3)
                # The counter is increased by 1
                c <- c + 1
            }

            return(result)
        },

        #' @description
        #' Calculates the probability of the given word given the
        #' previous model-1 words, where model is the maximum n-gram number. It
        #' looks up the probability of a word given n previous words. The
        #' previous n words are converted to numeric hash using digest2int
        #' function. The hash is looked up in a data frame of transition
        #' probabilities. The word is converted to a number by checking its
        #' position in a list of unique words. If the hash and the word position
        #' were found, then the probability of the previous word and hash is
        #' returned. If it was not found, then the hash of the n-1 previous
        #' words is taken and the processed is repeated. If the data was not
        #' found in the data frame, then the word probability is returned. This
        #' is known as back-off. If the word probability could not be found then
        #' the default probability is returned. The default probability is
        #' calculated as 1/(N+V), Where N = number of words in corpus and V is
        #' the number of dictionary words.
        #' @param word The word whose probability is to be calculated.
        #' @param pw The previous words.
        #' @return The probability of the word given the previous words.
        get_word_prob = function(word, pw) {
            # The tp data is fetched from the model object
            tp <- private$m$get_config("tp")
            # The word list data is fetched from the model object
            wl <- private$m$get_config("wl")
            # The default probability is fetched from the model object
            dp <- private$m$get_config("dp")
            # If the default probability is not set, then an error is raised
            if (is.null(dp)) {
                stop("The default probability is not set in the model file !")
            }
            # The length of previous words
            pwl <- length(pw)
            # The probability of the word given the previous words. It is
            # initialized to the default probability, which should be 1/(N+V)
            prob <- dp
            # The loop counter
            c <- 1
            # Indicates if the word was found
            found <- FALSE
            # The next word id
            nw <- match(word, wl$pre)
            # If the next word was not found
            if (is.na(nw)) {
                # The information message
                msg <- paste0(
                    "The next word: ", word, " was not found"
                )
                # Information message is shown
                private$display_msg(msg, 3)
                # The default probability is returned
                return(prob)
            }
            # If the previous word count is 0
            if (pwl == 0) {
                return(prob)
            }

            # The previous words are checked
            for (i in pwl:1) {
                # The previous words to check
                tpw <- pw[c:pwl]
                # The key to use for the transition matrix
                k <- paste(tpw, collapse = "_")
                # The key is converted to a numeric hash
                h <- digest2int(k)
                # The transition probabilities data is checked
                res <- tp[tp$pre == h & tp$nw == nw, ]
                # If the prefix was found
                if (nrow(res) > 0) {
                    # The word was found
                    found <- TRUE
                    # The probability is set
                    prob <- as.numeric(res$prob)
                    # The information message
                    msg <- paste0(
                        "The n-gram key: ", k,
                        " and the next word: ", word, " were found"
                    )
                    # Information message is shown
                    private$display_msg(msg, 3)
                    # The loop ends
                    break
                }
                else {
                    # The information message
                    msg <- paste0(
                        "The n-gram key: ", k,
                        " and the next word: ", word, " were not found"
                    )
                    # Information message is shown
                    private$display_msg(msg, 3)
                }
                # The information message
                msg <- paste0("Backing off to ", (i), "-gram")
                # Information message is shown
                private$display_msg(msg, 3)
                # The counter is increased by 1
                c <- c + 1
            }

            # If the word was not found then the probability of the word is
            # checked in the n1-gram
            if (!found) {
                # If the word was not found
                if (sum(wl$pre == word) == 0) {
                    # Information message is shown
                    private$display_msg("Using default probability", 3)
                }
                else {
                    # The word probability
                    prob <- as.numeric(wl[wl$pre == word, "prob"])
                }
            }

            return(prob)
        }
    ),
    private = list(
        # @field m The model object.
        m = NULL,
        # @description
        # Fetches the list of previous words from the given list of words.
        # @param words A character vector of previous words or a single vector
        #   containing the previous word text.
        # @param dc A DataCleaner object. If it is given, then the given words
        #   are cleaned.
        # @return The list of previous words.
        get_prev_words = function(words, dc) {
            # The options for token generation
            tg_opts <- private$m$get_config("tg_opts")
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
            }

            # If the words should be stemmed
            if (tg_opts[["stem_words"]]) {
                # The previous words are stemmed
                w <- wordStem(w)
            }

            # If the words are in the form of a line
            if (length(w) == 1) {
                # The words are split on space
                w <- strsplit(w, " ")[[1]]
            }

            # The length of previous words
            pwl <- length(w)
            # If the previous words length is 0
            if (pwl == 0) {
                return(NULL)
            }
            # If the previous word length is more than 3
            if (pwl > 3) {
                # The last 3 words are extracted.
                pw <- w[(pwl - 2):pwl]
            }
            else {
                pw <- w
            }
        },
        # @description
        # Checks the result from the tp table
        # @param res The rows from the combined tp table.
        # @param count The number of results to return.
        # @param k The key string used to search the tp table.
        # @return The results of checking tp table.
        check_results = function(res, count, k) {
            # The word list data is fetched from the model object
            wl <- private$m$get_config("wl")
            # The word was found
            found <- FALSE
            # The required results
            result <- list("found" = F, "words" = "", "probs" = "")
            # If the prefix was found
            if (nrow(res) > 0) {
                # The word was found
                found <- TRUE
                # The result is sorted by probability
                sres <- res[order(res$prob, decreasing = T), ]
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
                nw <- as.character(wl$pre[ind])

                # The result is updated
                result[["words"]] <- nw
                result[["probs"]] <- probs
                result[["found"]] <- T
                # The information message
                msg <- paste0("The n-gram key: ", k, " was found")
                # Information message is shown
                private$display_msg(msg, 3)
            }
            else {
                # The information message
                msg <- paste0("The n-gram key: ", k, " was not found")
                # Information message is shown
                private$display_msg(msg, 3)
                # The result is updated
                result[["found"]] <- F
            }
            return(result)
        }
    )
)
