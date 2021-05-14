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
#' memory and time is needed to generate a model for different input data sizes.
#' It provides a method for determining how much memory is needed by the final
#' model.
#' @importFrom ggplot2 ggplot aes aes_string geom_point geom_smooth labs xlim
#'   coord_cartesian
#' @importFrom digest digest2int
#' @importFrom SnowballC wordStem
#' @importFrom patchwork plot_annotation
#' @importFrom pryr mem_change object_size
ModelEvaluator <- R6::R6Class(
    "ModelEvaluator",
    inherit = TextFileProcessor,
    public = list(
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
        #' @export
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
            private$model <- model
            # The sample size is set
            private$ssize <- ssize
            # The data directory name is set
            private$ddir <- ddir
            # The model directory name is set
            private$mdir <- mdir
            # If the dc_opts are given
            if (length(dc_opts) > 0) {
                # The custom dc_opts are merged with the default dc_opts
                private$dc_opts = modifyList(private$dc_opts, dc_opts)
            }
            # If the tg_opts are given
            if (length(tg_opts) > 0) {
                # The custom tg_opts are merged with the default tg_opts
                private$tg_opts = modifyList(private$tg_opts, tg_opts)
            }
            # The transition probabilities data is initialized
            private$tp <- NULL
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
            private$read_tp_data(private$model)
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
                if (private$tg_opts[["stem_words"]]) {
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
                res <- private$tp[private$tp$pre == h, ]
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
                    nw <- as.character(private$wl$pre[ind])

                    # The result is updated
                    result[["words"]] <- nw
                    result[["probs"]] <- probs
                    result[["found"]] <- T
                    # The information message
                    msg <- paste0("The ngram key: ", k, " was found")
                    # Information message is shown
                    private$display_msg(msg, 3)
                    # The loop ends
                    break;
                }
                else {
                    # The information message
                    msg <- paste0("The ngram key: ", k, " was not found")
                    # Information message is shown
                    private$display_msg(msg, 3)
                    # The result is updated
                    result[["found"]] <- F
                }
                # The information message
                msg <- paste0("Backing off to ", (i) ,"-gram")
                # Information message is shown
                private$display_msg(msg, 3);
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
            if (is.null(private$dp))
                stop("The default probability is not set !")
            # The length of previous words
            pwl <- length(pw)
            # The probability of the word given the previous words. It is
            # initialized to the default probability, which should be 1/(N+V)
            prob <- private$dp
            # The loop counter
            c <- 1
            # Indicates if the word was found
            found <- FALSE
            # The next word id
            nw <- match(word, private$wl$pre)
            # If the next word was not found
            if (is.na(nw)) {
                # The information message
                msg <- paste0(
                    "The next word: ", word, " was not found")
                # Information message is shown
                private$display_msg(msg, 3)
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
                    res <- private$tp[private$tp$pre == h & private$tp$nw == nw, ]
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
                        private$display_msg(msg, 3)
                        # The loop ends
                        break
                    }
                    else {
                        # The information message
                        msg <- paste0("The ngram key: ",
                                      k, " and the next word: ",
                                      word, " were not found")
                        # Information message is shown
                        private$display_msg(msg, 3)
                    }
                    # The information message
                    msg <- paste0("Backing off to ", (i) ,"-gram")
                    # Information message is shown
                    private$display_msg(msg, 3)
                    # The counter is increased by 1
                    c <- c + 1
                }
            }
            # If the word was not found then the probability of the word is
            # checked in the n1-gram
            if (!found) {
                # If the word was not found
                if (sum(private$wl$pre == word) == 0) {
                    # Information message is shown
                    private$display_msg("Using default probability", 3)
                }
                else {
                    # The word probability
                    prob <- as.numeric(private$wl[private$wl$pre == word, "prob"])
                }
            }

            return(prob)
        }
    )
)
