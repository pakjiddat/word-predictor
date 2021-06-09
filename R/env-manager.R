#' Allows managing the test environment
#'
#' @description
#' This class provides a method for creating directories in the tempdir folder
#' for testing purposes. It also provides a method for reading files from the
#' inst/extdata folder.
EnvManager <- R6::R6Class(
    "EnvManager",
    inherit = Base,
    public = list(
        #' @description
        #' It initializes the current object. It simply calls the base class
        #' constructor.
        #' @param rp The prefix for accessing the package root folder.
        #' @param ve The level of detail in the information messages.
        #' @export
        initialize = function(rp = "../../", ve = 0) {
            # The base class is initialized
            super$initialize(NULL, NULL, ve)
            # The root prefix is set
            private$rp <- rp
        },

        #' @description
        #' Checks if the given file exists. If it does not exist,
        #' then it tries to load the file from the inst/extdata data folder of
        #' the package. It throws an error if the file was not found. If the
        #' file exists, then the method simply returns the file name.
        #' @param fn The file name.
        #' @param dfn The name of the default file in the external data folder
        #'   of the package.
        #' @return The name of the file if it exists, or the full path to the
        #'   default file.
        get_data_fn = function(fn, dfn) {
            # The required file name
            rfn <- fn
            # If the file is not given
            if (is.null(fn)) {
                # The file path is set to the default file
                # included with the wordpredictor package
                rfn <- system.file("extdata", dfn, package = "wordpredictor")
                # If the file was not found
                if (!file.exists(rfn)) {
                    # An error message is shown
                    private$dm("The file: ",
                        rfn,
                        " does not exist !",
                        md = -1,
                        ty = "e"
                    )
                }
            }
            # If the file name is given but the file does not exist
            else if (!file.exists(fn)) {
                # An error message is shown
                private$dm("The file: ",
                    fn,
                    " does not exist !",
                    md = -1,
                    ty = "e"
                )
            }
            return(rfn)
        },

        #' @description
        #' Removes all files in the given directory.
        #' @param dn The directory name.
        remove_files = function(dn) {
            # The information message
            msg <- paste0("Removing all files in ", dn, "\n")
            # The information message is shown
            private$dm(msg, md = 1)
            # Each file in the directory is deleted
            for (fn in dir(dn, full.names = T)) {
                # The file is removed
                file.remove(fn)
            }
            # The information message is shown
            private$dm(" \u2714\n", md = 1)
        },

        #' @description
        #' Removes the ed folder created by the setup_env method. Also sets
        #' the R option, "ed" to NULL.
        #' @param rf If the environment folder should be removed.
        td_env = function(rf = F) {
            # The wordpredictor options
            wp <- getOption("wordpredictor")
            # The information message
            msg <- paste0("Removing the folder ", wp$ed)
            # The information message is shown
            private$dm(msg, md = 1)

            # The environment folder is removed
            unlink(wp$ed, recursive = T, force = T)
            # If the folder should not be removed
            if (!rf) {
                # The folder is created
                dir.create(wp$ed)
            }
            # The "ed" option is set to NULL
            wp$ed <- NULL
            # The wordpredictor options are updated
            options("wordpredictor" = wp)
            # The information message is shown
            private$dm(" \u2714\n", md = 1)
        },

        #' @description
        #' Copies the ed folder created by the setup_env method to
        #' inst/extdata.
        cp_env = function() {
            # The wordpredictor options
            wp <- getOption("wordpredictor")
            # The path to the folder
            fp <- paste0(private$rp, "inst/extdata/")
            # The information message
            msg <- paste0(
                "Copying the directory: ", wp$ed, " to the folder ", fp)
            # The information message is shown
            private$dm(msg, md = 1)
            # If the folder does not exist
            if (!dir.exists(fp)) {
                # The new folder path is created
                dir.create(fp)
            }
            # The tempdir is copied to the inst/extdata folder
            file.copy(wp$ed, fp, recursive = T)
            # The information message is shown
            private$dm(" \u2714\n", md = 1)
        },

        #' @description
        #' Copies the given files from test folder to the
        #' environment folder.
        #' @param fns The list of test files to copy
        #' @param cf A custom environment folder. It is a path relative to the
        #'   current directory. If not specified, then the tempdir function is
        #'   used to generate the environment folder.
        #' @return The list of folders that can be used during testing.
        setup_env = function(fns = c(), cf = NULL) {
            # The information message
            msg <- paste0("Setting up the test environment")
            # The information message is shown
            private$dh(msg, "-", md = 1)
            # The environment folder name
            ed <- NULL
            # If the cf is given and it does not exist
            if (!is.null(cf) && !dir.exists(cf)) {
                # The information message
                msg <- paste0("Creating custom environment folder: ", cf)
                # The information message is shown
                private$dm(msg, md = 1)

                # The custom environment folder is created
                dir.create(cf)
                # The information message is shown
                private$dm(" \u2714\n", md = 1)
                # The environment folder is set
                ed <- cf
            }
            else {
                # The tempdir location
                ed <- tempdir()
                # If the tempdir does not exist, then it is created
                if (!dir.exists(ed)) {
                    # The information message is shown
                    private$dm(
                        "The tempdir:",
                        ed, "does not exist. Creating the dir",
                        md = 1
                    )
                    # The tempdir is created
                    dir.create(ed)
                    # The information message is shown
                    private$dm(" \u2714\n", md = 1)
                }
            }

            # The wordpredictor options
            wp <- getOption("wordpredictor")
            # The ed option is updated
            wp$ed <- ed
            # The wordpredictor options are updated
            options("wordpredictor" = wp)

            # Each file is copied from inst/extdata to the given folder
            for (fn in fns) {
                # The source file path
                sfp <- system.file("extdata", fn, package = "wordpredictor")
                # If the source file path does not exist
                if (!file.exists(sfp)) {
                    stop(getwd())
                }
                # The information message is shown
                private$dm("Copying file:", fn, "to", ed, md = 1)
                # The source file is copied
                file.copy(sfp, ed)
                # The information message is shown
                private$dm(" \u2714\n", md = 1)
            }

            # The information message is shown
            private$dh("DONE", "=", md = 1)

            return(ed)
        }
    ),
    private = list(
        # @field rp The prefix for accessing the package root.
        rp = "../../"
    )
)
