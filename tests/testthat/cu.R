# The wordpredictor options
wp <- getOption("wordpredictor")
# An object of class EnvManager is created
em <- EnvManager$new(ve = wp$ve)
# If the ea option is 't'
if (wp$ea == "t") {
    # The test environment is removed
    em$td_env(T)
} else if (wp$ea == "c") {
    # The test environment is copied
    em$cp_env()
}
