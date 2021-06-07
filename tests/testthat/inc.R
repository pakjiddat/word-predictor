# The wordpredictor options
wp <- getOption("wordpredictor")
# An object of class EnvManager is created
em <- EnvManager$new(ve = wp$ve)
# The required files are downloaded
ed <- em$setup_env(wp$rf)
