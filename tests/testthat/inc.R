# The wordpredictor options
wp <- getOption("wordpredictor")
# An object of class EnvManager is created
em <- EnvManager$new(ve = wp$ve)

tryCatch({
    # The required files are downloaded
    ed <- em$setup_env(wp$rf)
},
error = function(cond) {
  print(cond)
  traceback()
},
warning = function(cond) {
    print(cond)
    traceback()
})
