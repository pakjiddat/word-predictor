# Start of environment setup code
# The level of detail in the information messages
ve <- 2
# The name of the folder that will contain all the files. It will be created in
# the current directory. NULL value implies tempdir will be used.
fn <- NULL
# The required files. They are default files that are part of the package
rf <- c("n2.RDS")
# An object of class EnvManager is created
em <- EnvManager$new(ve = ve, rp = "./")
# The required files are downloaded
ed <- em$setup_env(rf, fn)
# End of environment setup code

# The n-gram file name
nfn <- paste0(ed, "/n2.RDS")
# The DataAnalyzer object is created
da <- DataAnalyzer$new(nfn, ve = ve)
# The top features plot is checked
df <- da$plot_n_gram_stats(opts = list(
    "type" = "top_features",
    "n" = 10,
    "save_to" = NULL,
    "dir" = ed
))
# N-gram statistics are displayed
print(df)

# The test environment is removed. Comment the below line, so the files
# generated by the function can be viewed
em$td_env()
