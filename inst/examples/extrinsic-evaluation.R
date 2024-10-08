# Start of environment setup code
# The level of detail in the information messages
ve <- 2
# The name of the folder that will contain all the files. It will be created in
# the current directory. NULL implies tempdir will be used
fn <- NULL
# The required files. They are default files that are part of the package
rf <- c("def-model.RDS", "validate-clean.txt")
# An object of class EnvManager is created
em <- EnvManager$new(ve = ve, rp = "./")
# The required files are downloaded
ed <- em$setup_env(rf, fn)
# End of environment setup code

# The model file name
mfn <- paste0(ed, "/def-model.RDS")
# The validation file name
vfn <- paste0(ed, "/validate-clean.txt")

# ModelEvaluator class object is created
me <- ModelEvaluator$new(mf = mfn, ve = ve)
# The intrinsic evaluation is performed
stats <- me$extrinsic_evaluation(lc = 100, fn = vfn)
# The evaluation stats are printed
print(stats)

# The test environment is removed. Comment the below line, so the files
# generated by the function can be viewed
em$td_env()
