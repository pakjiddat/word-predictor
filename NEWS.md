# wordpredictor 0.0.3

## Bug fixes

  * Disabled caching in R Markdown files, because it was causing problems with CRAN checks.
  
# wordpredictor 0.0.2

## Bug fixes

  * Fixed small bugs that were causing problems with GitHub actions and CRAN checks.
  * Removed custom `.Rprofile` file as it was causing problems with GitHub actions.
  * Updated sample code in `features.Rmd` vignette so it does not cause issues with R CMD Check on MacOs.
  * Removed `inst/extdata folder` from `.gitignore` since it was causing problems with check-standard workflow on GitHub.
  * Removed non-standard characters from example in data-cleaner.R file as they were causing problems with CRAN check on "Debian Linux, R-devel, clang".
  * Issues related to the bug fixes: [#318](https://github.com/r-lib/actions/issues/318), [#319](https://github.com/r-lib/actions/issues/319), [#320](https://github.com/r-lib/actions/issues/320)
 
# wordpredictor 0.0.1

  * Initial Release.
