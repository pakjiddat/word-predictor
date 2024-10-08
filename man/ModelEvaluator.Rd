% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-evaluator.R
\name{ModelEvaluator}
\alias{ModelEvaluator}
\title{Evaluates performance of n-gram models}
\description{
It provides methods for performing extrinsic and intrinsic
evaluation of a n-gram model. It also provides a method for comparing
performance of multiple n-gram models.

Intrinsic evaluation is based on calculation of Perplexity. Extrinsic
evaluation involves determining the percentage of correct next word
predictions.
}
\details{
Before performing the intrinsic and extrinsic model evaluation, a validation
file must be first generated. This can be done using the DataSampler class.

Each line in the validation file is evaluated. For intrinsic evaluation
Perplexity for the line is calculated. An overall summary of the Perplexity
calculations is returned. It includes the min, max and mean Perplexity.

For extrinsic evaluation, next word prediction is performed on each line. If
the actual next word is one of the three predicted next words, then the
prediction is considered to be accurate. The extrinsic evaluation returns the
percentage of correct and incorrect predictions.
}
\examples{

## ------------------------------------------------
## Method `ModelEvaluator$compare_performance`
## ------------------------------------------------

# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be
# used.
fn <- NULL
# The required files. They are default files that are part of the
# package
rf <- c("def-model.RDS")
# An object of class EnvManager is created
em <- EnvManager$new(ve = ve, rp = "./")
# The required files are downloaded
ed <- em$setup_env(rf, fn)
# End of environment setup code
# ModelEvaluator class object is created
me <- ModelEvaluator$new(ve = ve)
# The performance evaluation is performed
me$compare_performance(opts = list(
    "save_to" = NULL,
    "dir" = ed
))

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()

## ------------------------------------------------
## Method `ModelEvaluator$evaluate_performance`
## ------------------------------------------------

# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be used
fn <- NULL
# The required files. They are default files that are part of the
# package
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
# The performance evaluation is performed
stats <- me$evaluate_performance(lc = 20, fn = vfn)
# The evaluation stats are printed
print(stats)

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()

## ------------------------------------------------
## Method `ModelEvaluator$intrinsic_evaluation`
## ------------------------------------------------

# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be used
fn <- NULL
# The required files. They are default files that are part of the
# package
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
stats <- me$intrinsic_evaluation(lc = 20, fn = vfn)
# The evaluation stats are printed
print(stats)

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()

## ------------------------------------------------
## Method `ModelEvaluator$extrinsic_evaluation`
## ------------------------------------------------

# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be used
fn <- NULL
# The required files. They are default files that are part of the
# package
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

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()
}
\section{Super class}{
\code{\link[wordpredictor:Base]{wordpredictor::Base}} -> \code{ModelEvaluator}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ModelEvaluator-new}{\code{ModelEvaluator$new()}}
\item \href{#method-ModelEvaluator-compare_performance}{\code{ModelEvaluator$compare_performance()}}
\item \href{#method-ModelEvaluator-plot_stats}{\code{ModelEvaluator$plot_stats()}}
\item \href{#method-ModelEvaluator-evaluate_performance}{\code{ModelEvaluator$evaluate_performance()}}
\item \href{#method-ModelEvaluator-intrinsic_evaluation}{\code{ModelEvaluator$intrinsic_evaluation()}}
\item \href{#method-ModelEvaluator-extrinsic_evaluation}{\code{ModelEvaluator$extrinsic_evaluation()}}
\item \href{#method-ModelEvaluator-clone}{\code{ModelEvaluator$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelEvaluator-new"></a>}}
\if{latex}{\out{\hypertarget{method-ModelEvaluator-new}{}}}
\subsection{Method \code{new()}}{
It initializes the current object. It is used to set the
model file name and verbose options.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelEvaluator$new(mf = NULL, ve = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mf}}{The model file name.}

\item{\code{ve}}{The level of detail in the information messages.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelEvaluator-compare_performance"></a>}}
\if{latex}{\out{\hypertarget{method-ModelEvaluator-compare_performance}{}}}
\subsection{Method \code{compare_performance()}}{
It compares the performance of the models in the given folder.

The performance of the model is compared for the 4 metric which are
time taken, memory used, Perplexity and accuracy. The performance
comparison is displayed on plots.

4 plots are displayed. One for each performance metric. A fifth plot
shows the variation of Perplexity with accuracy. All 5 plots are
plotted on one page.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelEvaluator$compare_performance(opts)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{opts}}{The options for comparing model performance.
\itemize{
\item \strong{save_to}. The graphics device to save the plot to.
NULL implies plot is printed.
\item \strong{dir}. The directory containing the model file, plot and stats.
}}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be
# used.
fn <- NULL
# The required files. They are default files that are part of the
# package
rf <- c("def-model.RDS")
# An object of class EnvManager is created
em <- EnvManager$new(ve = ve, rp = "./")
# The required files are downloaded
ed <- em$setup_env(rf, fn)
# End of environment setup code
# ModelEvaluator class object is created
me <- ModelEvaluator$new(ve = ve)
# The performance evaluation is performed
me$compare_performance(opts = list(
    "save_to" = NULL,
    "dir" = ed
))

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelEvaluator-plot_stats"></a>}}
\if{latex}{\out{\hypertarget{method-ModelEvaluator-plot_stats}{}}}
\subsection{Method \code{plot_stats()}}{
It plots the given stats on 5 plots. The plots are displayed on a
single page.

The 4 performance metrics which are time taken, memory, Perplexity
and accuracy are plotted against the model name. Another plot
compares Perplexity with accuracy for each model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelEvaluator$plot_stats(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{The data to plot}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The ggplot object is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelEvaluator-evaluate_performance"></a>}}
\if{latex}{\out{\hypertarget{method-ModelEvaluator-evaluate_performance}{}}}
\subsection{Method \code{evaluate_performance()}}{
It performs intrinsic and extrinsic evaluation for the given model
and validation text file. The given number of lines in the validation
file are used in the evaluation

It performs two types of evaluations. One is intrinsic evaluation,
based on Perplexity, the other is extrinsic evaluation based on
accuracy.

It returns the results of evaluation. 4 evaluation metrics are
returned. Perplexity, accuracy, memory and time taken. Memory is the
size of the model object. Time taken is the time needed for
performing both evaluations.

The results of the model evaluation are saved within the model object
and also returned.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelEvaluator$evaluate_performance(lc, fn)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lc}}{The number of lines of text in the validation file to be
used for the evaluation.}

\item{\code{fn}}{The name of the validation file. If it does not exist, then
the default file validation-clean.txt is checked in the models
folder}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The performance stats are returned.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be used
fn <- NULL
# The required files. They are default files that are part of the
# package
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
# The performance evaluation is performed
stats <- me$evaluate_performance(lc = 20, fn = vfn)
# The evaluation stats are printed
print(stats)

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelEvaluator-intrinsic_evaluation"></a>}}
\if{latex}{\out{\hypertarget{method-ModelEvaluator-intrinsic_evaluation}{}}}
\subsection{Method \code{intrinsic_evaluation()}}{
Evaluates the model using intrinsic evaluation based on
Perplexity. The given number of sentences are taken from the
validation file. For each sentence, the Perplexity is calculated.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelEvaluator$intrinsic_evaluation(lc, fn)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lc}}{The number of lines of text in the validation file to be
used for the evaluation.}

\item{\code{fn}}{The name of the validation file. If it does not exist, then
the default file validation-clean.txt is checked in the models
folder}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The min, max and mean Perplexity score.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be used
fn <- NULL
# The required files. They are default files that are part of the
# package
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
stats <- me$intrinsic_evaluation(lc = 20, fn = vfn)
# The evaluation stats are printed
print(stats)

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelEvaluator-extrinsic_evaluation"></a>}}
\if{latex}{\out{\hypertarget{method-ModelEvaluator-extrinsic_evaluation}{}}}
\subsection{Method \code{extrinsic_evaluation()}}{
Evaluates the model using extrinsic evaluation based on
Accuracy. The given number of sentences are taken from the validation
file.

For each sentence, the model is used to predict the next word.
The accuracy stats are returned. A prediction is considered to be
correct if one of the predicted words matches the actual word.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelEvaluator$extrinsic_evaluation(lc, fn)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lc}}{The number of lines of text in the validation file to be
used for the evaluation.}

\item{\code{fn}}{The name of the validation file.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The number of correct and incorrect predictions.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be used
fn <- NULL
# The required files. They are default files that are part of the
# package
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

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelEvaluator-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ModelEvaluator-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelEvaluator$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
