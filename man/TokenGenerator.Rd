% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/token-generator.R
\name{TokenGenerator}
\alias{TokenGenerator}
\title{Generates n-grams from text files}
\description{
It generates n-gram tokens along with their frequencies. The data
may be saved to a file in plain text format or as a R object.
}
\examples{

## ------------------------------------------------
## Method `TokenGenerator$generate_tokens`
## ------------------------------------------------

# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be used
fn <- NULL
# The required files. They are default files that are part of the
# package
rf <- c("test-clean.txt")
# An object of class EnvManager is created
em <- EnvManager$new(ve = ve, rp = "./")
# The required files are downloaded
ed <- em$setup_env(rf, fn)
# End of environment setup code

# The n-gram size
n <- 4
# The test file name
tfn <- paste0(ed, "/test-clean.txt")
# The n-gram number is set
tg_opts <- list("n" = n, "save_ngrams" = TRUE, "dir" = ed)
# The TokenGenerator object is created
tg <- TokenGenerator$new(tfn, tg_opts, ve = ve)
# The n-gram tokens are generated
tg$generate_tokens()

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()
}
\section{Super class}{
\code{\link[wordpredictor:Base]{wordpredictor::Base}} -> \code{TokenGenerator}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TokenGenerator-new}{\code{TokenGenerator$new()}}
\item \href{#method-TokenGenerator-generate_tokens}{\code{TokenGenerator$generate_tokens()}}
\item \href{#method-TokenGenerator-clone}{\code{TokenGenerator$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TokenGenerator-new"></a>}}
\if{latex}{\out{\hypertarget{method-TokenGenerator-new}{}}}
\subsection{Method \code{new()}}{
It initializes the current obj. It is used to set the file name,
tokenization options and verbose option.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TokenGenerator$new(fn = NULL, opts = list(), ve = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{fn}}{The path to the input file.}

\item{\code{opts}}{The options for generating the n-gram tokens.
\itemize{
\item \strong{n}. The n-gram size.
\item \strong{save_ngrams}. If the n-gram data should be saved.
\item \strong{min_freq}. All n-grams with frequency less than min_freq are
ignored.
\item \strong{line_count}. The number of lines to process at a time.
\item \strong{stem_words}. If words should be transformed to their stems.
\item \strong{dir}. The dir where the output file should be saved.
\item \strong{format}. The format for the output. There are two options.
\itemize{
\item \strong{plain}. The data is stored in plain text.
\item \strong{obj}. The data is stored as a R obj.
}
}}

\item{\code{ve}}{The level of detail in the information messages.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TokenGenerator-generate_tokens"></a>}}
\if{latex}{\out{\hypertarget{method-TokenGenerator-generate_tokens}{}}}
\subsection{Method \code{generate_tokens()}}{
It generates n-gram tokens and their frequencies from the
given file name. The tokens may be saved to a text file as plain text
or a R object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TokenGenerator$generate_tokens()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The data frame containing n-gram tokens along with their
frequencies.
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
rf <- c("test-clean.txt")
# An object of class EnvManager is created
em <- EnvManager$new(ve = ve, rp = "./")
# The required files are downloaded
ed <- em$setup_env(rf, fn)
# End of environment setup code

# The n-gram size
n <- 4
# The test file name
tfn <- paste0(ed, "/test-clean.txt")
# The n-gram number is set
tg_opts <- list("n" = n, "save_ngrams" = TRUE, "dir" = ed)
# The TokenGenerator object is created
tg <- TokenGenerator$new(tfn, tg_opts, ve = ve)
# The n-gram tokens are generated
tg$generate_tokens()

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TokenGenerator-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TokenGenerator-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TokenGenerator$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
