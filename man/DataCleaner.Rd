% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-cleaner.R
\name{DataCleaner}
\alias{DataCleaner}
\title{Provides data cleaning functionality}
\description{
It provides a memory efficient method for removing unneeded
characters from text files. It is suitable for cleaning large text files.
}
\details{
It provides a method for cleaning text files. It allows removing bad words,
stop words, non dictionary words, extra space, punctuation and non-alphabet
characters. It also allows conversion to lower case. It supports large text
files.
}
\examples{

## ------------------------------------------------
## Method `DataCleaner$clean_file`
## ------------------------------------------------

# Start of environment setup code
# The level of detail in the information messages
ve <- 0
# The name of the folder that will contain all the files. It will be
# created in the current directory. NULL implies tempdir will be used
fn <- NULL
# The required files. They are default files that are part of the
# package
rf <- c("test.txt")
# An object of class EnvManager is created
em <- EnvManager$new(ve = ve, rp = "./")
# The required files are downloaded
ed <- em$setup_env(rf, fn)
# End of environment setup code

# The cleaned test file name
cfn <- paste0(ed, "/test-clean.txt")
# The test file name
fn <- paste0(ed, "/test.txt")
# The data cleaning options
dc_opts <- list("output_file" = cfn)
# The data cleaner object is created
dc <- DataCleaner$new(fn, dc_opts, ve = ve)
# The sample file is cleaned
dc$clean_file()

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()

## ------------------------------------------------
## Method `DataCleaner$clean_lines`
## ------------------------------------------------

# The level of detail in the information messages
ve <- 0
# Test data is read
l <- c(
    "If you think I'm wrong, send me a link to where it's happened",
    "We're about 90percent done with this room",
    "This isn't how I wanted it between us.",
    "Almost any cute breed can become ornamental",
    "Once upon a time there was a kingdom with a castle",
    "That's not a thing any of us are granted'",
    "Why are you being so difficult? she asks."
)
# The expected results
res <- c(
    "if you think wrong send me a link to where its happened",
    "were about percent done with this room",
    "this how i wanted it between us",
    "almost any cute breed can become ornamental",
    "once upon a time there was a kingdom with a castle",
    "thats not a thing any of us are granted",
    "why are you being so difficult she asks"
)
# The DataCleaner object is created
dc <- DataCleaner$new(ve = ve)
# The line is cleaned
cl <- dc$clean_lines(l)
# The cleaned lines are printed
print(cl)
}
\section{Super class}{
\code{\link[wordpredictor:Base]{wordpredictor::Base}} -> \code{DataCleaner}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DataCleaner-new}{\code{DataCleaner$new()}}
\item \href{#method-DataCleaner-clean_file}{\code{DataCleaner$clean_file()}}
\item \href{#method-DataCleaner-clean_lines}{\code{DataCleaner$clean_lines()}}
\item \href{#method-DataCleaner-clone}{\code{DataCleaner$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataCleaner-new"></a>}}
\if{latex}{\out{\hypertarget{method-DataCleaner-new}{}}}
\subsection{Method \code{new()}}{
It initializes the current object. It is used to set the file name
and verbose options.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataCleaner$new(fn = NULL, opts = list(), ve = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{fn}}{The path to the file to clean.}

\item{\code{opts}}{The options for data cleaning.
\itemize{
\item \strong{min_words}. The minimum number of words per sentence.
\item \strong{line_count}. The number of lines to read and clean at a time.
\item \strong{save_data}. If the combined processed lines should be saved.
\item \strong{output_file}. Name of the output file used to store the data.
\item \strong{sw_file}. The stop words file path.
\item \strong{dict_file}. The dictionary file path.
\item \strong{bad_file}. The bad words file path.
\item \strong{to_lower}. If the words should be converted to lower case.
\item \strong{remove_stop}. If stop words should be removed.
\item \strong{remove_punct}. If punctuation symbols should be removed.
\item \strong{remove_non_dict}. If non dictionary words should be removed.
\item \strong{remove_non_alpha}. -> If non alphabet symbols should be removed.
\item \strong{remove_extra_space}. -> If leading, trailing and double spaces
should be removed.
\item \strong{remove_bad}. If bad words should be removed
}}

\item{\code{ve}}{The level of detail in the information messages.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataCleaner-clean_file"></a>}}
\if{latex}{\out{\hypertarget{method-DataCleaner-clean_file}{}}}
\subsection{Method \code{clean_file()}}{
It provides an efficient method for cleaning text files.
It removes unneeded characters from the given text file with several
options.

It allows removing punctuation, bad words, stop words,
non-alphabetical symbols and non-dictionary words. It reads a certain
number of lines from the given text file. It removes unneeded
characters from the lines and then saves the lines to an output text
file.

File cleaning progress is displayed if the verbose option was
set in the class constructor. It is suitable for cleaning large text
files.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataCleaner$clean_file()}\if{html}{\out{</div>}}
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
rf <- c("test.txt")
# An object of class EnvManager is created
em <- EnvManager$new(ve = ve, rp = "./")
# The required files are downloaded
ed <- em$setup_env(rf, fn)
# End of environment setup code

# The cleaned test file name
cfn <- paste0(ed, "/test-clean.txt")
# The test file name
fn <- paste0(ed, "/test.txt")
# The data cleaning options
dc_opts <- list("output_file" = cfn)
# The data cleaner object is created
dc <- DataCleaner$new(fn, dc_opts, ve = ve)
# The sample file is cleaned
dc$clean_file()

# The test environment is removed. Comment the below line, so the
# files generated by the function can be viewed
em$td_env()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataCleaner-clean_lines"></a>}}
\if{latex}{\out{\hypertarget{method-DataCleaner-clean_lines}{}}}
\subsection{Method \code{clean_lines()}}{
It cleans the given lines of text using the options
passed to the current object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataCleaner$clean_lines(lines)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lines}}{The input sentences.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The cleaned lines of text.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# The level of detail in the information messages
ve <- 0
# Test data is read
l <- c(
    "If you think I'm wrong, send me a link to where it's happened",
    "We're about 90percent done with this room",
    "This isn't how I wanted it between us.",
    "Almost any cute breed can become ornamental",
    "Once upon a time there was a kingdom with a castle",
    "That's not a thing any of us are granted'",
    "Why are you being so difficult? she asks."
)
# The expected results
res <- c(
    "if you think wrong send me a link to where its happened",
    "were about percent done with this room",
    "this how i wanted it between us",
    "almost any cute breed can become ornamental",
    "once upon a time there was a kingdom with a castle",
    "thats not a thing any of us are granted",
    "why are you being so difficult she asks"
)
# The DataCleaner object is created
dc <- DataCleaner$new(ve = ve)
# The line is cleaned
cl <- dc$clean_lines(l)
# The cleaned lines are printed
print(cl)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataCleaner-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DataCleaner-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataCleaner$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
