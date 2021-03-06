% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brk_labels.R
\name{brk_labels}
\alias{brk_labels}
\title{Create pretty breaks labels}
\usage{
brk_labels(
  x,
  format = NULL,
  custom = FALSE,
  mult_by = 1,
  round_digits = NULL,
  sep = " to ",
  ...
)
}
\arguments{
\item{x}{A character vector.}

\item{format}{A function for formatting text, a string giving desired output format, or \code{NULL} (the default) for no formatting. Built-in shorthands are \code{"percent"}, \code{"dollar"}, and \code{"dollark"}, the last of which formats numbers like \verb{$12.3k}. Alternatively, provide a character argument to be used by \code{base::formatC} and set \code{custom = TRUE}.}

\item{custom}{Logical, whether to use custom formatting, triggering a call to \code{formatC} with the arguments supplied to \code{format} and \code{...}. Defaults \code{FALSE}.}

\item{mult_by}{A number by which to multiply values in breaks. Defaults to 1, i.e. just the numbers input. Note that multiplication is carried out \emph{before} rounding and formatting.}

\item{round_digits}{If not \code{NULL} (the default), the number of digits to round to. Note that this takes place after multiplying by \code{mult_by}.}

\item{sep}{A string by which to separate values in breaks.}

\item{...}{Any additional arguments to pass on to the function given in \code{format}, or to \code{base::formatC} if \code{custom} is \code{TRUE}.}
}
\value{
A character vector of formatted break labels.
}
\description{
This function maps over a vector of labels, such as those returned by \code{base::cut}. It works well in formatting \code{ggplot} scales, and can optionally pass parameters to an underlying call to \code{base::formatC}. Any \code{NA} values will be retained.
}
\examples{
percentage_brks <- c("[0.04,0.15]", "(0.15,0.25]", "(0.25,0.4]")
brk_labels(percentage_brks)
brk_labels(percentage_brks, format = "percent", mult_by = 100)

scientific_brks <- c("[-15500,0]", "(0,20000]", "(20000,25000]")
brk_labels(scientific_brks, format = "e", custom = TRUE, digits = 2)
brk_labels(scientific_brks, format = stringr::str_pad, side = "left", pad = "0", width = 3)
}
