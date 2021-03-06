% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind_self.R
\name{bind_self}
\alias{bind_self}
\title{Neatly bind a data frame to itself, with a column changed}
\usage{
bind_self(.data, group, new_value, append = FALSE, .id = NULL)
}
\arguments{
\item{.data}{A data frame.}

\item{group}{Bare column name of the column needing to be changed.}

\item{new_value}{String giving the new value for column \code{group}.}

\item{append}{Logical: should new values be appended to the end of \code{data}, or prepended to the beginning (the default)?}

\item{.id}{Optional, inheriting from \verb{[dplyr::bind_rows()]}: if a string, will create IDs for each data frame and create a column of IDs with the name given as \code{.id}.}
}
\value{
A data frame with twice as many rows as the original.
}
\description{
This function changes the value of a data frame's column, then binds back to itself in order to fit into a pipeline. This preserves grouping, and if \code{group} is a factor, returns \code{group} as a factor with expanded levels.
}
\examples{
town_populations <- data.frame(
  name = c("East Haven", "Hamden", "West Haven"),
  pop = c(29015, 61476, 54972)
)
town_populations \%>\%
  dplyr::mutate(measure = "total_pop") \%>\%
  bind_self(group = name, new_value = "Inner Ring towns")

town_populations \%>\%
  dplyr::group_by(name) \%>\%
  bind_self(group = name, new_value = "Inner Ring towns") \%>\%
  dplyr::summarise(pop = sum(pop))
}
