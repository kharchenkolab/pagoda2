\name{pagoda2From}
\alias{pagoda2From}
\title{Create A Pagoda2 Object From Input}
\usage{
pagoda2From(x, format = NULL, reader.args = list(), ...)
}
\arguments{
\item{x}{Matrix-like object or input path.}

\item{format}{Input format for paths. \code{NULL} guesses from input.}

\item{reader.args}{Named list of arguments passed to \code{readCounts()}.}

\item{...}{Arguments passed to \code{Pagoda2$new()}.}
}
\value{
Pagoda2 object.
}
\description{
Create a Pagoda2 object from a count matrix or supported input path. Directory
paths are currently read through \code{readCounts()}.
}
