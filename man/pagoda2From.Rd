\name{pagoda2From}
\alias{pagoda2From}
\alias{pagoda2From10x}
\alias{pagoda2From10xH5}
\alias{pagoda2FromAnnData}
\alias{pagoda2FromH5Seurat}
\alias{pagoda2FromLoom}
\alias{readPagoda2}
\title{Create A Pagoda2 Object From Input}
\usage{
pagoda2From(x, format = NULL, reader.args = list(), ...)

pagoda2From10x(path, reader.args = list(), ...)

pagoda2From10xH5(path, reader.args = list(), ...)

pagoda2FromAnnData(path, reader.args = list(), ...)

pagoda2FromH5Seurat(path, reader.args = list(), ...)

pagoda2FromLoom(path, reader.args = list(), ...)

readPagoda2(path, format = NULL, reader.args = list(), ...)
}
\arguments{
\item{x}{Matrix-like object or input path.}

\item{path}{Input path.}

\item{format}{Input format for paths. \code{NULL} guesses from input.}

\item{reader.args}{Named list of arguments passed to \code{readCounts()}.}

\item{...}{Arguments passed to \code{Pagoda2$new()}.}
}
\value{
Pagoda2 object.
}
\description{
Create a Pagoda2 object from a count matrix or supported input path. The
\code{Pagoda2$from()} generator method and \code{readPagoda2()} auto-detect
supported file formats; specific helpers fix the input format explicitly.
}
