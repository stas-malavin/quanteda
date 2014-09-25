% Generated by roxygen2 (4.0.2): do not edit by hand
\name{ndoc}
\alias{ndoc}
\alias{ndoc.corpus}
\alias{ndoc.dfm}
\title{get the number of documents}
\usage{
ndoc(x)

\method{ndoc}{corpus}(x)

\method{ndoc}{dfm}(x, ...)
}
\arguments{
\item{x}{a corpus or dfm object}

\item{...}{additional parameters}
}
\value{
an integer (count) of the number of documents in the corpus or dfm
}
\description{
Returns the number of documents in a corpus objects
}
\examples{
ndoc(inaugCorpus)
ndoc(dfm(inaugCorpus))
}
