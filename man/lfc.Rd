% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_files.R
\name{lfc}
\alias{lfc}
\title{List Files in Character (Vector)}
\usage{
lfc(.dirs, .reg = NULL, .rec = FALSE)
}
\arguments{
\item{.dirs}{Vector/List or single paths to directory/directories}

\item{.reg}{RegEx to find files (defaults to '*' all files)}

\item{.rec}{Should the directories be searched recursively?}
}
\value{
A character vector with paths
}
\description{
List Files in Character (Vector)
}
\examples{
library(rUtils)
dir <- system.file("extdata", package = "rUtils")
lfc(dir)
rm(dir)
}
