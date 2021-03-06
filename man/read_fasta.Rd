% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fasta.r
\name{read_fasta}
\alias{read_fasta}
\title{Reads fasta file.}
\usage{
read_fasta(file, skip = 0, nlines = 0, comment.char = "")
}
\arguments{
\item{file}{Name of fasta file}

\item{skip}{the number of  lines of the input file to skip before
    beginning to read data values.}

\item{nlines}{if positive, the maximum number of lines of data to be read.}

\item{comment.char}{character: a character vector of length one
    containing a single character or an empty string.  Use \code{""} to
    turn off the interpretation of comments altogether (the default).}
}
\value{
List of sequences.
}
\description{
Reads fasta file from provided path and returns list of sequences (strings)
with sequence names as names of items in list. This include some manipulation
with sequence names to remove trailing whitespace and standardization of
sequences themselves (uppercase) for future smooth processing.
Also, if illegal characters are found in sequence, throws warning.
}

