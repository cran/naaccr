% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_naaccr.R
\name{write_naaccr}
\alias{write_naaccr}
\title{Write records in NAACCR format}
\usage{
write_naaccr(records, con, version = NULL, format = NULL, encoding = "UTF-8")
}
\arguments{
\item{records}{A \code{naaccr_record} object.}

\item{con}{Either a character string naming a file or a
\code{\link[base:connections]{connection}} open for writing.}

\item{version}{An integer specifying the NAACCR format version for parsing
the records. Use this or \code{format}, not both. If both \code{version}
and \code{format} are \code{NULL} (the default), the most recent version is
used.}

\item{format}{A \code{\link{record_format}} object for writing the records.}

\item{encoding}{String specifying the character encoding for the output file.}
}
\description{
Write records from a \code{\link{naaccr_record}} object to a connection in
fixed-width format, according to a specific version of the NAACCR format.
}
