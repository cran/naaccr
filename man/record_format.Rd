% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/record_format.R
\name{record_format}
\alias{record_format}
\alias{as.record_format}
\title{Define custom fields for NAACCR records}
\usage{
record_format(
  name,
  item,
  start_col = NA_integer_,
  end_col = NA_integer_,
  type = "character",
  alignment = "left",
  padding = " ",
  parent = "Tumor",
  cleaner = list(NULL),
  unknown_finder = list(NULL),
  name_literal = NA_character_,
  width = NA_integer_
)

as.record_format(x, ...)
}
\arguments{
\item{name}{Item name appropriate for a \code{data.frame} column name.}

\item{item}{NAACCR item number.}

\item{start_col}{First column of the field in a fixed-width record.}

\item{end_col}{*Deprecated: Use the \code{width} parameter instead.*
Last column of the field in a fixed-width record.}

\item{type}{Name of the column class.}

\item{alignment}{Alignment of the field in fixed-width files. Either
\code{"left"} (default) or \code{"right"}.}

\item{padding}{Single-character strings to use for padding in fixed-width
files.}

\item{parent}{Name of the parent node to include this field under when
writing to an XML file.
Values can be \code{"NaaccrData"}, \code{"Patient"}, \code{"Tumor"}, or
\code{NA} (default).
Fields with \code{NA} for parent won't be included in an XML file.}

\item{cleaner}{(Optional) List of functions to handle special cases of
cleaning field data (e.g., convert all values to uppercase).
Values of \code{NULL} (the default) mean the default cleaning function for
the \code{type} is used.
The value can also be the name of a function to retrieve with
\code{\link[methods:methodUtilities]{getFunction}}.
See Details.}

\item{unknown_finder}{(Optional) List of functions to detect when codes mean
the actual values are unknown or not applicable.
Values of \code{NULL} (the default) mean the default unknown finding
function for the \code{type} is used.
The value can also be the name of a function to retrieve with
\code{\link[methods:methodUtilities]{getFunction}}.
See Details.}

\item{name_literal}{(Optional) Item name in plain language.}

\item{width}{(Optional) Item width in characters.}

\item{x}{Object to be coerced to a \code{record_format}, usually a
\code{data.frame} or \code{list}.}

\item{...}{Other arguments passed to \code{record_format}.}
}
\value{
An object of class \code{"record_format"} which has the following
  columns:
  \describe{
    \item{\code{name}}{
      (\code{character}) XML field name.
    }
    \item{\code{item}}{
      (\code{integer}) Field item number.
    }
    \item{\code{start_col}}{
      (\code{integer}) First column of the field in a fixed-width text file.
      If \code{NA}, the field will not be read from or written to fixed-width
      files. They will included in XML files.
    }
    \item{\code{end_col}}{
      (\code{integer}) (*Deprecated: Use \code{width} instead.*)
      Last column of the field in a fixed-width text file.
      If \code{NA}, the field will not be read from or written to fixed-width
      files. This is the norm for fields only found in XML formats.
    }
    \item{\code{type}}{
      (\code{factor}) R class for the column vector.
    }
    \item{\code{alignment}}{
      (\code{factor}) Alignment of the field's values in a fixed-width
      text file.
    }
    \item{\code{padding}}{
      (\code{character}) String used for padding field values in a
      fixed-width text file.
    }
    \item{\code{parent}}{
      (\code{factor}) Parent XML node for the field. One of
      \code{"NaaccrData"}, \code{"Patient"}, or \code{"Tumor"}.
    }
    \item{\code{cleaner}}{
      (\code{list} of \code{function} objects) Function to prepare the
      field's values for analysis.
      Values of \code{NULL} will use the standard cleaner functions for the
      \code{type} (see below).
    }
    \item{\code{unknown_finder}}{
      (\code{list} of \code{function} objects) Function to detect codes
      meaning the actual values are missing or unknown for the field.
    }
    \item{\code{name_literal}}{
      (\code{character}) Field name in plain language.
    }
    \item{\code{width}}{
      (\code{integer}) Character width of the field values.
      Mostly meant for reading and writing flat files.
    }
  }
}
\description{
Create a \code{record_format} object, which is used to read NAACCR records.
}
\details{
To define registry-specific fields in addition to the standard fields, create
a \code{record_format} object for the registry-specific fields and combine it
with one of the formats provided with the package using \code{rbind}.
}
\section{Format Types}{


  The levels \code{type} can take, along with the functions used to process
  them when reading a file:

  \describe{
    \item{\code{address}}{
      (\code{\link{clean_address_number_and_street}})
      Street number and street name parts of an address.
    }
    \item{\code{age}}{
      (\code{\link{clean_age}})
      Age in years.
    }
    \item{\code{boolean01}}{
      (\code{\link{naaccr_boolean}}, with \code{false_value = "0"})
      True/false, where \code{"0"} means false and \code{"1"} means true.
    }
    \item{\code{boolean12}}{
      (\code{\link{naaccr_boolean}}, with \code{false_value = "1"})
      True/false, where \code{"1"} means false and \code{"2"} means true.
    }
    \item{\code{census_block}}{
      (\code{\link{clean_census_block}})
      Census Block ID number.
    }
    \item{\code{census_tract}}{
      (\code{\link{clean_census_tract}})
      Census Tract ID number.
    }
    \item{\code{character}}{
      (\code{\link{clean_text}})
      Miscellaneous text.
    }
    \item{\code{city}}{
      (\code{\link{clean_address_city}})
      City name.
    }
    \item{\code{count}}{
      (\code{\link{clean_count}})
      Integer count.
    }
    \item{\code{county}}{
      (\code{\link{clean_county_fips}})
      County FIPS code.
    }
    \item{\code{Date}}{
      (\code{\link{as.Date}}, with \code{format = "\%Y\%m\%d"})
      NAACCR-formatted date (YYYYMMDD).
    }
    \item{\code{datetime}}{
      (\code{\link{as.POSIXct}}, with \code{format = "\%Y\%m\%d\%H\%M\%S"})
      NAACCR-formatted datetime (YYYYMMDDHHMMSS)
    }
    \item{\code{facility}}{
      (\code{\link{clean_facility_id}})
      Facility ID number.
    }
    \item{\code{icd_9}}{
      (\code{\link{clean_icd_9_cm}})
      ICD-9-CM code.
    }
    \item{\code{icd_code}}{
      (\code{\link{clean_icd_code}})
      ICD-9 or ICD-10 code.
    }
    \item{\code{integer}}{
      (\code{\link{as.integer}})
      Miscellaneous whole number.
    }
    \item{\code{numeric}}{
      (\code{\link{as.numeric}})
      Miscellaneous decimal number.
    }
    \item{\code{override}}{
      (\code{\link{naaccr_override}})
      Field describing why another field's value was over-ridden.
    }
    \item{\code{physician}}{
      (\code{\link{clean_physician_id}})
      Physician ID number.
    }
    \item{\code{postal}}{
      (\code{\link{clean_postal}})
      Postal code for an address (a.k.a. ZIP code in the United States).
    }
    \item{\code{ssn}}{
      (\code{\link{clean_ssn}})
      Social Security Number.
    }
    \item{\code{telephone}}{
      (\code{\link{clean_telephone}})
      10-digit telephone number.
    }
  }
}

\examples{
  my_fields <- record_format(
    name      = c("foo", "bar", "baz"),
    item      = c(2163, 1180, 1181),
    start_col = c(975, 1381, NA),
    width     = c(1, 55, 4),
    type      = c("numeric", "facility", "character"),
    parent    = c("Patient", "Tumor", "Tumor"),
    cleaner   = list(NULL, NULL, trimws)
  )
  my_format <- rbind(naaccr_format_16, my_fields)
}
