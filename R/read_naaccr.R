#' Create a connection specified in one of several ways
#' @param input Either a string with a file name (containing no \code{\\n}
#'   character), a \code{\link[base:connections]{connection}} object, or the
#'   text records themselves as a character vector.
#' @param encoding String giving the input's encoding. See the 'Encoding'
#'   section of \code{\link[base]{file}} in the \pkg{base} package.
#' @return An open \code{connection} object
#' @seealso
#'   \code{\link[base]{connections}},
#'   \code{\link[base]{textConnection}}
#' @noRd
as.connection <- function(input, encoding) {
  # Based on logic in utils::read.table
  if (is.character(input)) {
    value <- input
    input <- if (length(input) > 1L || grepl('\n', input[1L], fixed = TRUE)) {
      if (identical(as.character(encoding), "native.enc")) {
        textConnection(value)
      } else {
        textConnection(value, encoding = encoding)
      }
    } else {
      file(input, open = 'rt', encoding = encoding)
    }
  }
  if (!inherits(input, 'connection')) {
    stop("'input' must be a filepath, a connection, or a character vector")
  }
  if (!isOpen(input, 'read')) {
    open(input, 'rt')
  }
  input
}


#' @param record_lines Character vector of entire NAACCR record lines.
#' @param start_cols   Integer vector of the first index of fields in the
#'   records. Must be the same length as \code{end_cols}.
#' @param end_cols     Integer vector of the last index of fields in the
#'   records. Must be the same length as \code{start_cols}.
#' @param col_names    Character vector of field names.  Must be the same length
#' @return A \code{data.frame} with the columns specified by \code{start_cols},
#'   \code{end_cols}, and \code{col_names}. All columns are character vectors.
#' @noRd
#' @import stringi
#' @import data.table
split_fields <- function(record_lines,
                         start_cols,
                         end_cols,
                         col_names = NULL) {
  if (length(start_cols) != length(end_cols)) {
    stop("start_cols and end_cols must be the same length")
  }
  if (!is.null(col_names) && length(start_cols) != length(col_names)) {
    stop("start_cols and end_cols must be the same length")
  }
  item_matrix <- stringi::stri_sub_all(record_lines, start_cols, end_cols)
  item_matrix <- do.call(rbind, item_matrix)
  item_matrix[] <- stringi::stri_trim_both(item_matrix)
  item_matrix <- as.data.table(item_matrix)
  setnames(item_matrix, col_names)
  item_matrix
}


#' Read NAACCR records from a file
#'
#' Read and parse cancer incidence records according to a NAACCR format from
#' either fixed-width files (\code{read_naaccr} and \code{read_naaccr_plain})
#' or XML documents (\code{read_naaccr_xml} and \code{read_naaccr_xml_plain}).
#'
#' \code{read_naaccr} and \code{read_naaccr_xml} return data sets suited for
#' analysis in R.
#' \code{read_naaccr_plain} and \code{read_naaccr_xml_plain} return data sets
#' with the unchanged record values.
#'
#' Anyone who wants to analyze the records in R should use \code{read_naaccr}
#' or \code{read_naaccr_xml}.
#' In the returned \code{\link{naaccr_record}}, columns are of appropriate
#' classes, coded values are replaced with factors, and unknowns are replaced
#' with \code{NA}.
#'
#' \code{read_naaccr_plain} and \code{read_naaccr_xml_plain} is a "format strict"
#' way to read incidence records.
#' All values returned are the literal character values from the records.
#' The only processing done is that leading and trailing whitespace is trimmed.
#' This is useful if the values will be passed to other software that expects
#' the plain NAACCR values.
#'
#' For \code{read_naaccr_plain} and \code{read_naaccr}, if the \code{version}
#' and \code{format} arguments are left \code{NULL}, the default format is
#' version 18. This was the last format to be used for fixed-width files.
#'
#' @param input Either a string with a file name (containing no \code{\\n}
#'   character), a \code{\link[base:connections]{connection}} object, or the text records
#'   themselves as a character vector.
#' @param keep_fields Character vector of XML field names to keep in the
#'   dataset. If \code{NULL} (default), all columns are kept.
#' @param skip An integer specifying the number of lines of the data file to
#'   skip before beginning to read data.
#' @param nrows A number specifying the maximum number of records to read.
#'   \code{Inf} (the default) means "all records."
#' @param buffersize Maximum number of lines to read at one time.
#' @param encoding String giving the input's encoding. See the 'Encoding'
#'   section of \code{\link[base:connections]{file}} in the \pkg{base} package.
#'   For \code{read_naaccr_xml} and \code{read_naaccr_xml_plain}, this is a
#'   \emph{backup} encoding. If the XML document includes an encoding
#'   specification, that will be used. Otherwise, \code{encoding} will be used.
#' @param as_text Logical indicating (if \code{TRUE}) that \code{input} is a
#'   character string containing XML or (if \code{FALSE}) it is the path to a
#'   file with XML content.
#' @param ... Additional arguments passed onto \code{\link{as.naaccr_record}}.
#' @inheritParams naaccr_record
#' @return
#'   For \code{read_naaccr}, a \code{data.frame} of the records.
#'   The columns included depend on the NAACCR \code{\link{record_format}} version.
#'   Columns are atomic vectors; there are too many to describe them all.
#'
#'   For \code{read_naaccr_plain}, a \code{data.frame} based on the
#'   \code{record_format} specified by either the \code{version} or
#'   \code{format} argument.
#'   The names of the columns will be those in the format's \code{name} column.
#'   All columns are character vectors.
#' @note
#'   Some of the parameter text was shamelessly copied from the
#'   \code{\link[utils]{read.table}} and \code{\link[utils]{read.fwf}} help
#'   pages.
#' @references
#'  North American Association of Central Cancer Registries (October 2018).
#'  Standards for Cancer Registries Volume II: Data Standards and Data Dictionary.
#'  Twenty first edition.
#'  \url{https://apps.naaccr.org/data-dictionary/}.
#'
#'  North American Association of Central Cancer Registries (April 2019).
#'  NAACCR XML Data Exchange Standard. Version 1.4.
#'  \url{https://www.naaccr.org/xml-data-exchange-standard/}.
#' @seealso \code{\link{naaccr_record}}
#' @examples
#'   # This file has synthetic abstract records
#'   incfile <- system.file(
#'     "extdata", "synthetic-naaccr-18-abstract.txt",
#'     package = "naaccr"
#'   )
#'   fields <- c("ageAtDiagnosis", "sex", "sequenceNumberCentral")
#'   read_naaccr(incfile, version = 18, keep_fields = fields)
#'   recs <- read_naaccr_plain(incfile, version = 18, keep_fields = fields)
#'   recs
#'   # Note sequenceNumberCentral has been split in two: a number and a flag
#'   summary(recs[["sequenceNumberCentral"]])
#'   summary(recs[["sequenceNumberCentralFlag"]])
#' @import stringi
#' @import data.table
#' @rdname read_naaccr
#' @export
read_naaccr_plain <- function(input,
                              version = NULL,
                              format = NULL,
                              keep_fields = NULL,
                              skip = 0,
                              nrows = Inf,
                              buffersize = 10000,
                              encoding = getOption("encoding")) {
  if (!inherits(input, "connection")) {
    input <- as.connection(input, encoding = encoding)
    on.exit(
      if (isOpen(input)) close(input),
      add = TRUE
    )
  }
  # The default format version is 18, the last one that supported fixed-width
  if (is.null(version) && is.null(format)) {
    format <- naaccr_format_18
  }
  format <- choose_naaccr_format(
    version = version, format = format, keep_fields = keep_fields
  )
  unread_fields <- !is.finite(format[["start_col"]]) |
    !is.finite(format[["width"]])
  unread_format <- format[unread_fields]
  read_format <- format[!unread_fields]
  if (nrow(read_format) == 0L) {
    stop("No fields in the format have a finite start column and width")
  }
  # Read all record types as the longest type, padding and then truncating
  # Break the reading into chunks because of the typically large files.
  # "Growing" vectors is inefficient, so allocate many new spaces when needed
  if (skip > 0L) {
    readLines(input, n = skip, encoding = encoding)
  }
  chunks <- if (is.finite(nrows)) {
    vector("list", ceiling(nrows / buffersize))
  } else {
    vector("list", 1000L)
  }
  index <- 0L
  rows_read <- 0L
  end_cols <- read_format[["start_col"]] + read_format[["width"]] - 1L
  while (rows_read < nrows) {
    chunk_size <- min(buffersize, nrows - rows_read)
    record_lines <- readLines(input, n = chunk_size, encoding = encoding)
    if (length(record_lines) == 0L) {
      break
    }
    rows_read <- rows_read + length(record_lines)
    index <- index + 1L
    line_lengths <- stringi::stri_width(record_lines)
    record_width <- max(end_cols, na.rm = TRUE)
    record_lines <- stringi::stri_pad_right(
      record_lines,
      width = record_width - line_lengths
    )
    record_lines <- stringi::stri_sub(record_lines, 1L, record_width)
    chunks[[index]] <- split_fields(
      record_lines = record_lines,
      start_cols   = read_format[["start_col"]],
      end_cols     = end_cols,
      col_names    = read_format[["name"]]
    )
    if (index >= length(chunks)) {
      chunks <- c(chunks, vector("list", 1000L))
    }
  }
  if (rows_read == 0L) {
    records <- rep(list(character(0L)), nrow(format))
    setDT(records)
    setnames(records, format[["name"]])
  } else {
    records <- data.table::rbindlist(chunks)
    if (nrow(unread_format) > 0L) {
      set(records, j = unread_format[["name"]], value = "")
    }
  }
  setcolorder(records, format[["name"]])
  setDF(records)
  records
}


#' @rdname read_naaccr
#' @export
read_naaccr <- function(input,
                        version = NULL,
                        format = NULL,
                        keep_fields = NULL,
                        keep_unknown = FALSE,
                        skip = 0,
                        nrows = Inf,
                        buffersize = 10000,
                        encoding = getOption("encoding"),
                        ...) {
  records <- read_naaccr_plain(
    input = input,
    version = version,
    format = format,
    keep_fields = keep_fields,
    skip = skip,
    nrows = nrows,
    buffersize = buffersize,
    encoding = encoding
  )
  as.naaccr_record(
    x = records,
    keep_unknown = keep_unknown,
    version = version,
    format = format,
    ...
  )
}


#' Gather all <Item> nodes under a <NaaccrData>, <Patient>, or <Tumor>.
#' These will be combined with other node-items in a single table.
#' @importFrom XML getNodeSet xmlGetAttr xmlValue
#' @import stringi
#' @noRd
items_to_row <- function(parent_node, keep_fields) {
  items <- getNodeSet(parent_node, "ns:Item", namespaces = "ns")
  ids <- vapply(items, xmlGetAttr, character(1), name = "naaccrId")
  names(items) <- ids
  if (!is.null(keep_fields)) {
    items <- items[ids %in% keep_fields]
  }
  values <- vapply(items, xmlValue, character(1))
  values <- stringi::stri_trim_both(values)
  out <- as.list(values)
  names(out) <- names(items)
  out
}


#' @importFrom XML getNodeSet
#' @noRd
make_patient_table <- function(patient, keep_fields) {
  tumors <- getNodeSet(patient, "ns:Tumor", namespaces = "ns")
  tumor_rows <- lapply(tumors, items_to_row, keep_fields = keep_fields)
  patient_table <- rbindlist(tumor_rows, use.names = TRUE, fill = TRUE)
  patient_items <- items_to_row(patient, keep_fields = keep_fields)
  if (length(patient_items)) {
    set(patient_table, j = names(patient_items), value = patient_items)
  }
  patient_table
}


#' @importFrom XML getNodeSet
#' @noRd
make_registry_table <- function(registry, keep_fields) {
  patients <- getNodeSet(registry, "ns:Patient", namespaces = "ns")
  patient_rows <- lapply(patients, make_patient_table, keep_fields = keep_fields)
  registry_table <- rbindlist(patient_rows, use.names = TRUE, fill = TRUE)
  registry_items <- items_to_row(registry, keep_fields = keep_fields)
  if (length(registry_items)) {
    set(registry_table, j = names(registry_items), value = registry_items)
  }
  registry_table
}


#' @importFrom XML xmlInternalTreeParse free getNodeSet xmlAttrs
#' @importFrom stringi stri_detect_regex stri_extract_first_regex
#' @import data.table
#' @rdname read_naaccr
#' @export
read_naaccr_xml_plain <- function(input,
                                  version = NULL,
                                  format = NULL,
                                  keep_fields = NULL,
                                  as_text = FALSE,
                                  encoding = getOption("encoding")) {
  if (inherits(input, "connection")) {
    input <- readLines(input, encoding = encoding)
    as_text <- TRUE
  }
  tree <- xmlInternalTreeParse(input, ignoreBlanks = FALSE, asText = as_text)
  on.exit(free(tree), add = TRUE)
  registry_nodes <- getNodeSet(tree, "//ns:NaaccrData", namespaces = "ns")
  registry_tables <- lapply(
    registry_nodes, make_registry_table, keep_fields = keep_fields
  )
  records <- rbindlist(registry_tables, use.names = TRUE, fill = TRUE)
  # NAACCR XML files must include the standard dictionary used.
  # If the records don't already have a "naaccrRecordVersion" field, set it
  # based on what the user chooses for `version` or `format`.
  # If neither is given, use what the file says.
  use_ver_num <- is.null(version) && is.null(format)
  ver_num <- NULL
  if (use_ver_num) {
    top_attrs <- xmlAttrs(registry_nodes[[1L]])
    base_dict <- top_attrs[["baseDictionaryUri"]]
    dict_pattern <- "^http://naaccr.org/naaccrxml/naaccr-dictionary-(\\d{3}).xml$"
    ver_num <- stri_match_first_regex(base_dict, dict_pattern)[[2L]]
    if (is.na(ver_num) || !(ver_num %in% names(naaccr_formats))) {
      warning(paste('The base dictionary specified is not recognized:', base_dict))
      ver_num <- max(names(naaccr_formats))
    }
    fmt <- choose_naaccr_format(version = ver_num, keep_fields = keep_fields)
  } else if (!is.null(version)) {
    fmt <- choose_naaccr_format(version = version, keep_fields = keep_fields)
    ver_num <- formatC(as.integer(version), format = "d")
  } else if (!is.null(format)) {
    # See if the format is equivalent to an official one
    for (official_num in rev(names(naaccr_formats))) {
      official <- naaccr_formats[[official_num]]
      same_values <- isTRUE(all.equal(format, official, check.attributes = FALSE))
      same_names <- identical(names(format), names(official))
      if (same_values && same_names) {
        format <- official
        ver_num <- official_num
        break
      }
    }
    fmt <- choose_naaccr_format(format = format, keep_fields = keep_fields)
  } else{
    fmt <- choose_naaccr_format(version, format, keep_fields = keep_fields)
  }
  if (is.null(keep_fields)) {
    keep_fields <- fmt[["name"]]
  }
  missing_fields <- setdiff(keep_fields, names(records))
  if (length(missing_fields) > 0L) {
    set(records, j = missing_fields, value = "")
  }
  setcolorder(records, keep_fields)
  if (!is.null(ver_num) && "naaccrRecordVersion" %in% keep_fields) {
    need_ver_num <- which(!nzchar(records[["naaccrRecordVersion"]]))
    set(records, i = need_ver_num, j = "naaccrRecordVersion", value = ver_num)
  }
  setDF(records)
  records
}


#' @rdname read_naaccr
#' @export
read_naaccr_xml <- function(input,
                            version = NULL,
                            format = NULL,
                            keep_fields = NULL,
                            keep_unknown = FALSE,
                            as_text = FALSE,
                            encoding = getOption("encoding"),
                            ...) {
  records <- read_naaccr_xml_plain(
    input = input,
    keep_fields = keep_fields,
    as_text = as_text,
    encoding = encoding
  )
  ver_nums <- unique(records[["naaccrRecordVersion"]])
  ver_nums <- ver_nums[!is.na(ver_nums)]
  if (is.null(version) && is.null(format) && length(ver_nums) > 0L) {
    if (length(ver_nums) > 1L) {
      warning("Multiple NAACCR versions specified in records. Using most recent.")
    }
    version <- max(ver_nums)
  }
  as.naaccr_record(
    x = records,
    keep_unknown = keep_unknown,
    version = version,
    format = format,
    ...
  )
}
