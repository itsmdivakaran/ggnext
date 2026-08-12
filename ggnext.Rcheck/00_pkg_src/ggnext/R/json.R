# Minimal JSON serializer -----------------------------------------------------
#
# ggnext serializes its computed-geometry buffer to JSON for the interactive
# (<canvas>) render target. Rather than pull in jsonlite for a job this small,
# we write the ~60 lines ourselves: the buffer only ever contains NULL,
# logicals, numbers, strings, unnamed lists (arrays) and named lists (objects).
# This keeps the package's hard dependency graph at exactly {S7}.

#' Serialize an R object to a JSON string
#'
#' Internal, purpose-built serializer for the computed-geometry buffer.
#' Supports `NULL`, logical/numeric/character vectors, and (possibly nested)
#' lists. Named lists become JSON objects; unnamed lists and vectors of
#' length != 1 become JSON arrays.
#'
#' @param x Object to serialize.
#' @return A length-1 character vector containing JSON.
#' @noRd
to_json <- function(x) {
  if (is.null(x)) {
    return("null")
  }
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms) && all(nms != "")) {
      # Named list -> JSON object.
      fields <- vapply(
        seq_along(x),
        function(i) paste0(json_escape(nms[[i]]), ":", to_json(x[[i]])),
        character(1)
      )
      return(paste0("{", paste(fields, collapse = ","), "}"))
    }
    # Unnamed list -> JSON array.
    items <- vapply(x, to_json, character(1))
    return(paste0("[", paste(items, collapse = ","), "]"))
  }
  if (!is.atomic(x)) {
    stop("to_json() cannot serialize objects of class ", class(x)[[1]])
  }
  # Atomic vector: serialize each element, then wrap in [] unless scalar.
  elems <- vapply(seq_along(x), function(i) json_atom(x[[i]]), character(1))
  if (length(elems) == 1L && is.null(dim(x))) elems else {
    paste0("[", paste(elems, collapse = ","), "]")
  }
}

# Serialize one atomic element (logical, number, or string).
json_atom <- function(x) {
  if (is.na(x)) {
    return("null")
  }
  if (is.logical(x)) {
    return(if (x) "true" else "false")
  }
  if (is.numeric(x)) {
    # JSON has no Inf literal; null is the only honest representation.
    if (!is.finite(x)) {
      return("null")
    }
    # 15 significant digits round-trips doubles closely enough for pixel
    # math, while avoiding "0.30000000000000004"-style noise. Very small
    # magnitudes keep scientific notation rather than expanding to a
    # 300-character literal.
    fixed <- abs(x) == 0 || (abs(x) >= 1e-4 && abs(x) < 1e15)
    return(format(x, digits = 15, trim = TRUE, scientific = !fixed))
  }
  json_escape(as.character(x))
}

# Escape a string per RFC 8259 and wrap it in double quotes.
json_escape <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  # Remaining C0 control characters must be \uXXXX escapes (RFC 8259).
  if (grepl("[\001-\037]", s)) {
    for (cc in unique(unlist(regmatches(s, gregexpr("[\001-\037]", s))))) {
      s <- gsub(cc, sprintf("\\u%04x", utf8ToInt(cc)), s, fixed = TRUE)
    }
  }
  paste0("\"", s, "\"")
}
