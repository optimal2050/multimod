#' Check if a string is a compound expression
#'
#' @param s Character string (GAMS-like expression)
#' @param ops Character vector of known operators (defaults: arithmetic, logical, relational)
#' @return Logical: TRUE if expression, FALSE if atomic
#' @export
#'
#' @examples
#' is_expression("x + y") # TRUE
#' is_expression("x") # FALSE
#'
is_expression <- function(
    s,
    ops = c(
      "+", "-", "*", "/", "^",
      "=", "==", "<", "<=", ">", ">=",
      "=e=", "=l=", "=g=", "=le=", "=ge=",
      "and", "or", "not"
    )) {
  if (!is.character(s) || length(s) != 1) {
    return(FALSE)
  }

  s <- trimws(s)

  if (s == "") {
    return(FALSE)
  }

  # Remove leading/trailing brackets
  while (startsWith(s, "(") && endsWith(s, ")")) {
    inner <- substr(s, 2, nchar(s) - 1)
    # Ensure balanced parentheses
    if (sum(strsplit(inner, "")[[1]] == "(") == sum(strsplit(inner, "")[[1]] == ")")) {
      s <- trimws(inner)
    } else {
      break
    }
  }

  # Check for top-level operators
  top_ops <- top_level_operators(s, ops = ops)
  return(length(top_ops) > 0)
}

#' Get the depth of a nested list structure (AST, multimod, and other objects)
#'
#' This function calculates the depth of a nested list structure.
#' Wrapper for `purrr::pluck_depth`.
#'
#' @param x A list or nested list structure.
#'
#' @returns An integer representing the depth of the list.
#' @export
depth <- function(x) {
  purrr::pluck_depth(x)
}

#' Check if a character is a special character
#'
#' @param ch A character string to check.
#'
#' @returns A logical value indicating whether the character is a special character.
#' @export
#'
#' @examples
#' is_special("!") # TRUE
#' is_special("a") # FALSE
#' is_special("1") # FALSE
#' is_special(1) # FALSE
#' is_special("#") # TRUE
is_special <- function(ch) {
  return(grepl("^[[:punct:]]+$", ch))
}

#' Check if a string is a word (alphabetic characters only)
#'
#' This function tests whether the input string consists entirely of letters (a–z, A–Z)
#' with no digits, punctuation, or special characters.
#'
#' @param ch A character string or vector of strings to test.
#'
#' @returns A logical vector the same length as `ch`, where each element is `TRUE`
#' if the corresponding string consists only of letters, `FALSE` otherwise.
#'
#' @export
#'
#' @examples
#' is_word("alpha")     # TRUE
#' is_word("123")       # FALSE
#' is_word("var_1")     # FALSE
#' is_word(c("a", "B", "C3"))  # TRUE, TRUE, FALSE
is_word <- function(ch) {
  return(grepl("^[a-zA-Z]+$", ch))
}


#' Check if a string is alphanumeric (letters, digits, or underscores)
#'
#' This function tests whether the input string consists entirely of
#' letters (`a–z`, `A–Z`), digits (`0–9`), or underscores (`_`).
#'
#' @param ch A character string or vector of strings to test.
#'
#' @returns A logical vector the same length as `ch`, where each element is `TRUE`
#' if the corresponding string contains only alphanumeric characters and underscores,
#' `FALSE` otherwise.
#'
#' @export
#'
#' @examples
#' is_word_num("alpha123")     # TRUE
#' is_word_num("var_1")        # TRUE
#' is_word_num("a-b")          # FALSE
#' is_word_num(c("abc", "123", "a_b", "x-y"))  # TRUE, TRUE, TRUE, FALSE
is_word_num <- function(ch) {
  return(grepl("^[a-zA-Z0-9_]+$", ch))
}


.split_line_on_pattern <- function(line,
                                   pattern,
                                   position = c("before", "after", "both"),
                                   ignore.case = FALSE) {
  position <- match.arg(position)
  if (length(pattern) == 0) return(line)

  # Escape special characters
  # safe_patterns <- sapply(pattern, function(p) {
  #   if (grepl("^[a-zA-Z0-9_]+$", p)) return(p)
  #   return(sprintf("(?<!\\\\)%s", gsub("([\\^$.|?*+(){}])", "\\\\\\1", p)))
  # })
  safe_patterns <- pattern #!!! reconsider later

  combined_pattern <- paste0("(", paste(safe_patterns, collapse = "|"), ")")

  if (!grepl(combined_pattern, line, perl = TRUE, ignore.case = ignore.case)) return(line)

  # Match and split
  m <- gregexpr(combined_pattern, line, perl = TRUE, ignore.case = ignore.case)[[1]]
  if (m[1] == -1) return(line)

  matches <- regmatches(line, gregexpr(combined_pattern, line, perl = TRUE, ignore.case = ignore.case))[[1]]
  result <- character()
  start <- 1

  for (i in seq_along(m)) {
    match_start <- m[i]
    match_text <- matches[i]
    match_end <- match_start + nchar(match_text) - 1

    if (position == "before") {
      result <- c(result, substr(line, start, match_start - 1))
      start <- match_start
    } else if (position == "after") {
      result <- c(result, substr(line, start, match_end))
      start <- match_end + 1
    } else if (position == "both") {
      result <- c(result, substr(line, start, match_start - 1), match_text)
      start <- match_end + 1
    }
  }

  if (start <= nchar(line)) {
    result <- c(result, substr(line, start, nchar(line)))
  }

  return(trimws(result))
}

split_line_on_pattern <- function(
    lines,
    pattern,
    position = c("before", "after", "both"),
    ignore.case = FALSE) {
  position <- match.arg(position)
  out <- list()

  for (line in lines) {
    if (!nzchar(trimws(line))) {
      out <- c(out, "")  # preserve blank lines
      next
    }

    split_line <- .split_line_on_pattern(
      line = line,
      pattern = pattern,
      position = position,
      ignore.case = ignore.case
    )

    out <- c(out, split_line)
  }

  return(unlist(out))
}

#' Recursively extract elements by name from a nested structure
#'
#' @param obj A nested list or S3 object (e.g., multimod or ast node)
#' @param name Character string of the slot/element to extract (e.g., "condition")
#' @param recursive Logical, whether to search recursively through nested objects
#' @returns A list of all matching elements
#' @export
#' @examples
#' extract_elements_by_name(model, "condition")
extract_elements_by_name <- function(obj, name, recursive = TRUE) {
  results <- list()

  walk <- function(x) {
    if (is.list(x)) {
      if (!is.null(x[[name]])) {
        results[[length(results) + 1]] <<- x[[name]]
      }
      if (recursive) {
        for (el in x) walk(el)
      }
    }
  }

  walk(obj)
  results
}

