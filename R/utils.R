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
#' @param name Character string of the slot/element to extract (e.g., "when")
#' @param recursive Logical, whether to search recursively through nested objects
#' @returns A list of all matching elements
#' @export
#' @examples
#' extract_elements_by_name(model, "when")
extract_ast_elements <- function(obj, name, recursive = TRUE) {
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

replace_ast_elements <- function(obj, name, replacement) {
  if (is.list(obj)) {
    for (i in seq_along(obj)) {
      if (is.list(obj[[i]])) {
        obj[[i]] <- replace_ast_elements(obj[[i]], name, replacement)
      }
      if (names(obj)[i] == name) {
        obj[[i]] <- replacement
      }
    }
  }
  obj
}

if (F) {
  # ast_type <- list(
  #   "when" = "cond",
  # )
  ast_type <- c("when", "mapping")
}

#' Replace AST elements with "ast_where" nodes
#'
#' This function replaces elements of a given AST object with "ast_where" nodes.
#' It is useful for long expressions to improve readability of latex output and
#' network trees. The "ast_where" stores the original expression in a `$content`
#' slot, and ignored by the parsers to GAMS, Julia, and other languages.
#'
#' @param obj An object of class `ast`, `multimod`, or a list.
#' @param ast_type A character vector of AST types to be replaced
#' (e.g., "when", "mapping").
#' @param name_prefix A character string prefix for the new names of the
#' replaced elements. Defaults to "s".
#'
#' @returns
#' @export
#'
remap_ast_elements <- function(obj,
                               ast_type = list("when" = "condition"),
                               name_prefix = "s",
                               n = 0L
                               ) {

  if (is_empty(ast_type) || is_empty(obj)) return(obj)

  remap_fun <- function(ob) {
    if (inherits(ob, names(ast_type))) {
      slot_names <- names(ob)
      slot_names <- slot_names[slot_names %in% ast_type[[node_type(ob)]]]
      if (length(slot_names) == 0) return(ob)
      for (slot_name in slot_names) {
        if (is.null(ob[[slot_name]])) next
        n <<- n + 1L; map_name <- paste0(name_prefix, n)
        ob[[slot_name]] <- ast_where(name = map_name, content = ob[[slot_name]])
      }
    } else if (inherits(ob, c("ast", "multimod", "list"))) {
      for (i in seq_along(ob)) {
        ob[[i]] <- remap_fun(ob[[i]])
      }
    }
    ob
  }
  obj <- remap_fun(obj)
  # browser()
  obj
}


extract_where_nodes <- function(ast) {
  result <- list()

  walk <- function(x) {
    if (inherits(x, "where")) {
      result[[x$name]] <<- x$content
    }
    if (is.list(x) && !is.data.frame(x)) {
      lapply(x, walk)
    }
  }

  walk(ast)
  result
}

#' Apply name aliases to AST nodes
#'
#' @param ast An AST object or list.
#' @param aliases A named list or character vector. Keys are aliases, values are original names.
#' @param classes Character vector of AST classes to filter (optional).
#'
#' @return Modified AST with aliases applied.
#' @export
alias_ast_names <- function(ast, aliases, classes = NULL) {
  # browser()
  stopifnot(is.list(aliases))

  # reverse_aliases <- setNames(names(aliases), unname(aliases))  # e.g. region -> r
  reverse_aliases <- aliases

  # Recursive rename function
  rename_walk <- function(x) {
    # browser()
    if (inherits(x, "ast")) {
      node_cls <- node_type(x)
      if (is.null(classes) || node_cls %in% classes) {
        if (!is.null(x$name) && x$name %in% names(reverse_aliases)) {
          x$name <- reverse_aliases[[x$name]]
        }
      }
      # Recurse into subfields
      for (i in seq_along(x)) {
        x[[i]] <- rename_walk(x[[i]])
      }
    } else if (is.list(x) && !is.data.frame(x)) {
      x <- lapply(x, rename_walk)
    }
    x
  }

  rename_walk(ast)
}

short_names_sets <- list(
  comm    = "c",  # commodity
  region  = "r",  # region
  year    = "y",  # year
  slice   = "t",  # time slice
  sup     = "s",  # supply
  dem     = "d",  # demand
  tech    = "n",  # technology
  stg     = "g",  # storage
  trade   = "z",  # interregional trade
  expp    = "x",  # export to ROW
  imp     = "m",  # import from ROW
  weather = "w",  # weather
  process = "p",  # process
  aux     = "a",  # auxiliary indicator (e.g. flags, switches)
  input   = "i",  # input flows to process
  output  = "o",  # output flows from process
  group   = "u",  # group of related commodities or tags
  # shorts for aliases
  techp   = "np",
  regionp = "rp",
  region2 = "r2",
  src     = "rs",
  dst     = "rd",
  yearp   = "yp",
  yeare   = "ye",
  yearn   = "yn",
  year2   = "y2",
  slicep  = "tp",
  slicepp = "tpp",
  slice2  = "t2",
  groupp  = "up",
  commp   = "cp",
  acomm   = "ca",
  comme   = "ce",
  supp    = "sp"
)

#' Resolve a dimension name to its alias
#'
#' @param name A character string or vector of dimension names.
#' @param base Named list of base aliases.
#' @param extended Named list of extended aliases.
#' @param default If no alias is found, return `name`, `NA`, or `"?"`.
#'
#' @return A character vector of resolved aliases.
#' @export
resolve_alias <- function(name,
                          base = base_aliases,
                          extended = extended_aliases,
                          default = NULL) {
  name <- as.character(name)
  out <- character(length(name))

  for (i in seq_along(name)) {
    n <- name[i]
    if (n %in% names(extended)) {
      out[i] <- extended[[n]]
    } else if (n %in% names(base)) {
      out[i] <- base[[n]]
    } else {
      out[i] <- if (!is.null(default)) default else n
    }
  }

  out
}

#' Resolve alias to full set name
#'
#' @param alias A character vector of alias names (e.g., "r", "yp").
#' @param base Named list of base aliases.
#' @param extended Named list of extended aliases.
#' @param default Fallback if alias not found.
#'
#' @return Character vector of full names.
#' @export
resolve_full_name <- function(alias,
                              base = base_aliases,
                              extended = extended_aliases,
                              default = NULL) {
  alias <- as.character(alias)

  # Invert mappings
  base_inv <- setNames(names(base), unname(base))
  ext_inv  <- setNames(names(extended), unname(extended))

  out <- character(length(alias))
  for (i in seq_along(alias)) {
    a <- alias[i]
    if (a %in% names(ext_inv)) {
      out[i] <- ext_inv[[a]]
    } else if (a %in% names(base_inv)) {
      out[i] <- base_inv[[a]]
    } else {
      out[i] <- if (!is.null(default)) default else a
    }
  }

  out
}



