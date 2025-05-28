#' Write a full GAMS model file from a multimod object
#'
#' @param model A `multimod` model object
#' @param file Output file path (optional)
#' @param format_expr logical; whether to format expressions with line breaks
#' @param ... Additional arguments passed to formatting functions
#'
#' @return Character vector or writes file if `file` is given
write_gams.multimod <- function(model, file = NULL, format_expr = TRUE, ...) {
  stopifnot(inherits(model, "model"))

  lines <- character()

  # Header
  if (!is.null(model$name)) {
    lines <- c(lines, paste0("* Model: ", model$name))
  }
  if (!is.null(model$desc)) {
    lines <- c(lines, paste0("* ", model$desc))
  }
  lines <- c(lines, "")

  # Sets
  for (s in model$sets) {
    lines <- c(lines, as_gams(s))
  }

  # Aliases
  if (!is.null(model$aliases) && length(model$aliases) > 0) {
    for (alias in model$aliases) {
      if (length(alias) >= 2) {
        lines <- c(lines, paste0("alias (", paste(alias, collapse = ", "), ");"))
      }
    }
  }

  lines <- c(lines, "")

  # Mappings
  for (m in model$mappings) {
    lines <- c(lines, as_gams(m))
  }
  lines <- c(lines, "")

  # Parameters
  for (p in model$parameters) {
    lines <- c(lines, as_gams(p))
  }
  lines <- c(lines, "")

  # Variables
  for (v in model$variables) {
    lines <- c(lines, as_gams(v))
  }
  lines <- c(lines, "")

  # Equations (declarations)
  for (eq in model$equations) {
    lines <- c(lines, paste0("equation ", eq$name, ";"))
  }
  lines <- c(lines, "")

  # Equations (definitions)
  for (eq in model$equations) {
    gams_eq <- as_gams(eq)
    if (format_expr) {
      gams_eq <- format_gams_expression(gams_eq, ...)
    }
    lines <- c(lines, gams_eq, "")
  }

  # Output
  if (!is.null(file)) {
    writeLines(lines, con = file, useBytes = TRUE)
  } else {
    return(lines)
  }
}


# remove_redundant_parens <- function(gams_code) {
#   lines <- unlist(strsplit(gams_code, "\n"))
#
#   open_sum_line <- grep("^\s*sum\\(", lines)
#   close_paren_line <- grep("\)\s*;\s*$", lines)
#
#   if (length(open_sum_line) > 0 && length(close_paren_line) > 0) {
#     first_line <- lines[open_sum_line[1]]
#     last_line <- lines[close_paren_line[length(close_paren_line)]]
#
#     # Remove outer parentheses: "sum((...))" -> "sum(...)"
#     lines[open_sum_line[1]] <- sub("sum\\(\\(", "sum(", first_line)
#     lines[close_paren_line[length(close_paren_line)]] <- sub("\\)\\);", ");", last_line)
#   }
#
#   paste(lines, collapse = "\n")
# }


format_gams_expression <- function(gams_lines, indent = 4) {
  `%+%` <- function(a, b) paste0(a, b)
  browser()

  extract_sum_parts <- function(part) {
    inside <- sub("^sum\\((.*)\\)$", "\\1", part)
    chars <- strsplit(inside, "")[[1]]
    depth <- 0; i <- 1
    while (i <= length(chars)) {
      ch <- chars[i]
      if (ch %in% c("(", "[")) depth <- depth + 1
      if (ch %in% c(")", "]")) depth <- depth - 1
      if (ch == "," && depth == 0) break
      i <- i + 1
    }
    list(
      index = trimws(paste0(chars[1:(i - 1)], collapse = "")),
      expr = trimws(paste0(chars[(i + 1):length(chars)], collapse = ""))
    )
  }

  format_sum_expression <- function(expr, current_indent) {
    parts <- extract_sum_parts(expr)
    out <- c()
    out <- c(out, strrep(" ", current_indent) %+% "sum(" %+% parts$index %+% ",")

    out <- c(out, split_recursive(parts$expr, current_indent + indent))
    out[length(out)] <- out[length(out)] %+% ")"
    out
  }

  split_recursive <- function(expr, current_indent) {
    browser()
    chars <- strsplit(expr, "")[[1]]
    depth <- 0; buffer <- ""; parts <- character(); i <- 1
    while (i <= length(chars)) {
      ch <- chars[i]
      if (ch %in% c("(", "[")) depth <- depth + 1
      if (ch %in% c(")", "]")) depth <- depth - 1
      if (depth == 0 && ch == "+") {
        parts <- c(parts, trimws(buffer)); buffer <- ""
      } else buffer <- paste0(buffer, ch)
      i <- i + 1
    }
    if (nzchar(trimws(buffer))) parts <- c(parts, trimws(buffer))

    result <- character()
    for (j in seq_along(parts)) {
      term <- trimws(parts[j])

      if (grepl("^sum\\(.*\\)$", term)) {
        nested <- format_sum_expression(term, current_indent + indent)
        nested[1] <- strrep(" ", current_indent) %+% "    " %+% trimws(nested[1])  # indent first line
        result <- c(result, nested)
      } else {
        # split on * and /
        chars <- strsplit(term, "")[[1]]
        depth <- 0; buffer <- ""; subparts <- character(); i <- 1
        while (i <= length(chars)) {
          ch <- chars[i]
          if (ch %in% c("(", "[")) depth <- depth + 1
          if (ch %in% c(")", "]")) depth <- depth - 1
          if (depth == 0 && chars[i] %in% c("*", "/")) {
            subparts <- c(subparts, trimws(buffer), chars[i])
            buffer <- ""
          } else buffer <- paste0(buffer, ch)
          i <- i + 1
        }
        if (nzchar(trimws(buffer))) subparts <- c(subparts, trimws(buffer))

        if (length(subparts)) {
          result <- c(result, strrep(" ", current_indent) %+% subparts[1])
        }
        if (length(subparts) > 1) {
          for (k in seq(2, length(subparts), by = 2)) {
            result <- c(result, strrep(" ", current_indent) %+% subparts[k] %+% " " %+% subparts[k + 1])
          }
        }
      }

      if (j < length(parts)) result <- c(result, strrep(" ", current_indent) %+% "+")
    }
    result
  }

  # --- Main logic ---
  if (is.character(gams_lines) && length(gams_lines) == 1) {
    gams_lines <- strsplit(gams_lines, "\n")[[1]]
  }
  stopifnot(length(gams_lines) >= 3)

  eq_header <- gams_lines[1]
  lhs_line  <- gams_lines[2]
  rhs_expr  <- sub(";$", "", gams_lines[3])

  formatted <- c(
    eq_header,
    strrep(" ", indent) %+% trimws(lhs_line),
    split_recursive(rhs_expr, indent),
    ");"
  )

  return(formatted)
}

