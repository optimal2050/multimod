
#' Write a GAMS model file from a multimod object
#'
#' @param model A `multimod` model object
#' @param file Output file path (optional)
#' @param format_expr logical; whether to format expressions with line breaks
#' @param ... Additional arguments passed to formatting functions
#'
#' @return Character vector or writes file if `file` is given
#' @export
write_gams <- function(model, file = NULL, format_expr = TRUE, ...) {
  UseMethod("write_gams")
}


#' Write a full GAMS model file from a multimod object
#'
#' @param model A `multimod` model object
#' @param file Output file path (optional)
#' @param format_expr logical; whether to format expressions with line breaks
#' @param gdx Name of a GDX to `$load` the data from, as written by
#'   [write_gams_data()]. `NULL` (default) emits declarations and equations
#'   only. NEOS decodes its input GDX to `in.gdx`, so that is the useful
#'   name for a NEOS job.
#' @param include_solve Logical; append the `model` / `solve` statements.
#'   Needed for a runnable job, and harmless otherwise.
#' @param objective Name of the variable to minimise.
#' @param ... Additional arguments passed to formatting functions
#'
#' @return Character vector or writes file if `file` is given
#' @method write_gams model
#' @export
write_gams.model <- function(model, file = NULL, format_expr = TRUE,
                             gdx = NULL, include_solve = FALSE,
                             objective = "vObjective", ...) {
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
  subset_symbols <- dual_registered_symbols(model)
  lines <- c(lines, "sets")
  for (s in model$sets) {
    # The subset form is declared here, ahead of the alias statements that
    # name these sets. The declaration comes from the mapping, which carries
    # the parent, so `sup` is emitted as `sup(process)` once.
    if (!is.null(s$name) && s$name %in% subset_symbols) {
      lines <- c(lines, as_gams(model$mappings[[s$name]], desc = TRUE))
      next
    }
    lines <- c(lines, as_gams(s, desc = TRUE))
  }
  lines <- c(lines, ";")

  # Aliases
  if (!is.null(model$aliases) && length(model$aliases) > 0) {
    for (alias in model$aliases) {
      if (length(alias) >= 2) {
        lines <- c(lines, paste0("alias (", paste(alias, collapse = ", "), ");"))
      }
    }
  }

  lines <- c(lines, "")
  lines <- c(lines, "sets")
  # Mappings
  for (m in model$mappings) {
    # Already declared with the sets above, ahead of the aliases.
    if (!is.null(m$name) && m$name %in% subset_symbols) next
    lines <- c(lines, as_gams(m, desc = TRUE))
  }
  lines <- c(lines, ";")

  lines <- c(lines, "")
  lines <- c(lines, "parameters")
  # Parameters
  for (p in model$parameters) {
    lines <- c(lines, as_gams(p, desc = TRUE))
  }
  lines <- c(lines, ";")

  lines <- c(lines, "")
  # A bare `variable` is free in GAMS; a non-negative one is `positive`.
  vars <- model$variables %||% list()
  is_pos <- vapply(vars, function(v) {
    t <- v$vtype
    !is.null(t) && length(t) == 1L && !is.na(t) && identical(tolower(t), "positive")
  }, logical(1))

  if (any(!is_pos)) {
    lines <- c(lines, "variables")
    for (v in vars[!is_pos]) lines <- c(lines, as_gams(v, desc = TRUE))
    lines <- c(lines, ";")
  }
  if (any(is_pos)) {
    lines <- c(lines, "positive variables")
    for (v in vars[is_pos]) lines <- c(lines, as_gams(v, desc = TRUE))
    lines <- c(lines, ";")
  }
  lines <- c(lines, "")

  # Equations (declarations)
  # for (eq in model$equations) {
  #   lines <- c(lines, paste0("equation ", eq$name, ";"))
  # }
  # lines <- c(lines, "")

  # Equations (definitions)
  for (eq in model$equations) {
    lines <- c(lines, paste0("equation ", eq$name, "   ", eq$desc, ";"))
    gams_eq <- as_gams(eq)
    if (format_expr) {
      gams_eq <- format_gams_expression(gams_eq, ...)
    }
    lines <- c(lines, gams_eq, "")
  }

  # Data. Values travel in a GDX, loaded at compile time. Base sets precede
  # their subsets for domain checking, and every declared symbol is loaded,
  # empty ones included.
  if (!is.null(gdx)) {
    set_syms <- names(model$sets %||% list())
    map_syms <- setdiff(names(model$mappings %||% list()), set_syms)
    par_syms <- names(model$parameters %||% list())

    lines <- c(lines, "* Data", "$onEmpty", paste0("$gdxin ", gdx))
    for (nm in c(set_syms, map_syms, par_syms)) {
      lines <- c(lines, paste0("$load ", nm))
    }
    lines <- c(lines, "$gdxin", "$offEmpty", "")

    # GAMS has no per-parameter default, so one is assigned after loading.
    # A stored zero is written as EPS, which survives the fill; the second
    # pass restores it to zero.
    defaulted <- character(0)
    for (nm in par_syms) {
      p <- model$parameters[[nm]]
      dv <- p$defVal
      if (is.null(dv) || !is.numeric(dv) || length(dv) != 1 || dv == 0) next
      dims <- get_dim_names(p$dims)
      if (length(dims) == 0) next
      idx <- paste0("(", paste(dims, collapse = ","), ")")
      lines <- c(lines, paste0(nm, idx, "$(not ", nm, idx, ") = ",
                               format(dv, scientific = FALSE), ";"))
      defaulted <- c(defaulted, nm)
    }
    # Every parameter that stores a zero is restored, not only the defaulted
    # ones: an EPS left in a coefficient is a non-zero where zero was meant.
    # Parameters with no stored zero carry no EPS and are skipped, which
    # keeps this from adding a dense pass per parameter.
    stores_zero <- function(p) {
      for (d in list(p$data, p$folded_data)) {
        if (!is.null(d) && nrow(d) > 0 && "value" %in% names(d) &&
            any(d$value == 0, na.rm = TRUE)) return(TRUE)
      }
      FALSE
    }
    restored <- 0L
    for (nm in par_syms) {
      p <- model$parameters[[nm]]
      dims <- get_dim_names(p$dims)
      if (length(dims) == 0 || !stores_zero(p)) next
      idx <- paste0("(", paste(dims, collapse = ","), ")")
      lines <- c(lines, paste0(nm, idx, "$(", nm, idx, " = EPS) = 0;"))
      restored <- restored + 1L
    }
    if (length(defaulted) > 0 || restored > 0) lines <- c(lines, "")
  }

  if (isTRUE(include_solve)) {
    mdl <- model$name %||% "mm_model"
    mdl <- gsub("[^A-Za-z0-9_]", "_", mdl)
    lines <- c(lines,
               paste0("model ", mdl, " / all / ;"),
               paste0("solve ", mdl, " minimizing ", objective, " using LP;"),
               paste0("display ", objective, ".l;"),
               "")
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
    ";"
  )

  return(formatted)
}

