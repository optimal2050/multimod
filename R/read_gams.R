gams_ops <- c(
  "+", "-",
  "*", "/",
  "**", "^",
  # "<", ">", "=", "<=", ">=", "==", "!=", "<>",
  "<", "<=", "=", "<>", ">=", ">", "LT", "LE", "EQ", "NE", "GE", "GT",
  "and", "or", "not"
  # , "$" # handle separately
)

# Operator precedence:
#   ^ **
#   * /
#   + - binary and unary
# < <= = <> >= > LE LE EQ NE GE GT
# NOT
# AND
# OR XOR EQV IMP
# https://www.gams.com/49/docs/UG_DataEntry.html#UG_DataEntry_Parameters

split_top_level_dollar <- function(expr_str) {
  chars <- strsplit(expr_str, "")[[1]]
  level <- 0
  for (i in seq_along(chars)) {
    if (chars[i] == "(") {
      level <- level + 1
    } else if (chars[i] == ")") {
      level <- level - 1
    } else if (chars[i] == "$" && level == 0) {
      return(c(
        substring(expr_str, 1, i - 1),
        substring(expr_str, i + 1)
      ))
    }
  }
  return(NULL) # No top-level $
}

# Remove comments between $ontext and $offtext
remove_ontext_offtext <- function(lines) {
  # browser()
  result <- character(0)
  inside_comment <- FALSE

  for (line in lines) {
    trimmed <- trimws(tolower(line))
    if (grepl("^\\$ontext", trimmed)) {
      inside_comment <- TRUE
      next
    }
    if (grepl("^\\$offtext", trimmed)) {
      inside_comment <- FALSE
      next
    }
    if (!inside_comment) {
      result <- c(result, line)
    }
  }
  result
}

# include files
gams_expand_includes <- function(lines, base_path = ".") {
  # browser()
  expanded <- character()

  for (line in lines) {
    # cat(line, "\n")
    # line <- iconv(line, to = "UTF-8", sub = "byte")
    line <- iconv(line, from = "", to = "UTF-8", sub = "?")
    # trimmed <- stringi::stri_trim_both(line)
    trimmed <- trimws(line)

    if (grepl("^\\$include\\b", trimmed, ignore.case = TRUE)) {
      # Extract filename
      include_file <- sub("^\\$include\\s+", "", trimmed, ignore.case = TRUE)
      include_file <- gsub('"', "", include_file) # remove optional quotes
      include_path <- file.path(base_path, include_file)

      if (!file.exists(include_path)) {
        warning("Included file not found: ", include_path)
        next
      }

      # Read included file lines and append
      included_lines <- readLines(include_path, warn = FALSE)
      included_lines <- iconv(included_lines, from = "", to = "UTF-8", sub = "?")
      expanded <- c(expanded, included_lines)
    } else {
      expanded <- c(expanded, line)
    }
  }

  return(expanded)
}


normalize_gams_lines <- function(lines) {
  # browser()
  decl_patterns <- c(
    "^set(s)?\\b", "^alias(es)?\\b",
    "^parameter(s)?\\b", "^scalar(s)?\\b",
    "^table(s)?\\b",
    "^([a-zA-Z0-9_]+\\s+)?variable(s)?\\b", # any word-like symbol before variable(s)
    # "^variable(s)?\\b",
    # "^positive variable(s)?\\b",
    # "^negative variable(s)?\\b", "^binary variable(s)?\\b", "^integer variable(s)?\\b",
    "^equation(s)?\\b", "^model(s)?\\b"
  )
  decl_regex <- paste(decl_patterns, collapse = "|")

  result <- character()
  in_decl_block <- FALSE
  buffer <- NULL

  for (line in lines) {
    # if (grepl("alias", line, ignore.case = TRUE)) {
    #   browser()
    # }

    # Skip lines starting with $
    if (startsWith(line, "$")) {
      next
    }

    trimmed <- trimws(line)

    # Start of declaration block
    if (!in_decl_block && grepl(decl_regex, trimmed, ignore.case = TRUE)) {
      in_decl_block <- TRUE

      # capture the declaration keyword
      # browser()
      decl_keyword <- regmatches(
        trimmed,
        regexpr(decl_regex, trimmed, ignore.case = TRUE)
      )[[1]]
      result <- c(result, decl_keyword)

      # Remove the keyword from the line
      trimmed <- gsub(decl_regex, "", trimmed, ignore.case = TRUE) |> trimws()

      if (grepl(";", trimmed)) {
        result <- c(result, sub(";", "", trimmed), ";")
        in_decl_block <- FALSE
      } else {
        result <- c(result, trimmed)
      }
      next
    }

    # Still in declaration block
    if (in_decl_block) {
      result <- c(result, line)
      if (grepl(";", trimmed)) {
        in_decl_block <- FALSE
      }
      next
    }

    # Accumulate multi-line expression
    if (is.null(buffer)) {
      buffer <- trimmed
    } else {
      buffer <- paste(buffer, trimmed)
    }

    if (grepl(";", trimmed)) {
      stmts <- strsplit(buffer, ";", fixed = TRUE)[[1]]
      for (stmt in stmts) {
        stmt <- trimws(stmt)
        if (nzchar(stmt)) {
          result <- c(result, paste0(stmt, ";"))
        }
      }
      buffer <- NULL
    }
  }

  # Final flush
  if (!is.null(buffer)) {
    result <- c(result, trimws(buffer))
  }

  return(result)
}


strip_gams_data_block <- function(lines) {
  out <- character()
  in_decl <- FALSE
  in_block <- FALSE
  block_lines <- character()

  is_decl_start <- function(line) grepl("^\\s*(set|sets|parameter|parameters)\\b", line, ignore.case = TRUE)
  is_block_start <- function(line) grepl("^\\s*/\\s*$", line)
  is_block_end   <- function(line) grepl("^\\s*/\\s*[;,]?$", line)

  flush_block <- function(block) {
    # Replace all /.../ groups inside the block with a space
    text <- paste(block, collapse = "\n")
    gsub("(?s)/.*?/\\s*", " ", text, perl = TRUE)
  }

  for (line in lines) {
    if (!in_decl && is_decl_start(line)) {
      in_decl <- TRUE
      block_lines <- c(line)
      next
    }

    if (in_decl) {
      block_lines <- c(block_lines, line)

      # End of full declaration
      if (grepl("/\\s*;", line)) {
        cleaned <- flush_block(block_lines)
        out <- c(out, unlist(strsplit(cleaned, "\n", fixed = TRUE)))
        in_decl <- FALSE
        block_lines <- character()
      }

      next
    }

    # not in declaration, pass line as-is
    out <- c(out, line)
  }

  # Final fallback (unclosed blocks)
  if (length(block_lines)) {
    cleaned <- flush_block(block_lines)
    out <- c(out, unlist(strsplit(cleaned, "\n", fixed = TRUE)))
  }

  return(out)
}

# split_indexed_operator <- function(expr_str) {
#   # Remove leading aggregate name
#   expr_str <- sub("^(sum|prod)\\(", "", expr_str)
#   expr_str <- sub("\\)$", "", expr_str)
#
#   chars <- strsplit(expr_str, "")[[1]]
#   level <- 0
#   for (i in seq_along(chars)) {
#     ch <- chars[i]
#     if (ch == "(") {
#       level <- level + 1
#     } else if (ch == ")") {
#       level <- level - 1
#     } else if (ch == "," && level == 0) {
#       # browser()
#       index <- substring(expr_str, 1, i - 1)
#       value_expr <- substring(expr_str, i + 1)
#       return(c(trimws(index), trimws(value_expr)))
#     }
#   }
#   return(NULL) # malformed or no split point
# }
split_indexed_operator <- function(expr_str, func_name) {
  # Escape function name in case it contains special regex characters
  func_pattern <- paste0("^", func_name, "\\(")
  expr_str <- sub(func_pattern, "", expr_str)
  expr_str <- sub("\\)$", "", expr_str)

  chars <- strsplit(expr_str, "")[[1]]
  level <- 0
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "(") {
      level <- level + 1
    } else if (ch == ")") {
      level <- level - 1
    } else if (ch == "," && level == 0) {
      index <- substring(expr_str, 1, i - 1)
      value_expr <- substring(expr_str, i + 1)
      return(c(trimws(index), trimws(value_expr)))
    }
  }
  return(NULL) # malformed or no split point
}



# Known indexed GAMS functions
known_funcs_indexed <- c("sum", "prod", "smin", "smax", "sand", "sor")

parse_gams_expr <- function(
    expr,
    symbols = list(),
    known_funcs = c("log", "exp", "abs", "sqrt", "ord", "card"),
    # known_funcs_indexed = known_funcs_indexed,
    depth = 0, max_depth = 20,
    brackets = FALSE, # whether to wrap in brackets, passed to ast_*
    ...
    ) {
  # message(expr)
  # if (brackets) browser()
  # browser()
  # if (expr == "mvTechInp(tech,comm,region,year,slice)") browser()
  if (depth > max_depth) stop("Maximum expression nesting depth exceeded")

  expr <- trimws(expr)
  if (expr == "") {
    return(NULL)
  }

  # check if expr has relational operator
  if (grepl("=\\s*(E|L|G|LE|GE)\\s*=", gsub("\\s+", "", expr), ignore.case = TRUE)) {
    # !!! add LE GE
    stop(
      "A relational operator: =e=, =l=, =g= found in the expression.\n",
      "Use gams_to_multimod() to parse equations with relational operators."
    )
  }

  # Clean malformed operator sequences
  expr <- gsub("\\*\\s*\\+", "*", expr)
  expr <- gsub("\\+\\s*\\+", "+", expr)
  # expr <- gsub("\\*\\s*\\*", "*", expr)
  # expr <- gsub("^\\*", "", expr)
  expr <- gsub("\\s+", " ", expr)

  # math and logic ####
  # `*`, `/`
  # `and`, `or` `not`
  # ">=", "<=", ">", "<", "="
  # `+`, `-`
  # browser()
  top_ops <- find_top_level_operators(expr, descending = TRUE)$op |> unique()
  for (op in top_ops) {
    tokens <- split_top_level(expr, op, keep_op = FALSE, as_list = FALSE)
    if (!is.null(tokens) && length(tokens) >= 2) {
      lhs <- parse_gams_expr(tokens[[1]], symbols, known_funcs, depth + 1, max_depth)
      rhs <- parse_gams_expr(paste(tokens[-1], collapse = paste0(" ", op, " ")), symbols, known_funcs, depth + 1, max_depth)
      # return(list(type = "expression", op = tolower(op), lhs = lhs, rhs = rhs))
      return(ast_expression(op, lhs = lhs, rhs = rhs, brackets = brackets))
    } else if (length(tokens) == 1) {
      # Handle unary operators
      if (op %in% c("not", "-", "!")) {
        rhs <- parse_gams_expr(tokens[[1]], symbols, known_funcs,
                               depth + 1, max_depth)
        return(ast_unary(op, rhs))
      } else if (op %in% "+") {
        # ignore unary plus
        return(parse_gams_expr(tokens[[1]], symbols, known_funcs,
                               depth, max_depth))
      } else {
        stop("Unrecognized unary operator: ", op, "\n",
             "Cannot parse expression: ", expr)
      }
    }
  }

  # Has top-level operator(s)
  top_dollar <- find_top_level_operators(expr, ops = "$", precedence = NULL)
  # top-level $ condition ####
  dollar_parts <- split_top_level_dollar(expr)
  if (!is.null(dollar_parts)) {
    return(ast_when(
      condition = parse_gams_expr(dollar_parts[2], symbols, known_funcs),
      then = parse_gams_expr(dollar_parts[1], symbols, known_funcs)
    ))
  }

  # parentheses ####
  # !!! any other cases were parentheses should not be removed?
  if (startsWith(expr, "(") && endsWith(expr, ")")) {
    # brackets <- TRUE
    return(parse_gams_expr(
      substr(expr, 2, nchar(expr) - 1),
      symbols, known_funcs,
      depth + 1, max_depth,
      brackets = TRUE
    ))
  }

  # known functions ####
  # browser()
  if (grepl("^[a-zA-Z0-9_]+\\(", expr)) {
    result <- parse_function_expr(expr,
                                  symbols = symbols,
                                  known_funcs = known_funcs,
                                  known_funcs_indexed = known_funcs_indexed,
                                  depth = depth + 1,
                                  max_depth = max_depth)
    # browser()
    if (!is.null(result)) return(result)
  }

  # sum(...), prod(...)
  if (grepl("^(sum|prod)\\(", expr)) {
    browser() # should not be here!
    agg_type <- if (grepl("^sum\\(", expr)) "sum" else "prod"
    parts <- split_indexed_operator(expr)
    if (is.null(parts) || length(parts) != 2) stop("Malformed aggregate: cannot split arguments")
    index <- parse_gams_expr(parts[1], symbols, known_funcs)
    # browser()
    if (!inherits(index, "ast")) {
      # assume index is a dimension
      index <- ast_dims(index) # ensure index is treated as a dimension
    }
    value_expr <- parse_gams_expr(trimws(parts[2]), symbols, known_funcs)

    parsed_expr <- switch(
      agg_type,
      "sum" = ast_sum(index, value_expr),
      "prod" = ast_prod(index, value_expr)
      # "sum" = ast_sum(index, domain, value_expr),
      # "prod" = ast_prod(index, domain, value_expr)
    )

    return(parsed_expr)
  }

  # Handle dot-access like y.val as val(y)
  if (grepl("^[a-zA-Z0-9_]+\\.[a-zA-Z0-9_]+$", expr)) {
    # browser()
    parts <- strsplit("y.val", "\\.", fixed = FALSE)[[1]]
    object <- parts[1]
    method <- parts[2]
    object <- parse_gams_expr(object, symbols = symbols)
    return(ast_func(method, value = object))
  }

  # dims and function-like: name(dims) ####
  # use "dims" name for both: dimension and arguments
  if (grepl("^[a-zA-Z0-9_]+\\([^()]+\\)$", expr)) {
    name <- sub("\\(.*", "", expr)
    dims <- gsub("[()]", "", sub("^[^(]+\\(", "", expr))
    dims <- trimws(strsplit(dims, ",")[[1]])
    symbol_type <- detect_symbol_type(name, symbols, known_funcs)
    # browser()
    dims <- ast_dims(dims, symbols = symbols)
    return(do.call(paste0("ast_", symbol_type), list(name, dims)))
  }

  # Sequence of symbols
  max_lev <- get_expression_level(expr, as_data_frame = FALSE) |> max()
  if (max_lev == 0) {
    sq <- find_top_level_operators(expr, ",", precedence = NULL)
    if (nrow(sq) > 0) {
      symb <- split_top_level(expr, ",", keep_op = FALSE, as_list = FALSE)
    } else {
      # check if expr is a word-like symbol
      if (grepl("^[a-zA-Z][a-zA-Z0-9_]*$", expr)) {
        symb <- expr
      } else if (grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", expr)) {
        # numeric constant
        return(ast_constant(as.numeric(expr)))
      } else {
        stop("Unrecognized expression: ", expr)
      }
    }
    # assign
    # browser()
    symb <- symb |>
      lapply(function(x) {
        sb <- detect_symbol_type(x, symbols = symbols, known_funcs = known_funcs)
        do.call(paste0("ast_", sb), list(x))
      })
    # return(ast_dims(symb))
    if (length(symb) == 1) {
      symb <- symb[[1]] # return single symbol directly
    } else {
      symb <- ast_dims(symb) # wrap in dims if multiple symbols
    }
    return(symb)
  }

  if (max_lev > 0) {
    browser()
    stop("Expression has unrecognized operators or structure: ", expr)
  }

  return(ast_symbol(expr))
}

read_gams <- function(
    file_or_text,
    include = TRUE,
    interim_file = NULL,
    strict = TRUE,
    verbose = FALSE,
    ...) {
  # browser()
  if (file.exists(file_or_text)) {
    lines <- readLines(file_or_text, encoding = "UTF-8")
  } else {
    lines <- unlist(strsplit(file_or_text, "\n"))
  }
  # replace non-UTF-8 characters with '?' if any
  lines <- iconv(lines, from = "", to = "UTF-8", sub = "?")

  if (include) {
    lines <- gams_expand_includes(lines, base_path = dirname(file_or_text))
  } else {
    lines <- lines[!grepl("^\\$include\\b", lines, ignore.case = TRUE)]
  }

  # Remove commented lines
  lines <- lines[!grepl("^\\*", lines)]
  lines <- remove_ontext_offtext(lines)

  # remove data
  # strip_gams_data_block <- function(lines) {
  #   text <- paste(lines, collapse = "\n")
  #   # (?s) enables "dotall" so . matches newlines
  #   cleaned <- gsub("(?s)/.*?/\\s*", " ", text, perl = TRUE)
  #   strsplit(cleaned, "\n", fixed = TRUE)[[1]]
  # }

  # lines <- strip_gams_data_block(lines)
  lines <- normalize_gams_lines(lines)
  # writeLines(lines, "tmp/no_comments.gms")

  # Concatenate lines between semicolons
  # lines <- gsub("\\s*;\\s*", ";", lines) # remove spaces around semicolon

  lines <- gsub(";+", ";", lines) # normalize semicolon
  # semicolon to individual lines
  lines <- split_line_on_pattern(lines, ";", position = "both", ignore.case = TRUE)
  # add line-break after equation's `..`
  lines <- split_line_on_pattern(lines, "\\.\\.", position = "after", ignore.case = TRUE)
  # equation's `=E=`, `=L=`, `=G=`, ... to individual lines
  lines <- split_line_on_pattern(
    lines,
    c("=E=", "=L=", "=G=", "=LE=", "=GE="),
    position = "both", ignore.case = TRUE
  )
  # remove empty and white-space-only lines
  lines <- lines[!grepl("^\\s*$", lines)]
  # remove lines with more than one line break
  lines <- lines[!grepl("^\n+$", lines)]
  # remove empty and white-space-only lines
  lines <- lines[!grepl("^\\s*$", lines)]
  # trim excess spaces
  lines <- gsub("\\s+", " ", lines)
  # normalize brackets
  lines <- gsub("\\{", "(", lines)
  lines <- gsub("\\}", ")", lines)
  lines <- gsub("\\[", "(", lines)
  lines <- gsub("\\]", ")", lines)
  # around comas (inside brackets)
  lines <- gsub("\\s*,\\s+", ",", lines)

  if (!is.null(interim_file)) {
    writeLines(lines, interim_file)
  }
  # writeLines(lines, "tmp/no_comments.gms")
  # browser()

  # Initialize
  sets <- list()
  mappings <- list()
  aliases <- list()
  parameters <- list()
  variables <- list()
  equations <- list()

  mode <- NULL
  i <- 1

  while (i <= length(lines)) {
    line <- trimws(lines[i])

    # Skip blank or *-comment lines
    if (line == "" || grepl("^\\*", line)) {
      i <- i + 1
      if (verbose) message(line)
      next
    }

    if (verbose) {
      # cat("Processing line", i, "of", length(lines), "\n")
      cat(mode, "| ")
      # Sys.sleep(0.25)
    }

    ## Comment detection (if not removed above)
    if (isTRUE(mode == "comment")) {
      # browser()
      if (grepl("^\\$offtext", line, ignore.case = TRUE)) {
        mode <- NULL
        # i <- i + 1
        # if (verbose) message("End of comment block\n")
        if (verbose) message(line)
      }
      i <- i + 1
      next
    } else if (grepl("^\\$ontext", line, ignore.case = TRUE)) {
      mode <- "comment"
      i <- i + 1
      # if (verbose) message("Detected comment block\n")
      if (verbose) message(line, " -> ", mode)
      next
    } else if (grepl("^\\$offtext", line, ignore.case = TRUE)) {
      stop("Unmatched $offtext found without a corresponding $ontext.")
      # mode <- NULL
      # i <- i + 1
      # if (verbose) message("End of comment block\n")
      # next
    }

    # End of block detection
    if (grepl(";", line)) {
      # browser()
      stopifnot(nchar(line) == 1)
      mode <- NULL
      if (verbose) message(line)
      i <- i + 1
      next
    }

    # Search for declaration blocks

    if (grepl("^set(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "sets"
      # check if there is a code after set(s)
      the_rest <- sub("^set(s)?\\b\\s*", "", line, ignore.case = TRUE)
      if (nchar(the_rest) > 0) {
        browser()
        stop(
          "Unrecognized code after set(s) declaration: '",
          the_rest, "'\n",
          "Expected a new line. Check the parser logic or the GAMS file.",
        )
      }
      i <- i + 1
      next
    } else if (grepl("^alias(es)?\\b", line, ignore.case = TRUE)) {
      # browser()
      mode <- "aliases"
      if (verbose) message(line, " -> ", mode)
      # check if there is a code after alias(es)
      the_rest <- sub("^alias(es)?\\b\\s*", "", line, ignore.case = TRUE)
      if (nchar(the_rest) > 0) {
        browser()
        stop(
          "Unrecognized code after alias(es) declaration: '",
          the_rest, "'\n",
          "Expected a new line. Check the parser logic or the GAMS file.",
        )
      }
      i <- i + 1
      next
    } else if (grepl("^parameter(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "parameters"
      if (verbose) message(line, " -> ", mode)
      # check if there is a code after parameter(s)
      the_rest <- sub("^parameter(s)?\\b\\s*", "", line, ignore.case = TRUE)
      if (nchar(the_rest) > 0) {
        browser()
        stop(
          "Unrecognized code after parameter(s) declaration: '",
          the_rest, "'\n",
          "Expected a new line. Check the parser logic or the GAMS file.",
        )
      }
      i <- i + 1
      next
    # } else if (grepl("^((positive|free|integer)\\s+)?variable(s)?\\b", line, ignore.case = TRUE)) {
    } else if (grepl("^([a-zA-Z0-9_]+\\s+)?variable(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "variables"
      # extract variable type
      match_type <- regexec("^([a-zA-Z0-9_]+)\\s+variable(s)?\\b",
                       line, ignore.case = TRUE)
      # browser()
      var_type <- regmatches(line, match_type)[[1]][2]
      if (is.na(var_type)) var_type <- NULL

      if (verbose) message(line, " -> ", mode)
      i <- i + 1
      # check if there is a code after variable(s)
      the_rest <- sub("^([a-zA-Z0-9_]+\\s+)?variable(s)?\\b\\s*", "", line, ignore.case = TRUE)
      if (nchar(the_rest) > 0) {
        browser()
        stop(
          "Unrecognized code after variable(s) declaration: '",
          the_rest, "'\n",
          "Expected a new line. Check the parser logic or the GAMS file.",
        )
      }

      next
    } else if (grepl("^equation(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "equations"
      if (verbose) message(line, " -> ", mode)
      # check if there is a code after equation(s)
      the_rest <- sub("^equation(s)?\\b\\s*", "", line, ignore.case = TRUE)
      if (nchar(the_rest) > 0) {
        browser()
        stop(
          "Unrecognized code after equation(s) declaration: '",
          the_rest, "'\n",
          "Expected a new line. Check the parser logic or the GAMS file.",
        )
      }
      i <- i + 1
      next
    }

    if (verbose) cat(i, line, "\n")

    ## Mode-based parsing ####
    if (!is.null(mode) && mode == "sets") {
      # drop data - everything after the first "/"
      line <- sub("/.*$", "", line)
      # name and desc
      parts <- strsplit(line, "\\s+")[[1]]
      name_part <- parts[[1]][1]
      if (length(parts) > 1) {
        desc <- paste(parts[-1], collapse = " ") |> trimws()
      } else {
        desc <- character(0)
      }
      # browser()
      if (grepl("\\(", name_part)) {
        name <- sub("\\(.*", "", line)
        inside <- sub("^[^(]+\\(([^)]+)\\).*", "\\1", line)
        dims <- trimws(unlist(strsplit(inside, ",")))
        mappings[[name]] <- list(name = name, desc = desc, dims = dims)
      } else {
        name <- name_part
        sets[[name]] <- list(name = name, desc = desc, dims = character(0))
      }
      i <- i + 1
      next
    }

    if (!is.null(mode) && mode == "aliases") {
      # browser()
      matches <- gregexpr("\\([^()]+\\)", line)[[1]]
      if (matches[1] != -1) {
        for (start in matches) {
          end <- start + attr(matches, "match.length")[which(matches == start)] - 1
          pair_str <- substr(line, start, end)
          pair_str <- gsub("[()]", "", pair_str)
          vars <- trimws(unlist(strsplit(pair_str, ",")))
          if (length(vars) >= 2) {
            aliases[[length(aliases) + 1]] <- vars
          }
        }
      }
      i <- i + 1
      next
    }

    if (!is.null(mode) && mode %in% c("parameters", "variables")) {
      # browser()
      line <- sub("^\\*+@", "", line)
      line <- trimws(line)
      if (line == "") {
        i <- i + 1
        next
      }

      close_paren_pos <- regexpr("\\)", line)
      if (close_paren_pos > 0) {
        sym_def <- substr(line, 1, close_paren_pos)
        rest_of_line <- substr(line, close_paren_pos + 1, nchar(line))
      } else {
        sym_def <- strsplit(line, "\\s+")[[1]][1]
        rest_of_line <- substring(line, nchar(sym_def) + 1)
      }

      sym_name <- sub("\\(.*", "", sym_def)
      dims_raw <- sub("^[^(]+\\(([^)]*)\\)", "\\1", sym_def)
      dims <- if (dims_raw != sym_def) trimws(unlist(strsplit(dims_raw, ","))) else character(0)
      desc <- trimws(rest_of_line)

      symbol_obj <- list(name = sym_name, dims = dims, desc = desc)

      if (mode == "parameters") {
        parameters[[sym_name]] <- symbol_obj
      } else if (mode == "variables") {
        # browser()
        symbol_obj$type <- var_type
        variables[[sym_name]] <- symbol_obj
      }
      i <- i + 1
      next
    }

    # Don't treat lines with $.. as declarations — they're equation bodies
    if (!is.null(mode) && mode == "equations" && !grepl("\\$.*\\.\\.", line)) {
      # browser()
      # match <- regexec("^([a-zA-Z0-9_]+)\\(([^)]*)\\)\\s*(.*)$", line)
      match <- regexec("^([a-zA-Z0-9_]+)(?:\\(([^)]*)\\))?\\s*(.*)$", line)
      result <- regmatches(line, match)[[1]]

      if (length(result) >= 4) {
        eq_name <- result[2]
        dims <- trimws(unlist(strsplit(result[3], ",")))
        desc <- trimws(result[4])

        equations[[eq_name]] <- list(
          name = eq_name,
          dims = dims,
          desc = desc,
          domain = NULL,
          gams = NULL
        )
      }
      i <- i + 1
      next
    }

    # browser()
    ## Outside declarations: detect equation body ####
    eq_header <- parse_equation_header(line)
    if (!is.null(eq_header) &&
        grepl("S7_StorageLevelYearFinish", eq_header$name, ignore.case = TRUE)) {
      browser() # debug
    }
    if (!is.null(eq_header)) {
      eq_name <- eq_header$name
      body_accum <- character(0)
      i <- i + 1

      while (i <= length(lines) && !grepl(";", lines[i])) {
        body_accum <- c(body_accum, trimws(lines[i]))
        i <- i + 1
      }
      if (verbose) {
        if (grepl(".;", lines[i])) {
          browser() # debug - should not be here
          message("Found semicolon at end of equation body")
        }
      }
      if (i <= length(lines)) {
        body_accum <- c(body_accum, trimws(gsub(";", "", lines[i])))
        i <- i + 1
      }

      if (!is.null(equations[[eq_name]])) {
        equations[[eq_name]]$domain <- eq_header$condition
        equations[[eq_name]]$gams <- paste(body_accum, collapse = " ") |> trimws()
      } else {
        if (strict) {
          warning(paste("Equation body found for undeclared equation:", eq_name))
        } else {
          equations[[eq_name]] <- list(
            name = eq_header$name,
            dims = eq_header$dims,
            desc = "",
            domain = eq_header$condition,
            gams = paste(body_accum, collapse = " ") |> trimws()
          )
        }
      }
      eq_name <- NULL; body_accum <- NULL
      next
    }

    i <- i + 1
  }

  new_model_structure(
    sets = sets,
    mappings = mappings,
    aliases = aliases,
    parameters = parameters,
    variables = variables,
    equations = equations,
    source = file_or_text,
    language = "GAMS"
  )
}

parse_equation_header <- function(line) {
  # browser()
  pattern_step1 <- "^\\s*(.*?)\\s*\\.\\.\\s*$"
  # if (!grepl(pattern_step1, line)) stop("Invalid GAMS equation header")
  if (!grepl(pattern_step1, line)) return(NULL) # not an equation header

  header <- sub(pattern_step1, "\\1", line)

  header_parts <- split_top_level_dollar(header)

  if (!is.null(header_parts) && length(header_parts) == 2) {
    # browser()
    name_and_dims <- header_parts[[1]]
    condition = header_parts[[2]]
    # condition <- parse_gams_expr(hparts[[2]], symbols = symbols)
    # if (is.null(condition)) condition <- NULL
  } else if (is.null(header_parts)) {
    name_and_dims <- header
    condition     <- NULL
  } else {
    browser()
    stop("Unrecognized equation header format: ", line)
  }

  # if (grepl("\\$.", header)) {
  #   # pattern_step2 <- "^(.*?)\\$\\((.*)\\)$"
  #   # pattern_step2 <- "^([^(\\$]+\\([^)]*\\))\\$[({]([^)}]+)[)}]$"
  #   # pattern_step2 <- "^([^$]+)\\$\\(?\\{?([^)}]+)\\)?\\}?$"
  #   pattern_step2 <- "^([a-zA-Z0-9_]+(?:\\([^)]*\\))?)\\s*(?:\\$\\(?([^\\)]*\\)?[^)]*)\\)?)?$"
  #
  #   name_and_dims <- sub(pattern_step2, "\\1", header)
  #   condition     <- sub(pattern_step2, "\\2", header)
  # } else {
  #   name_and_dims <- header
  #   condition     <- NULL
  # }

  name_dims_pattern <- "^([a-zA-Z0-9_]+)\\(([^)]*)\\)$"
  if (grepl(name_dims_pattern, name_and_dims)) {
    eq_name <- sub(name_dims_pattern, "\\1", name_and_dims)
    dims_raw <- sub(name_dims_pattern, "\\2", name_and_dims)
    dims <- strsplit(dims_raw, ",\\s*")[[1]]
  } else {
    eq_name <- name_and_dims
    dims <- character(0)
  }

  list(
    name = eq_name,
    dims = dims,
    condition = condition
  )
}

parse_gams_equation <- function(eqn_info, symbols) {
  # browser()
  name <- trimws(eqn_info$name)
  lhs_rhs <- trimws(eqn_info$gams)
  # domain_str <- ""

  # ⛑ Skip if body is missing
  if (is.null(lhs_rhs) || !nzchar(lhs_rhs)) {
    warning(sprintf("Skipping equation %s: no GAMS body provided", name))
    return(NULL)
  }

  # if (!is.null(eqn_info$domain) && nzchar(eqn_info$domain)) {
  #   domain_str <- paste0("$", eqn_info$domain)
  # }

  # Type	Description
  # =e=	Equality: right-hand side must equal left-hand side.
  # =g=	Greater than: left-hand side must be greater than or equal to right-hand side.
  # =l=	Less than: left-hand side must be less than or equal to right-hand side.
  # =n=	No relationship implied between left-hand side and right-hand side. This equation type is ideally suited for use in MCP models and in variational inequalities.
  # =x=	Equation is defined by external programs. See External Equations.
  # =c=	Conic constraint. See Conic Programming in GAMS.
  # =b=	Boolean equations. See Logic Equations.
  # https://www.gams.com/49/docs/UG_Equations.html

  # Identify relational operator and split
  if (grepl("=[lL]=", lhs_rhs)) {
    relation <- "<="
    parts <- strsplit(lhs_rhs, "=[lL]=")[[1]]
  } else if (grepl("=[gG]=", lhs_rhs)) {
    relation <- ">="
    parts <- strsplit(lhs_rhs, "=[gG]=")[[1]]
  } else if (grepl("=[eE]=", lhs_rhs)) {
    relation <- "=="
    parts <- strsplit(lhs_rhs, "=[eE]=")[[1]]
  } else {
    stop("Unsupported or missing relational operator (=e=, =l=, =g=).")
  }

  if (length(parts) != 2) {
    stop("Equation body does not split into valid LHS and RHS.")
  }

  lhs_str <- trimws(parts[1])
  rhs_str <- trimws(parts[2])

  lhs <- parse_gams_expr(lhs_str, symbols)
  rhs <- parse_gams_expr(rhs_str, symbols)
  # browser()
  domain <- if (!is.null(eqn_info$domain) && nzchar(eqn_info$domain)) {
    # Parse domain condition
    domain <- parse_gams_expr(eqn_info$domain, symbols)
  } else {
    domain <- NULL
  }

  if (!is_empty(eqn_info$dims)) {
    eq_dims <- ast_dims(eqn_info$dims)
  } else {
    eq_dims <- NULL
  }
  # Return the equation object
  new_equation(
    name = eqn_info$name,
    dims = eq_dims,
    lhs = lhs,
    rhs = rhs,
    relation = relation,
    domain = domain,
    desc = eqn_info$desc
  )
}
