gams_ops <- c(
  "+", "-", "*", "/", "**",
  "<", ">", "=", "<=", ">=", "==", "!=", "<>",
  "and", "or", "not"
  # , "$" # handle separately
)

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


split_indexed_operator <- function(expr_str) {
  # Remove leading aggregate name
  expr_str <- sub("^(sum|prod)\\(", "", expr_str)
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
      # browser()
      index <- substring(expr_str, 1, i - 1)
      value_expr <- substring(expr_str, i + 1)
      return(c(trimws(index), trimws(value_expr)))
    }
  }
  return(NULL) # malformed or no split point
}

parse_gams_expr <- function(
    expr,
    symbols = list(),
    known_funcs = c("log", "exp", "abs", "sqrt"),
    depth = 0, max_depth = 20,
    brackets = FALSE # whether to wrap in brackets, passed to ast_*
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
  top_ops <- top_level_operators(expr, descending = TRUE)$op |> unique()
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
  top_dollar <- top_level_operators(expr, ops = "$", precedence = NULL)
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

  # sum(...), prod(...) ####
  if (grepl("^(sum|prod)\\(", expr)) {
    # browser()
    agg_type <- if (grepl("^sum\\(", expr)) "sum" else "prod"
    parts <- split_indexed_operator(expr)
    if (is.null(parts) || length(parts) != 2) stop("Malformed aggregate: cannot split arguments")
    index <- parse_gams_expr(parts[1], symbols, known_funcs)
    # index_domain <- split_top_level_dollar(parts[1]) |> trimws()
    # index <- parse_gams_expr(index_domain[1], symbols = symbols)
    # domain <- if (length(index_domain) > 1) {
    #   ast_when(
    #     condition = parse_gams_expr(index_domain[2], symbols),
    #     then = NULL
    #   )
    # } else {
    #   NULL
    # }
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

  # dims and function-like: name(dims) ####
  # use "dims" name for both: dimension and arguments
  if (grepl("^[a-zA-Z0-9_]+\\([^()]+\\)$", expr)) {
    name <- sub("\\(.*", "", expr)
    dims <- gsub("[()]", "", sub("^[^(]+\\(", "", expr))
    dims <- trimws(strsplit(dims, ",")[[1]])
    symbol_type <- detect_symbol_type(name, symbols, known_funcs)
    # browser()
    dims <- ast_dims(dims)
    return(do.call(paste0("ast_", symbol_type), list(name, dims)))
  }

  # Sequence of symbols
  max_lev <- expression_level(expr, as_data_frame = FALSE) |> max()
  if (max_lev == 0) {
    sq <- top_level_operators(expr, ",", precedence = NULL)
    if (nrow(sq) > 0) {
      symb <- split_top_level(expr, ",", keep_op = FALSE, as_list = FALSE)
    } else {
      # check if expr is a word-like symbol
      if (grepl("^[a-zA-Z][a-zA-Z0-9_]*$", expr)) {
        symb <- list(expr)
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
    return(ast_dims(symb))
  }

  if (max_lev > 0) {
    browser()
    stop("Expression has unrecognized operators or structure: ", expr)
  }

  return(ast_symbol(expr))
}

gams_to_multimod <- function(gams_eq, symbols) {
  # browser()
  # Collapse and normalize
  gams_eq <- gsub("[\r\n]", " ", gams_eq)
  gams_eq <- gsub("\\s+", " ", gams_eq)
  gams_eq <- gsub("\\(\\s+", "(", gams_eq)
  gams_eq <- gsub("\\s+\\)", ")", gams_eq)
  gams_eq <- gsub("\\s*,\\s*", ",", gams_eq)
  gams_eq <- trimws(gams_eq)

  # Updated tolerant regex
  pattern_with_domain <- "^([a-zA-Z0-9_]+)\\(([^)]*)\\)\\s*\\$\\s*([a-zA-Z0-9_]+)\\(([^)]*)\\)\\s*\\.\\."
  pattern_no_domain <- "^([a-zA-Z0-9_]+)\\(([^)]*)\\)\\s*\\.\\."

  match <- regmatches(gams_eq, regexec(pattern_with_domain, gams_eq))[[1]]

  if (length(match) >= 5 && !any(is.na(match))) {
    eq_name <- match[2]
    indices <- trimws(strsplit(match[3], ",")[[1]])
    domain <- new_ast(
      node_type = detect_symbol_type(match[4], symbols),
      name = match[4],
      dims = ast_dims(trimws(strsplit(match[5], ",")[[1]]))
    )
    body <- gsub(pattern_with_domain, "", gams_eq)
  } else {
    match2 <- regmatches(gams_eq, regexec(pattern_no_domain, gams_eq))[[1]]
    if (length(match2) >= 3 && !any(is.na(match2))) {
      eq_name <- match2[2]
      indices <- trimws(strsplit(match2[3], ",")[[1]])
      domain <- NULL
      body <- gsub(pattern_no_domain, "", gams_eq)
    } else {
      stop("Equation format not recognized: could not extract name, indices, and domain.")
    }
  }

  # Clean trailing semicolon
  body <- trimws(gsub(";$", "", body))

  # Identify relational operator and split
  if (grepl("=[lL]=", body)) {
    relation <- "<="
    parts <- strsplit(body, "=[lL]=")[[1]]
  } else if (grepl("=[gG]=", body)) {
    relation <- ">="
    parts <- strsplit(body, "=[gG]=")[[1]]
  } else if (grepl("=[eE]=", body)) {
    relation <- "=="
    parts <- strsplit(body, "=[eE]=")[[1]]
  } else {
    stop("Unsupported or missing relational operator (=e=, =l=, =g=).")
  }

  if (length(parts) != 2) {
    stop("Equation body does not split into valid LHS and RHS.")
  }

  # Parse both sides using parse_gams_expr()
  lhs_str <- trimws(parts[1])
  rhs_str <- trimws(parts[2])

  lhs <- parse_gams_expr(lhs_str, symbols)
  rhs <- parse_gams_expr(rhs_str, symbols)

  # Return the equation object
  new_equation(
    name = eq_name,
    dims = indices,
    lhs = lhs,
    rhs = rhs,
    relation = relation,
    domain = domain
  )
}

if (F) {
  # Avoid shadowing base::symbols (a graphics function)
  # rm(symbols)  # remove if accidentally defined as a function

  # Define symbol registry for parsing
  symbols <- list(
    variables = c("vTechInp", "vTechOut", "vTechCap", "vTechAct"),
    parameters = c("pTechCinp2use", "pTechUse2cact", "pTechCact2cout"),
    mappings = c("mTechGroupComm"),
    sets = c("tech", "region", "comm", "commp", "year", "slice")
  )


  eqn_str <- "eqTechSng2Sng(tech, region, comm, commp, year, slice)$meqTechSng2Sng(tech, region, comm, commp, year, slice)..
   vTechInp(tech, comm, region, year, slice) *
   pTechCinp2use(tech, comm, region, year, slice)
   =e=
   vTechOut(tech, commp, region, year, slice) /
           pTechUse2cact(tech, commp, region, year, slice) /
           pTechCact2cout(tech, commp, region, year, slice);"

  eq_obj <- gams_to_multimod(eqn_str, symbols)
  str(eq_obj, max.level = 5)
  names(eq_obj)
  class(eq_obj)


  gams_eq <- "
eqTechAfUp(tech, region, year, slice)$meqTechAfUp(tech, region, year, slice)..
  vTechAct(tech, region, year, slice)
  =l=
  pTechAfUp(tech, region, year, slice) *
  pTechCap2act(tech) *
  vTechCap(tech, region, year) *
  pSliceShare(slice) *
  prod(weather$mTechWeatherAfUp(weather, tech),
       pTechWeatherAfUp(weather, tech) * pWeather(weather, region, year, slice)
  );
"

  # symbols <- list(
  #   variables = c("vTechAct", "vTechCap"),
  #   parameters = c("pTechAfUp", "pTechCap2act", "pSliceShare", "pTechWeatherAfUp", "pWeather", "mTechWeatherAfUp"),
  #   sets = c("tech", "region", "year", "slice", "weather")
  # )

  symbols <- list(
    variables = c("vTechAct", "vTechCap"),
    parameters = c("pTechAfUp", "pTechCap2act", "pWeather", "pTechWeatherAfUp"),
    mappings = c(
      "mTechWeatherAfUp", "mTechGroupComm", "mTechSpan",
      "meqTechAfUp"
    ),
    sets = c("tech", "region", "year", "slice", "weather")
  )


  eq_obj <- gams_to_multimod(gams_eq, symbols)

  str(eq_obj, max.level = 2)



  ###########

  gams_eq <- "
eqTechAfUp(tech, region, year, slice)$meqTechAfUp(tech, region, year, slice)..
         vTechAct(tech, region, year, slice)
         =l=
         pTechAfUp(tech, region, year, slice) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pSliceShare(slice) *
         prod(weather$mTechWeatherAfUp(weather, tech),
              pTechWeatherAfUp(weather, tech) *
              pWeather(weather, region, year, slice)
              );
"

  # symbols <- list(
  #   variables = c("vTechAct", "vTechCap"),
  #   parameters = c("pTechAfUp", "pTechCap2act", "pSliceShare", "pTechWeatherAfUp", "pWeather", "mTechWeatherAfUp"),
  #   sets = c("tech", "region", "year", "slice", "weather")
  # )

  eq_obj <- gams_to_multimod(gams_eq, symbols)

  str(eq_obj, max.level = 10)

  eq_obj$lhs
  eq_obj$rhs |> str()
  eq_obj$rhs$lhs$rhs |> str()

  pretty_print_multimod(eq_obj, multiline = TRUE)
  pretty_print_multimod(eq_obj, latex = TRUE)
  pretty_print_multimod(eq_obj, multiline = TRUE, latex = TRUE)
  # pretty_print_multimod(eq_obj, multiline = TRUE, latex = TRUE)

  # build_graph_from_expr(eq_obj)

  # library(DiagrammeR)

  # expr_tree <- eq_obj$rhs  # or any expression subtree like eq_obj$rhs$lhs$rhs
  #
  # res <- build_graph_from_expr(expr_tree)
  #
  # nodes_df <- do.call(rbind, lapply(res$nodes, as.data.frame))
  # edges_df <- do.call(rbind, lapply(res$edges, as.data.frame))
  #
  # graph <- create_graph() |>
  #   add_nodes_from_table(nodes_df, label_col = "label") |>
  #   add_edges_from_table(edges_df, from_col = "from", to_col = "to")
  #
  # render_graph(graph)

  # plot_expr(eq_obj$rhs, title = "RHS")

  # plot_expr(eq_obj$rhs, title = "RHS of Equation")
  build_expr_graph(eq_obj$rhs)
  # plot_expr(eq_obj$rhs, title = "RHS Expression Tree")
  # plot_expr(eq_obj$lhs, title = "RHS Expression Tree")

  # plot_expr_visnetwork(eq_obj$rhs, title = "RHS Expression Tree")

  plot_equation_visnetwork(eq_obj, title = "Full Equation: eqTechAfUp")
}

# Remove comments between $ontext and $offtext
remove_ontext_offtext <- function(lines) {
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
expand_gams_includes <- function(lines, base_path = ".") {
  expanded <- character()

  for (line in lines) {
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
      expanded <- c(expanded, included_lines)
    } else {
      expanded <- c(expanded, line)
    }
  }

  return(expanded)
}


normalize_gams_lines <- function(lines) {
  decl_patterns <- c(
    "^set(s)?\\b", "^alias(es)?\\b", "^parameter(s)?\\b", "^scalar(s)?\\b",
    "^table(s)?\\b", "^variable(s)?\\b", "^positive variable(s)?\\b",
    "^negative variable(s)?\\b", "^binary variable(s)?\\b", "^integer variable(s)?\\b",
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


read_gams_model_structure <- function(
    file_or_text,
    include = TRUE,
    interim_file = NULL,
    strict = TRUE,
    verbose = FALSE,
    ...) {
  # browser()
  if (file.exists(file_or_text)) {
    lines <- readLines(file_or_text)
  } else {
    lines <- unlist(strsplit(file_or_text, "\n"))
  }

  if (include) {
    lines <- expand_gams_includes(lines, base_path = dirname(file_or_text))
  } else {
    lines <- lines[!grepl("^\\$include\\b", lines, ignore.case = TRUE)]
  }

  # remove data
  # strip_gams_data_block <- function(lines) {
  #   text <- paste(lines, collapse = "\n")
  #   # (?s) enables "dotall" so . matches newlines
  #   cleaned <- gsub("(?s)/.*?/\\s*", " ", text, perl = TRUE)
  #   strsplit(cleaned, "\n", fixed = TRUE)[[1]]
  # }

  # Remove commented lines
  lines <- lines[!grepl("^\\*", lines)]
  lines <- remove_ontext_offtext(lines)

  # lines <- strip_gams_data_block(lines)


  lines <- normalize_gams_lines(lines)
  writeLines(lines, "tmp/no_comments.gms")

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
  # lines <- unlist(strsplit(lines, "\n"))

  # kewords_in_one_line <- function(key_pattern, lines) {
  #   result <- character(0)
  #   # Find the line number of the first occurrence of the keyword
  #   for (line in lines) {
  #     if (grepl(key_pattern, line, ignore.case = TRUE)) {
  #       # if (grepl(key_pattern, "alias")) browser()
  #       # extract the keyword and add it to the result
  #       keyword <- regmatches(line, regexpr(key_pattern, line, ignore.case = TRUE))[[1]]
  #       # keyword <- gsub("\\s+", "", keyword)  # remove spaces
  #       result <- c(result, keyword)
  #       # check if anything else is in the line
  #       the_rest <- gsub(key_pattern, "", line, ignore.case = TRUE)
  #       the_rest <- gsub("\\s+", "", the_rest)
  #       if (nchar(the_rest) > 0) {
  #         # add as a new line
  #         result <- c(result, the_rest)
  #       }
  #       next
  #     }
  #     result <- c(result, line)
  #   }
  #   result
  # }
  #
  # lines <- kewords_in_one_line("^set(s)\\b", lines)
  # lines <- kewords_in_one_line("^alias(es)?\\b", lines)
  # lines <- kewords_in_one_line("^parameter(s)?\\b", lines)
  # lines <- kewords_in_one_line("^(positive\\s+)?variable(s)?\\b", lines)
  # lines <- kewords_in_one_line("^variable(s)?\\b", lines)
  # lines <- kewords_in_one_line("^equation(s)?\\b", lines)
  # lines <- kewords_in_one_line("=[LEG]=", lines)
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
      i <- i + 1
      next
    } else if (grepl("^alias(es)?\\b", line, ignore.case = TRUE)) {
      # browser()
      mode <- "aliases"
      if (verbose) message(line, " -> ", mode)
      i <- i + 1
      next
    } else if (grepl("^parameter(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "parameters"
      if (verbose) message(line, " -> ", mode)
      i <- i + 1
      next
    } else if (grepl("^(positive\\s+)?variable(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "variables"
      if (verbose) message(line, " -> ", mode)
      i <- i + 1
      next
    } else if (grepl("^equation(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "equations"
      if (verbose) message(line, " -> ", mode)
      i <- i + 1
      next
    }

    if (verbose) cat(i, line, "\n")

    ## Mode-based parsing
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
        # check string after ()
        # rest_of_line <- sub("^[^(]+\\([^)]*\\)", "", line)
        mappings[[name]] <- list(name = name, desc = desc, dims = dims)
      } else {
        name <- name_part
        # parts <- strsplit(line, "\\s+")[[1]]
        # if (length(parts) >= 1) {
        #   name <- parts[1]
        #   desc <- paste(parts[-1], collapse = " ") |> trimws()
        #   # sets[[parts[1]]] <- list(name = parts[1])
        #   sets[[name]] <- list(name = name, dims = character(0), desc = desc)
        # } else {
        #   sets[[parts[1]]] <-
        #     list(name = parts[1], dims = character(0), desc = character(0))
        # }
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
          if (length(vars) == 2) {
            aliases[[length(aliases) + 1]] <- vars
          }
        }
      }
      i <- i + 1
      next
    }

    if (!is.null(mode) && mode %in% c("parameters", "variables")) {
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
      } else {
        variables[[sym_name]] <- symbol_obj
      }
      i <- i + 1
      next
    }

    # Don't treat lines with $.. as declarations — they're equation bodies
    if (!is.null(mode) && mode == "equations" && !grepl("\\$.*\\.\\.", line)) {
      match <- regexec("^([a-zA-Z0-9_]+)\\(([^)]*)\\)\\s*(.*)$", line)
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

    ## Outside declarations: detect equation body
    match_body_start <- regexec("^([a-zA-Z0-9_]+)\\([^)]*\\)(\\$[^.]*)?\\.\\.$", line)
    result_body_start <- regmatches(line, match_body_start)[[1]]

    if (length(result_body_start) >= 3) {
      eq_name <- result_body_start[2]
      domain_raw <- result_body_start[3]
      domain <- if (!is.na(domain_raw) && nzchar(domain_raw)) sub("^\\$", "", domain_raw) else NULL

      body_accum <- character(0)
      i <- i + 1

      while (i <= length(lines) && !grepl(";", lines[i])) {
        body_accum <- c(body_accum, trimws(lines[i]))
        i <- i + 1
      }
      if (verbose) {
        if (grepl(";", lines[i])) {
          message("Found semicolon at end of equation body")
        } else {
          message("No semicolon found at end of equation body")
        }
      }
      if (i <= length(lines)) {
        body_accum <- c(body_accum, trimws(gsub(";", "", lines[i])))
        i <- i + 1
      }

      if (!is.null(equations[[eq_name]])) {
        equations[[eq_name]]$domain <- domain
        equations[[eq_name]]$gams <- paste(body_accum, collapse = " ")
      } else {
        if (strict) {
          warning(paste("Equation body found for undeclared equation:", eq_name))
        } else {
          equations[[eq_name]] <- list(
            name = eq_name,
            dims = character(0),
            desc = "",
            domain = domain,
            gams = paste(body_accum, collapse = " ")
          )
        }
      }
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


coerce_param <- function(name, param_info) {
  new_parameter(
    name = name,
    dims = param_info$dims,
    data = param_info$data
  )
}

coerce_variable <- function(name, var_info) {
  new_variable(
    name = name,
    dims = var_info$dims,
    data = var_info$data,
    domain = var_info$domain %||% "continuous"
  )
}

coerce_gams_equation <- function(eqn_info, symbols) {
  # browser()
  name <- trimws(eqn_info$name)
  lhs_rhs <- trimws(eqn_info$gams)
  domain_str <- ""

  # ⛑ Skip if body is missing
  if (is.null(lhs_rhs) || !nzchar(lhs_rhs)) {
    warning(sprintf("Skipping equation %s: no GAMS body provided", name))
    return(NULL)
  }

  if (!is.null(eqn_info$domain) && nzchar(eqn_info$domain)) {
    domain_str <- paste0("$", eqn_info$domain)
  }

  # If gams field already contains "..", assume full GAMS string
  if (grepl("\\.\\.", lhs_rhs, fixed = TRUE)) {
    full_string <- lhs_rhs
  } else {
    # Else build full GAMS declaration
    full_string <- paste0(
      name, "(", paste(eqn_info$dims, collapse = ","), ")",
      domain_str, ".. ", lhs_rhs
    )
  }

  parsed_eqn <- gams_to_multimod(full_string, symbols)
  # parsed_eqn <- tryCatch(
  #   {
  #     gams_to_multimod(full_string, symbols)
  #   },
  #   error = function(e) {
  #     cat("❌ Failed to parse equation:\n")
  #     cat("Name: ", name, "\n")
  #     cat("Equation: ", full_string, "\n")
  #     cat("Error: ", conditionMessage(e), "\n")
  #     # Force termination
  #     base::stop(simpleError(stop(simpleError(sprintf("Parsing failed for equation '%s': %s", name, conditionMessage(e))))))
  #   }
  # )




  parsed_eqn$desc <- eqn_info$desc
  return(parsed_eqn)
}

#' @title Convert equation to GAMS syntax
#' @description Render a `equation` object as a GAMS equation string.
#' @param eqn A `equation` object.
#' @returns A character string with valid GAMS syntax.
#' @export
as_gams.equation <- function(eqn) {
  stopifnot(inherits(eqn, "equation"))

  format_expr_gams <- function(expr) {
    if (is.null(expr)) return("")
    switch(expr$type,
           "expression" = {
             lhs <- format_expr_gams(expr$lhs %||% expr$left)
             rhs <- format_expr_gams(expr$rhs %||% expr$right)
             paste0("(", lhs, " ", expr$op, " ", rhs, ")")
           },
           "sum" = {
             index <- expr$index
             domain <- if (!is.null(expr$domain)) paste0("$", format_expr_gams(expr$domain)) else ""
             body <- format_expr_gams(expr$value)
             paste0("sum(", index, domain, ", ", body, ")")
           },
           "prod" = {
             index <- expr$index
             domain <- if (!is.null(expr$domain)) paste0("$", format_expr_gams(expr$domain)) else ""
             body <- format_expr_gams(expr$value)
             paste0("prod(", index, domain, ", ", body, ")")
           },
           "when" = {
             then <- format_expr_gams(expr$then)
             cond <- format_expr_gams(expr$condition)
             paste0(then, "$", cond)
           },
           "variable" = {
             if (length(expr$dims)) paste0(expr$name, "(", paste(expr$dims, collapse = ","), ")") else expr$name
           },
           "parameter" = {
             if (length(expr$dims)) paste0(expr$name, "(", paste(expr$dims, collapse = ","), ")") else expr$name
           },
           "mapping" = {
             paste0(expr$name, "(", paste(expr$dims, collapse = ","), ")")
           },
           "symbol" = expr$value,
           "constant" = as.character(expr$value),
           paste0("<?>", expr$type)
    )
  }

  dims <- paste(eqn$dims, collapse = ",")
  dom <- if (!is.null(eqn$domain)) paste0("$", format_expr_gams(eqn$domain)) else ""
  lhs <- format_expr_gams(eqn$lhs)
  rhs <- format_expr_gams(eqn$rhs)
  relation <- switch(eqn$relation,
                     "==" = "=e=",
                     "<=" = "=l=",
                     ">=" = "=g=",
                     eqn$relation
  )

  paste0(eqn$name, "(", dims, ")", dom, " .. ", lhs, " ", relation, " ", rhs, ";")
}



