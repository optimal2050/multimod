split_top_level_dollar <- function(expr_str) {
  chars <- strsplit(expr_str, "")[[1]]
  level <- 0
  for (i in seq_along(chars)) {
    if (chars[i] == "(") level <- level + 1
    else if (chars[i] == ")") level <- level - 1
    else if (chars[i] == "$" && level == 0) {
      return(c(
        substring(expr_str, 1, i - 1),
        substring(expr_str, i + 1)
      ))
    }
  }
  return(NULL)  # No top-level $
}


split_aggregate_args <- function(expr_str) {
  # Remove leading aggregate name
  expr_str <- sub("^(sum|prod)\\(", "", expr_str)
  expr_str <- sub("\\)$", "", expr_str)

  chars <- strsplit(expr_str, "")[[1]]
  level <- 0
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "(") level <- level + 1
    else if (ch == ")") level <- level - 1
    else if (ch == "," && level == 0) {
      arg1 <- substring(expr_str, 1, i - 1)
      arg2 <- substring(expr_str, i + 1)
      return(c(trimws(arg1), trimws(arg2)))
    }
  }
  return(NULL)  # malformed or no split point
}

split_top_level_operators <- function(expr_str) {
  chars <- strsplit(expr_str, "")[[1]]
  ops <- c()
  parts <- c()
  buf <- ""
  level <- 0

  for (ch in chars) {
    if (ch == "(") {
      level <- level + 1
      buf <- paste0(buf, ch)
    } else if (ch == ")") {
      level <- level - 1
      buf <- paste0(buf, ch)
    } else if ((ch == "*" || ch == "/") && level == 0) {
      parts <- c(parts, trimws(buf))
      ops <- c(ops, ch)
      buf <- ""
    } else {
      buf <- paste0(buf, ch)
    }
  }
  parts <- c(parts, trimws(buf))

  list(parts = parts, ops = ops)
}


parse_gams_domain <- function(expr, symbols = list()) {
  expr <- trimws(expr)
  if (grepl("\\band\\b", expr, ignore.case = TRUE)) {
    parts <- strsplit(expr, "(?i)\\band\\b", perl = TRUE)[[1]]
    result <- parse_gams_domain(parts[1], symbols)
    for (i in 2:length(parts)) {
      result <- list(
        type = "logic", op = "and",
        left = result,
        right = parse_gams_domain(parts[i], symbols)
      )
    }
    return(result)
  }

  if (grepl("\\bor\\b", expr, ignore.case = TRUE)) {
    parts <- strsplit(expr, "(?i)\\bor\\b", perl = TRUE)[[1]]
    result <- parse_gams_domain(parts[1], symbols)
    for (i in 2:length(parts)) {
      result <- list(
        type = "logic", op = "or",
        left = result,
        right = parse_gams_domain(parts[i], symbols)
      )
    }
    return(result)
  }

  if (grepl("(?<![<>=])=(?![=])", expr, perl = TRUE)) {
    parts <- strsplit(expr, "=", fixed = TRUE)[[1]]
    return(list(
      type = "compare", op = "=",
      left = parse_gams_domain(parts[1], symbols),
      right = parse_gams_domain(parts[2], symbols)
    ))
  }

  if (grepl(">=", expr, fixed = TRUE)) {
    parts <- strsplit(expr, ">=", fixed = TRUE)[[1]]
    return(list(
      type = "compare", op = ">=",
      left = parse_gams_domain(parts[1], symbols),
      right = parse_gams_domain(parts[2], symbols)
    ))
  }

  if (grepl("<=", expr, fixed = TRUE)) {
    parts <- strsplit(expr, "<=", fixed = TRUE)[[1]]
    return(list(
      type = "compare", op = "<=",
      left = parse_gams_domain(parts[1], symbols),
      right = parse_gams_domain(parts[2], symbols)
    ))
  }

  if (grepl(">", expr, fixed = TRUE)) {
    parts <- strsplit(expr, ">", fixed = TRUE)[[1]]
    return(list(
      type = "compare", op = ">",
      left = parse_gams_domain(parts[1], symbols),
      right = parse_gams_domain(parts[2], symbols)
    ))
  }

  if (grepl("<", expr, fixed = TRUE)) {
    parts <- strsplit(expr, "<", fixed = TRUE)[[1]]
    return(list(
      type = "compare", op = "<",
      left = parse_gams_domain(parts[1], symbols),
      right = parse_gams_domain(parts[2], symbols)
    ))
  }

  # If nothing matched, it's just a set membership or parameter
  return(parse_gams_expr(expr, symbols))
}

parse_gams_header <- function(header) {
  # remove extra spaces
  header <- trimws(header)

  # Find the ".." separator (equation definition start)
  pos_double_dot <- regexpr("\\.\\.", header)
  if (pos_double_dot == -1) stop("Cannot find '..' in equation header.")

  header_part <- substring(header, 1, pos_double_dot - 1)
  rest <- substring(header, pos_double_dot + 2)

  # Split at top-level $
  parts <- split_top_level_dollar(header_part)

  if (!is.null(parts)) {
    # With domain condition
    eq_name_indices <- parts[1]
    domain_expr <- parts[2]

    eq_match <- regmatches(eq_name_indices, regexec("^([a-zA-Z0-9_]+)\\((.*)\\)$", eq_name_indices))[[1]]
    if (length(eq_match) < 3) stop("Cannot parse equation name and indices.")

    eq_name <- eq_match[2]
    indices <- trimws(strsplit(eq_match[3], ",")[[1]])

    dom_match <- regmatches(domain_expr, regexec("^([a-zA-Z0-9_]+)\\((.*)\\)$", domain_expr))[[1]]
    if (length(dom_match) < 3) stop("Cannot parse domain condition.")

    domain_name <- dom_match[2]
    domain_args <- trimws(strsplit(dom_match[3], ",")[[1]])

    domain <- list(
      type = "param",
      name = domain_name,
      args = domain_args
    )
  } else {
    # No domain condition
    eq_match <- regmatches(header_part, regexec("^([a-zA-Z0-9_]+)\\((.*)\\)$", header_part))[[1]]
    if (length(eq_match) < 3) stop("Cannot parse equation name and indices.")

    eq_name <- eq_match[2]
    indices <- trimws(strsplit(eq_match[3], ",")[[1]])
    domain <- NULL
  }

  list(
    name = eq_name,
    indices = indices,
    domain = domain,
    rest = rest
  )
}

# Utility: split at top-level operators only
split_top_level <- function(expr, ops = c("+", "-")) {
  chars <- strsplit(expr, "")[[1]]
  level <- 0
  tokens <- c()
  current <- ""
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "(") level <- level + 1
    else if (ch == ")") level <- level - 1
    if (level == 0 && ch %in% ops) {
      tokens <- c(tokens, current)
      tokens <- c(tokens, ch)
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }
  tokens <- c(tokens, current)
  return(tokens)
}

parse_gams_expr <- function(expr, symbols = list(), known_funcs = c("log", "exp", "abs", "sqrt")) {
  if (is.null(expr) || is.na(expr)) stop("parse_gams_expr() received NULL or NA")
  expr <- trimws(expr)

  # Clean malformed operator sequences
  expr <- gsub("\\*\\s*\\+", "*", expr)
  expr <- gsub("\\+\\s*\\+", "+", expr)
  expr <- gsub("\\*\\s*\\*", "*", expr)
  expr <- gsub("^\\*", "", expr)
  expr <- gsub("\\s+", " ", expr)

  # Handle aggregates: sum(...) or prod(...)
  if (grepl("^(sum|prod)\\(", expr)) {
    agg_type <- if (grepl("^sum\\(", expr)) "sum" else "prod"
    parts <- split_aggregate_args(expr)
    if (is.null(parts) || length(parts) != 2) stop("Malformed aggregate: cannot split arguments")

    index_domain <- split_top_level_dollar(parts[1])
    index <- trimws(index_domain[1])
    domain <- if (length(index_domain) > 1) parse_gams_domain(trimws(index_domain[2]), symbols) else NULL
    value <- parse_gams_expr(trimws(parts[2]), symbols, known_funcs)

    return(list(type = agg_type, index = index, domain = domain, value = value))
  }

  # Top-level $ condition
  split_dollar <- function(expr_str) {
    chars <- strsplit(expr_str, "")[[1]]
    level <- 0
    for (i in seq_along(chars)) {
      if (chars[i] == "(") level <- level + 1
      else if (chars[i] == ")") level <- level - 1
      else if (chars[i] == "$" && level == 0) {
        return(c(substring(expr_str, 1, i - 1), substring(expr_str, i + 1)))
      }
    }
    return(NULL)
  }

  dollar_parts <- split_dollar(expr)
  if (!is.null(dollar_parts)) {
    return(list(
      type = "cond",
      condition = parse_gams_expr(dollar_parts[2], symbols, known_funcs),
      then = parse_gams_expr(dollar_parts[1], symbols, known_funcs)
    ))
  }

  # Handle parentheses
  if (startsWith(expr, "(") && endsWith(expr, ")")) {
    return(parse_gams_expr(substr(expr, 2, nchar(expr) - 1), symbols, known_funcs))
  }

  # Arithmetic + and -
  split_top_level <- function(expr, ops = c("+", "-")) {
    chars <- strsplit(expr, "")[[1]]
    level <- 0
    tokens <- c()
    current <- ""
    for (i in seq_along(chars)) {
      ch <- chars[i]
      if (ch == "(") level <- level + 1
      else if (ch == ")") level <- level - 1
      if (level == 0 && ch %in% ops) {
        tokens <- c(tokens, current)
        tokens <- c(tokens, ch)
        current <- ""
      } else {
        current <- paste0(current, ch)
      }
    }
    tokens <- c(tokens, current)
    return(tokens)
  }

  tokens <- split_top_level(expr, c("+", "-"))
  if (length(tokens) >= 3) {
    result <- parse_gams_expr(tokens[1], symbols, known_funcs)
    for (i in seq(2, length(tokens) - 1, by = 2)) {
      op <- tokens[i]
      rhs <- parse_gams_expr(tokens[i + 1], symbols, known_funcs)
      result <- list(type = "expr", op = op, left = result, right = rhs)
    }
    return(result)
  }

  # Multiplication and division
  if (grepl("\\*", expr) || grepl("/", expr)) {
    ops <- unlist(regmatches(expr, gregexpr("\\*|/", expr)))
    tokens <- unlist(strsplit(expr, "\\*|/", perl = TRUE))
    tokens <- trimws(tokens)

    if (length(tokens) != length(ops) + 1) {
      stop(sprintf(
        "Malformed multiplication/division expression:\n  expr = %s\n  tokens = %d, operators = %d",
        expr, length(tokens), length(ops)
      ))
    }

    parsed <- lapply(tokens, parse_gams_expr, symbols = symbols, known_funcs = known_funcs)
    result <- parsed[[1]]
    for (i in seq_along(ops)) {
      result <- list(
        type = "expr", op = ops[i],
        left = result,
        right = parsed[[i + 1]]
      )
    }
    return(result)
  }

  # Function-like: name(args)
  if (grepl("^[a-zA-Z0-9_]+\\([^()]+\\)$", expr)) {
    name <- sub("\\(.*", "", expr)
    args <- gsub("[()]", "", sub("^[^(]+\\(", "", expr))
    args <- trimws(strsplit(args, ",")[[1]])

    symbol_type <- detect_symbol_type(name, symbols, known_funcs)
    return(list(type = symbol_type, name = name, args = args))
  }

  # Fallback: simple symbol
  return(list(type = "symbol", value = expr))
}


gams_to_multimod <- function(gams_eq, symbols) {
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
    domain <- list(
      type = "param",
      name = match[4],
      args = trimws(strsplit(match[5], ",")[[1]])
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
      sense <- "<="
      parts <- strsplit(body, "=[lL]=")[[1]]
    } else if (grepl("=[gG]=", body)) {
      sense <- ">="
      parts <- strsplit(body, "=[gG]=")[[1]]
    } else if (grepl("=[eE]=", body)) {
      sense <- "=="
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

    # Return the multimod_eqn object
    new_multimod_eqn(
      name = eq_name,
      declared_dims = indices,
      lhs = lhs,
      rhs = rhs,
      sense = sense,
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
    mappings = c("mTechWeatherAfUp", "mTechGroupComm", "mTechSpan",
                 "meqTechAfUp" ),
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
  eq_obj$rhs$left$right |> str()

  pretty_print_multimod(eq_obj, multiline = TRUE)
  pretty_print_multimod(eq_obj, latex = TRUE)
  pretty_print_multimod(eq_obj, multiline = TRUE, latex = TRUE)
  # pretty_print_multimod(eq_obj, multiline = TRUE, latex = TRUE)

  # build_graph_from_expr(eq_obj)

  # library(DiagrammeR)

  # expr_tree <- eq_obj$rhs  # or any expression subtree like eq_obj$rhs$left$right
  #
  # res <- build_graph_from_expr(expr_tree)
  #
  # nodes_df <- do.call(rbind, lapply(res$nodes, as.data.frame))
  # edges_df <- do.call(rbind, lapply(res$edges, as.data.frame))
  #
  # graph <- create_graph() %>%
  #   add_nodes_from_table(nodes_df, label_col = "label") %>%
  #   add_edges_from_table(edges_df, from_col = "from", to_col = "to")
  #
  # render_graph(graph)

  # plot_multimod_expr(eq_obj$rhs, title = "RHS")

  # plot_multimod_expr(eq_obj$rhs, title = "RHS of Equation")
  build_expr_graph(eq_obj$rhs)
  # plot_multimod_expr(eq_obj$rhs, title = "RHS Expression Tree")
  # plot_multimod_expr(eq_obj$lhs, title = "RHS Expression Tree")

  # plot_expr_visnetwork(eq_obj$rhs, title = "RHS Expression Tree")

  plot_multimod_eqn_visnetwork(eq_obj, title = "Full Equation: eqTechAfUp")


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

read_gams_model_structure <- function(file_or_text, strict = TRUE) {
  if (file.exists(file_or_text)) {
    lines <- readLines(file_or_text)
  } else {
    lines <- unlist(strsplit(file_or_text, "\n"))
  }

  # lines <- lines[!grepl("^\\s*\\*", lines)]  # Remove comments

  # Remove lines between $ontext and $offtext
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
  lines <- remove_ontext_offtext(lines)

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
      next
    }

    ## Block start detection
    if (grepl("^set(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "sets"
      i <- i + 1
      next
    }
    if (grepl("^alias(es)?\\b", line, ignore.case = TRUE)) {
      mode <- "aliases"
      i <- i + 1
      next
    }
    if (grepl("^parameter(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "parameters"
      i <- i + 1
      next
    }
    if (grepl("^(positive\\s+)?variable(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "variables"
      i <- i + 1
      next
    }
    if (grepl("^equation(s)?\\b", line, ignore.case = TRUE)) {
      mode <- "equations"
      i <- i + 1
      next
    }


    ## Mode-based parsing
    if (!is.null(mode) && mode == "sets") {
      if (grepl("\\(", line)) {
        name <- sub("\\(.*", "", line)
        inside <- sub("^[^(]+\\(([^)]+)\\).*", "\\1", line)
        dims <- trimws(unlist(strsplit(inside, ",")))
        mappings[[name]] <- list(name = name, dims = dims)
      } else {
        parts <- strsplit(line, "\\s+")[[1]]
        if (length(parts) >= 1) {
          sets[[parts[1]]] <- list(name = parts[1])
        }
      }
      i <- i + 1
      next
    }

    if (!is.null(mode) && mode == "aliases") {
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
      if (line == "") { i <- i + 1; next }

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

  list(
    sets = sets,
    mappings = mappings,
    aliases = aliases,
    parameters = parameters,
    variables = variables,
    equations = equations
  )
}

build_symbols_list <- function(model_info) {
  list(
    sets = names(model_info$sets),
    mappings = names(model_info$mappings),
    parameters = names(model_info$parameters),
    variables = names(model_info$variables)
  )
}



if (F) {
  # Example usage
  gams_file <- "../energyRt/gams/energyRt.gms"
  # sets_info <- read_gams_symbols(gams_file)

  # names(sets_info)

  # sets_info$variables |> str()

  # names(sets_info)
  # print(sets_info$sets)

  # length(sets_info$mappings)
  # unique(names(sets_info$mappings)) |> length()
  # print(sets_info$mappings)
  # print(sets_info$aliases)

  # eqns_info <- read_gams_equations(gams_file)

  # eqns_info$eqTechSng2Sng


  model_info <- read_gams_model_structure(gams_file)
  str(model_info, max.level = 1)
  str(model_info$equations$eqTechSng2Sng)
  model_info$equations$eqTechActSng

  class(model_info)
  model_info$equations$eqTechSng2Sng

  symbols <- build_symbols_list(model_info)

  parse_gams_expr(model_info$equations$eqTechSng2Sng$gams, symbols) |>
    str(max.level = 5)

  multimod_model <- coerce_model_info_to_multimod(model_info)

}

coerce_param <- function(name, param_info) {
  new_multimod_param(
    name = name,
    declared_dims = param_info$dims,
    data = param_info$data
  )
}

coerce_variable <- function(name, var_info) {
  new_multimod_var(
    name = name,
    declared_dims = var_info$dims,
    data = var_info$data,
    domain = var_info$domain %||% "continuous"
  )
}

# coerce_equation <- function(name, eqn_info, symbols) {
#   lhs_rhs <- eqn_info$gams
#   domain_str <- ""
#
#   if (!is.null(eqn_info$domain) && nzchar(eqn_info$domain)) {
#     domain_str <- paste0("$", eqn_info$domain)
#   }
#
#   # If gams field already contains "..", assume full GAMS string
#   if (grepl("\\.\\.", lhs_rhs, fixed = TRUE)) {
#     full_string <- lhs_rhs
#   } else {
#     # Else build full GAMS declaration
#     full_string <- paste0(
#       name, "(", paste(eqn_info$dims, collapse = ","), ")",
#       domain_str, " .. ", lhs_rhs
#     )
#   }
#
#   # Safe parse: if still error, capture it
#   parsed_eqn <- tryCatch(
#     {
#       gams_to_multimod(full_string, symbols)
#     },
#     error = function(e) {
#       message("Failed to parse equation: ", name)
#       message("Equation text: ", full_string)
#       stop(e)  # propagate error
#     }
#   )
#
#   parsed_eqn$description <- eqn_info$desc
#   return(parsed_eqn)
# }


coerce_equation <- function(name, eqn_info, symbols) {
  lhs_rhs <- eqn_info$gams
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
      domain_str, " .. ", lhs_rhs
    )
  }

  # Safe parse
  parsed_eqn <- tryCatch(
    {
      gams_to_multimod(full_string, symbols)
    },
    error = function(e) {
      message("Failed to parse equation: ", name)
      message("Equation text: ", full_string)
      stop(e)  # propagate
    }
  )

  parsed_eqn$description <- eqn_info$desc
  return(parsed_eqn)
}


coerce_model_info_to_multimod <- function(model_info) {
  symbols <- build_symbols_list(model_info)

  parameters <- lapply(names(model_info$parameters), function(p) {
    coerce_param(p, model_info$parameters[[p]])
  })
  names(parameters) <- names(model_info$parameters)

  variables <- lapply(names(model_info$variables), function(v) {
    coerce_variable(v, model_info$variables[[v]])
  })
  names(variables) <- names(model_info$variables)

  equations <- lapply(names(model_info$equations), function(e) {
    coerce_equation(e, model_info$equations[[e]], symbols)
  })
  names(equations) <- names(model_info$equations)

  new_multimod_model(
    sets = model_info$sets,
    mappings = model_info$mappings,
    parameters = parameters,
    variables = variables,
    equations = equations
  )
}




