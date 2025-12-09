#' Read GMPL/MathProg Model File
#'
#' Parse a GLPK/MathProg (GMPL) model file and return a model_structure object.
#' This is the intermediate representation that can be converted to multimod format
#' using as_multimod().
#'
#' @param model_file Path to GMPL model file (.txt or .mod), or a character vector
#'   containing the model code lines (for in-memory parsing)
#' @param data_file Optional path to separate .dat data file, or character vector
#'   containing data lines (not yet implemented for parsing, but can be provided)
#' @param as_multimod Logical. If TRUE (default), convert directly to multimod_model object.
#'   If FALSE, return model_structure object.
#' @param verbose Logical. If TRUE, print parsing progress (default FALSE)
#'
#' @return A model_structure object (or multimod_model if as_multimod=TRUE)
#' @export
#'
#' @examples
#' \dontrun{
#' # From file paths
#' mod_struct <- read_gmpl("osemosys.txt", as_multimod = FALSE)
#'
#' # From in-memory character vectors (e.g., package data)
#' data(example_models)
#' model <- read_gmpl(
#'   model_file = example_models$OSeMOSYS$gmpl$model,
#'   data_file = example_models$OSeMOSYS$gmpl$data
#' )
#'
#' # Convert to multimod
#' model <- as_multimod(mod_struct)
#'
#' # Or directly
#' model <- read_gmpl("osemosys.txt", as_multimod = TRUE)
#' }
register_index_alias <- function(collection, set_name, alias_name) {
  if (is.null(set_name) || is.null(alias_name)) {
    return(collection)
  }
  set_name <- trimws(set_name)
  alias_name <- trimws(alias_name)
  if (set_name == "" || alias_name == "") {
    return(collection)
  }

  # Don't create alias if set_name is already an alias value
  # This prevents l->l1, r->r1, etc. when l,r are already short forms
  if (set_name %in% unname(collection)) {
    return(collection)
  }

  # Don't add duplicate alias for same set
  existing <- collection[names(collection) == set_name]
  if (length(existing) > 0 && alias_name %in% existing) {
    return(collection)
  }

  collection <- c(collection, alias_name)
  names(collection)[length(collection)] <- set_name
  collection
}

#' Read GMPL model file
#'
#' @param model_file Path to GMPL model file or character vector of GMPL code
#' @param data_file Optional path to GMPL data file
#' @param as_multimod Logical; convert to multimod format (default: TRUE if data_file provided)
#' @param verbose Logical; print detailed progress messages (default: FALSE)
#'
#' @returns Parsed GMPL model structure or multimod object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- read_gmpl("model.mod", "data.dat")
#' }
read_gmpl <- function(model_file,
                      data_file = NULL,
                      as_multimod = !is.null(data_file),
                      verbose = FALSE) {
  # Handle both file paths and character vectors
  if (length(model_file) == 1 && file.exists(model_file)) {
    # It's a file path - read it
    lines <- readLines(model_file, warn = FALSE)
    model_name <- tools::file_path_sans_ext(basename(model_file))
    model_source <- normalizePath(model_file)
  } else {
    # It's already file contents (character vector)
    lines <- model_file
    model_name <- "gmpl_model"
    model_source <- "<in-memory>"
  }

  # Preprocess: remove comments and normalize whitespace
  lines <- preprocess_gmpl(lines)

  # Initialize temporary structure for parsing
  temp_model <- list(
    sets = list(),
    parameters = list(),
    variables = list(),
    equations = list(),
    objectives = list(),  # Can have multiple objectives
    models = list(),      # Model definitions
    aliases = list(),     # Will collect from indexing expressions
    index_aliases = character(0)
  )

  # Parse declarations
  temp_model <- parse_gmpl_declarations(lines, temp_model, verbose = verbose)

  # If data file provided, load data
  # if (!is.null(data_file)) {
  #   temp_model <- load_gmpl_data(data_file, temp_model) # !!!
  # }

  data_source_path <- NULL
  if (!is.null(data_file)) {
    # Handle both file paths and character vectors for data_file too
    if (length(data_file) == 1 && file.exists(data_file)) {
      data_source_path <- normalizePath(data_file)
    } else {
      data_source_path <- "<in-memory>"
    }
  }

  # Convert to model_structure format
  model_struct <- new_model_structure(
    name = model_name,
    desc = paste0("GMPL model from ", if (model_source == "<in-memory>") "in-memory source" else basename(model_source)),
    sets = temp_model$sets,
    mappings = list(),  # GMPL doesn't have explicit mappings
    aliases = temp_model$aliases,  # Extracted from indexing expressions
    parameters = temp_model$parameters,
    variables = temp_model$variables,
    equations = temp_model$equations,  # Objective is already in equations
    objectives = temp_model$objectives,  # Objective metadata (plural)
    models = list(),  # GMPL doesn't have explicit model definitions
    source = model_source,
    language = "gmpl"
  )

  if (!is.null(temp_model$index_aliases) && length(temp_model$index_aliases) > 0) {
    model_struct$index_aliases <- temp_model$index_aliases
  }

  if (!is.null(data_source_path)) {
    model_struct$data_source <- data_source_path
  }

  # Convert to multimod if requested
  if (as_multimod) {
    extra_args <- if (!is.null(data_source_path)) list(data_source = data_source_path) else list()
    as_multimod_fn <- getExportedValue("multimod", "as_multimod")
    model <- do.call(as_multimod_fn, c(list(model_struct), extra_args))
    model <- make_valid(model, verbose = verbose, revalidate = FALSE)
    if (!is.null(data_file)) {
      model <- import_gmpl_data(model, data_file)
    }
    return(model)
  }

  return(model_struct)
}

#' Preprocess GMPL Text
#'
#' Remove comments and normalize whitespace
#'
#' @param lines Character vector of file lines
#' @return Preprocessed lines
#' @keywords internal
preprocess_gmpl <- function(lines) {
  # Remove single-line comments (# to end of line)
  lines <- gsub("#.*$", "", lines)

  # Trim whitespace
  lines <- trimws(lines)

  # Remove empty lines
  lines <- lines[nchar(lines) > 0]

  # Join lines that are continuation of declarations
  # (declarations end with semicolon)
  text <- paste(lines, collapse = " ")

  return(text)
}

#' Parse GMPL Declarations
#'
#' Main parser that identifies and processes all declarations
#'
#' @param text Preprocessed GMPL text
#' @param model Model object to populate
#' @param verbose Logical, print progress
#' @return Updated model object
#' @keywords internal
parse_gmpl_declarations <- function(text, model, verbose = FALSE) {
  # Split into statements (semicolon-separated)
  statements <- split_statements(text)

  if (verbose) message("Parsing ", length(statements), " statements...")

  for (stmt in statements) {
    stmt <- trimws(stmt)
    if (nchar(stmt) == 0) next

    # Identify statement type by first keyword
    if (grepl("^set\\s+", stmt, ignore.case = TRUE)) {
      model <- parse_gmpl_set(stmt, model, verbose)

    } else if (grepl("^param\\s+", stmt, ignore.case = TRUE)) {
      model <- parse_gmpl_param(stmt, model, verbose)

    } else if (grepl("^var\\s+", stmt, ignore.case = TRUE)) {
      model <- parse_gmpl_var(stmt, model, verbose)

    } else if (grepl("^s\\.t\\.\\s+", stmt, ignore.case = TRUE)) {
      model <- parse_gmpl_equation(stmt, model, verbose)

    } else if (grepl("^(minimize|maximize)\\s+", stmt, ignore.case = TRUE)) {
      model <- parse_gmpl_objective(stmt, model, verbose)

    } else if (grepl("^(check|printf|display|solve|end|data|for|table)\\s*", stmt, ignore.case = TRUE)) {
      # Ignore these statements for now (output/control statements)
      next

    } else {
      if (verbose) warning("Unrecognized statement: ", substr(stmt, 1, 50))
    }
  }

  return(model)
}

#' Split Text into Statements
#'
#' Split GMPL text by semicolons, respecting nested braces
#'
#' @param text GMPL text
#' @return Character vector of statements
#' @keywords internal
split_statements <- function(text) {
  statements <- character()
  current <- ""
  brace_depth <- 0
  in_string <- FALSE

  chars <- strsplit(text, "")[[1]]
  n <- length(chars)

  i <- 1
  while (i <= n) {
    ch <- chars[i]

    # Handle strings
    if (ch == '"' || ch == "'") {
      if (!in_string) {
        in_string <- TRUE
        quote_char <- ch
      } else if (ch == quote_char) {
        in_string <- FALSE
      }
      current <- paste0(current, ch)
      i <- i + 1
      next
    }

    if (!in_string) {
      # Track brace depth
      if (ch == "{") {
        brace_depth <- brace_depth + 1
      } else if (ch == "}") {
        brace_depth <- brace_depth - 1
      }

      # Split on semicolon at depth 0
      if (ch == ";" && brace_depth == 0) {
        statements <- c(statements, trimws(current))
        current <- ""
        i <- i + 1
        next
      }
    }

    current <- paste0(current, ch)
    i <- i + 1
  }

  # Add final statement if any
  if (nchar(trimws(current)) > 0) {
    statements <- c(statements, trimws(current))
  }

  return(statements)
}

#' Parse GMPL Set Declaration
#'
#' @param stmt Set declaration statement
#' @param model Model object
#' @param verbose Logical, print progress
#' @return Updated model object
#' @keywords internal
parse_gmpl_set <- function(stmt, model, verbose = FALSE) {
  # Pattern: set NAME [indexing] [attributes];
  # Example: "set YEAR;"
  # Example: "set FUEL{r in REGION};"

  # Remove "set" keyword
  stmt <- sub("^set\\s+", "", stmt, ignore.case = TRUE)

  # Extract set name (up to { or ; or space)
  name_match <- regexpr("^[A-Za-z_][A-Za-z0-9_]*", stmt)
  name <- regmatches(stmt, name_match)
  stmt <- substring(stmt, name_match + attr(name_match, "match.length"))

  # Check for indexing
  indexing <- NULL
  if (grepl("^\\s*\\{", stmt)) {
    idx_result <- extract_indexing(stmt)
    indexing <- idx_result$indexing
    stmt <- idx_result$remaining
  }

  # Parse attributes (dimen, within, etc.) - skip for now
  attributes <- list()

  # Add to model (model_structure format)
  model$sets[[name]] <- list(
    name = name,
    desc = "",  # GMPL doesn't have descriptions
    dims = character(0)  # Simple sets have no dims
  )

  if (verbose) message("  Set: ", name)

  return(model)
}

#' Create AST Formula Node
#'
#' Wraps an expression AST with metadata about which parameter field it populates.
#' Used for both default expressions and parameter formulas.
#'
#' @param target Character, name of parameter this formula belongs to
#' @param field Character, which field: "defVal" or "formula"
#' @param expr AST node representing the expression
#' @param index_vars Character vector of index variable names used in expr
#' @param index_sets Character vector of set names corresponding to index_vars
#' @param args Character vector of parameter names this expr depends on
#' @return ast_formula object
#' @keywords internal
new_ast_formula <- function(target, field, expr, index_vars = character(),
                            index_sets = character(), args = character()) {
  structure(
    list(
      target = target,
      field = field,
      expr = expr,
      index_vars = index_vars,
      index_sets = index_sets,
      args = args
    ),
    class = c("ast_formula", "ast")
  )
}

#' Parse GMPL Parameter Declaration
#'
#' @param stmt Parameter declaration statement
#' @param model Model object
#' @param verbose Logical, print progress
#' @return Updated model object
#' @keywords internal
parse_gmpl_param <- function(stmt, model, verbose = FALSE) {
  # Pattern: param NAME [indexing] [attributes] [default value] [:= formula];
  # Example: "param OperationalLife{r in REGION, t in TECHNOLOGY};"
  # Example: "param DiscountRate{r in REGION} := 0.05;"

  # Remove "param" keyword
  stmt <- sub("^param\\s+", "", stmt, ignore.case = TRUE)

  # Extract parameter name
  name_match <- regexpr("^[A-Za-z_][A-Za-z0-9_]*", stmt)
  name <- regmatches(stmt, name_match)
  stmt <- substring(stmt, name_match + attr(name_match, "match.length"))

  # Check for indexing
  indexing <- NULL
  if (grepl("^\\s*\\{", stmt)) {
    idx_result <- extract_indexing(stmt)
    indexing <- idx_result$indexing
    stmt <- idx_result$remaining
  }

  # Parse attributes and default value
  default_val <- NULL
  formula <- NULL
  is_symbolic <- FALSE

  # Check for := formula
  if (grepl(":=", stmt)) {
    parts <- strsplit(stmt, ":=", fixed = TRUE)[[1]]
    stmt <- parts[1]
    formula <- trimws(parts[2])
  }

  # Check for "symbolic" attribute
  if (grepl("\\bsymbolic\\b", stmt, ignore.case = TRUE)) {
    is_symbolic <- TRUE
    stmt <- gsub("\\bsymbolic\\b", "", stmt, ignore.case = TRUE)
  }

  # Check for "default" keyword
  if (grepl("\\bdefault\\s+", stmt, ignore.case = TRUE)) {
    default_match <- regexpr("default\\s+[^,;]+", stmt, ignore.case = TRUE)
    default_str <- regmatches(stmt, default_match)
    default_val <- sub("default\\s+", "", default_str, ignore.case = TRUE)
    default_val <- trimws(default_val)

    # If symbolic, keep as string (remove quotes if present)
    if (is_symbolic) {
      default_val <- gsub("^['\"]|['\"]$", "", default_val)
    } else {
      # Try to parse as numeric first
      numeric_val <- suppressWarnings(as.numeric(default_val))
      if (!is.na(numeric_val)) {
        default_val <- numeric_val
      } else {
        # Not numeric - parse as expression to AST
        if (verbose) message("  Parsing default expression: ", default_val)
        default_val <- tryCatch({
          ast <- parse_gmpl_expr(default_val, symbols = model)

          # Extract index variables and sets from indexing
          idx_vars <- character()
          idx_sets <- character()
          if (!is.null(indexing)) {
            idx_vars <- sapply(indexing$specs, function(s) {
              if (length(s$vars) == 1) s$vars[1]
              else tolower(substr(s$set, 1, 1))
            })
            idx_sets <- sapply(indexing$specs, function(s) s$set)
          }

          # Extract dependencies
          dep_args <- extract_args(ast)

          # Wrap in ast_formula
          new_ast_formula(
            target = name,
            field = "defVal",
            expr = ast,
            index_vars = idx_vars,
            index_sets = idx_sets,
            args = dep_args
          )
        }, error = function(e) {
          warning("Could not parse default expression for parameter '", name, "': ", e$message)
          # Fall back to keeping as string
          default_val
        })
        if (verbose && inherits(default_val, "ast_formula")) {
          message("    Default expression wrapped in ast_formula successfully")
        }
      }
    }
  }

  # Extract dims from indexing and collect aliases
  dims <- if (!is.null(indexing)) {
    # Collect aliases: short var -> full set name
    for (spec in indexing$specs) {
      if (!is.null(spec$set) && length(spec$vars) == 1) {
        var_name <- spec$vars[1]
        set_name <- spec$set
        # Store alias: SETNAME -> c(SETNAME, short_alias)
        if (is.null(model$aliases[[set_name]])) {
          model$aliases[[set_name]] <- c(set_name, var_name)
        } else if (!var_name %in% model$aliases[[set_name]]) {
          model$aliases[[set_name]] <- c(model$aliases[[set_name]], var_name)
        }
      }
    }
    sapply(indexing$specs, function(spec) spec$set)
  } else {
    character(0)
  }

  # Extract iterator variable names (dims_index_aliases) for computed parameters
  dims_index_aliases <- if (!is.null(indexing)) {
    result <- character(length(indexing$specs))
    for (i in seq_along(indexing$specs)) {
      spec <- indexing$specs[[i]]
      if (length(spec$vars) == 1) {
        result[i] <- spec$vars[1]
      } else {
        # Fallback if no iterator variable specified
        result[i] <- tolower(substr(spec$set, 1, 1))
      }
    }
    names(result) <- sapply(indexing$specs, function(spec) spec$set)
    result
  } else {
    character(0)
  }

  # Parse formula into AST if present
  formula_expr <- NULL
  if (!is.null(formula) && nzchar(formula)) {
    if (verbose) message("  Parsing formula for parameter: ", name)
    formula_expr <- tryCatch({
      ast <- parse_gmpl_expr(formula, symbols = model)

      # Extract index variables and sets from indexing
      idx_vars <- character()
      idx_sets <- character()
      if (!is.null(indexing)) {
        idx_vars <- sapply(indexing$specs, function(s) {
          if (length(s$vars) == 1) s$vars[1]
          else tolower(substr(s$set, 1, 1))
        })
        idx_sets <- sapply(indexing$specs, function(s) s$set)
      }

      # Extract dependencies
      dep_args <- extract_args(ast)

      # Wrap in ast_formula
      new_ast_formula(
        target = name,
        field = "formula",
        expr = ast,
        index_vars = idx_vars,
        index_sets = idx_sets,
        args = dep_args
      )
    }, error = function(e) {
      warning("Could not parse formula for parameter '", name, "': ", e$message)
      NULL
    })
    if (verbose) {
      if (!is.null(formula_expr)) {
        message("    Formula wrapped in ast_formula successfully")
      } else {
        message("    Formula parsing returned NULL")
      }
    }
  } else {
    if (verbose && !is.null(formula)) {
      message("  No formula for parameter: ", name, " (formula='", formula, "')")
    }
  }

  # Store parameter metadata in model_structure-friendly format (list, not AST)
  param <- list(
    name = name,
    dims = dims,
    desc = "",
    defVal = default_val,
    symbolic = is_symbolic,
    data = NULL,
    formula = formula_expr
  )

  if (length(dims_index_aliases) > 0) {
    param$dims_index_aliases <- dims_index_aliases
    for (i in seq_along(dims_index_aliases)) {
      set_name <- names(dims_index_aliases)[i]
      alias_value <- dims_index_aliases[[i]]
      model$index_aliases <- register_index_alias(model$index_aliases, set_name, alias_value)
    }
  }

  model$parameters[[name]] <- param

  if (verbose) message("  Parameter: ", name, " [", paste(dims, collapse=", "), "]")

  return(model)
}

#' Parse GMPL Variable Declaration
#'
#' @param stmt Variable declaration statement
#' @param model Model object
#' @param verbose Logical, print progress
#' @return Updated model object
#' @keywords internal
parse_gmpl_var <- function(stmt, model, verbose = FALSE) {
  # Pattern: var NAME [indexing] [>= lb] [<= ub] [binary|integer];
  # Example: "var RateOfDemand{r in REGION, l in TIMESLICE, f in FUEL, y in YEAR} >= 0;"

  # Remove "var" keyword
  stmt <- sub("^var\\s+", "", stmt, ignore.case = TRUE)

  # Extract variable name
  name_match <- regexpr("^[A-Za-z_][A-Za-z0-9_]*", stmt)
  name <- regmatches(stmt, name_match)
  stmt <- substring(stmt, name_match + attr(name_match, "match.length"))

  # Check for indexing
  indexing <- NULL
  if (grepl("^\\s*\\{", stmt)) {
    idx_result <- extract_indexing(stmt)
    indexing <- idx_result$indexing
    stmt <- idx_result$remaining
  }

  # Parse bounds and type
  lower_bound <- NULL
  upper_bound <- NULL
  var_type <- "continuous"

  # Check for >= lower bound
  if (grepl(">=", stmt)) {
    lb_match <- regexpr(">=\\s*[^,;<]+", stmt)
    lb_str <- regmatches(stmt, lb_match)
    lower_bound <- trimws(sub(">=\\s*", "", lb_str))
    # Try to convert to numeric
    lower_bound <- tryCatch(as.numeric(lower_bound), warning = function(w) lower_bound, error = function(e) lower_bound)
  }

  # Check for <= upper bound
  if (grepl("<=", stmt)) {
    ub_match <- regexpr("<=\\s*[^,;<]+", stmt)
    ub_str <- regmatches(stmt, ub_match)
    upper_bound <- trimws(sub("<=\\s*", "", ub_str))
    # Try to convert to numeric
    upper_bound <- tryCatch(as.numeric(upper_bound), warning = function(w) upper_bound, error = function(e) upper_bound)
  }

  # Check for binary/integer
  if (grepl("\\bbinary\\b", stmt, ignore.case = TRUE)) {
    var_type <- "binary"
  } else if (grepl("\\binteger\\b", stmt, ignore.case = TRUE)) {
    var_type <- "integer"
  }

  # Extract dims from indexing and collect aliases
  dims <- if (!is.null(indexing)) {
    # Collect aliases: short var -> full set name
    for (spec in indexing$specs) {
      if (!is.null(spec$set) && length(spec$vars) == 1) {
        var_name <- spec$vars[1]
        set_name <- spec$set
        # Store alias: SETNAME -> c(SETNAME, short_alias)
        if (is.null(model$aliases[[set_name]])) {
          model$aliases[[set_name]] <- c(set_name, var_name)
        } else if (!var_name %in% model$aliases[[set_name]]) {
          model$aliases[[set_name]] <- c(model$aliases[[set_name]], var_name)
        }
      }
    }
    sapply(indexing$specs, function(spec) spec$set)
  } else {
    character(0)
  }

  # Extract iterator variable names (dims_index_aliases) for variables
  dims_index_aliases <- if (!is.null(indexing)) {
    sapply(indexing$specs, function(spec) {
      if (length(spec$vars) > 0) spec$vars[1] else NULL
    })
  } else {
    character(0)
  }

  # Create bounds structure if any bounds are specified
  bounds <- NULL
  if (!is.null(lower_bound) || !is.null(upper_bound)) {
    bounds <- list()
    if (!is.null(lower_bound)) bounds$lo <- lower_bound
    if (!is.null(upper_bound)) bounds$up <- upper_bound
  }

  # Add to model (model_structure format)
  var <- list(
    name = name,
    dims = dims,
    desc = "",
    bounds = bounds,
    vtype = if (var_type != "continuous") var_type else NULL
  )

  # Add dims_index_aliases if indexing present
  if (length(dims_index_aliases) > 0) {
    var$dims_index_aliases <- dims_index_aliases
    for (i in seq_along(dims_index_aliases)) {
      set_name <- names(dims_index_aliases)[i]
      alias_value <- dims_index_aliases[[i]]
      model$index_aliases <- register_index_alias(model$index_aliases, set_name, alias_value)
    }
  }

  model$variables[[name]] <- var

  if (verbose) message("  Variable: ", name, " [", paste(dims, collapse=", "), "]")

  return(model)
}

#' Parse GMPL Equation Declaration
#'
#' @param stmt Equation declaration statement (s.t. constraint)
#' @param model Model object
#' @param verbose Logical, print progress
#' @return Updated model object
#' @keywords internal
parse_gmpl_equation <- function(stmt, model, verbose = FALSE) {
  # Pattern: s.t. NAME [indexing]: [condition:] expr sense expr;
  # Example: "s.t. EQ_SpecifiedDemand{r in REGION, l in TIMESLICE, f in FUEL, y in YEAR: SpecifiedAnnualDemand[r,f,y] <> 0}: lhs = rhs;"
  # browser()
  # Remove "s.t." keyword
  stmt <- sub("^s\\.t\\.\\s+", "", stmt, ignore.case = TRUE)

  # Extract constraint name
  name_match <- regexpr("^[A-Za-z_][A-Za-z0-9_]*", stmt)
  name <- regmatches(stmt, name_match)
  stmt <- substring(stmt, name_match + attr(name_match, "match.length"))

  # Check for indexing
  indexing <- NULL
  condition <- NULL
  if (grepl("^\\s*\\{", stmt)) {
    idx_result <- extract_indexing(stmt)
    indexing <- idx_result$indexing
    condition <- idx_result$condition
    stmt <- idx_result$remaining
  }

  # Skip past the colon after name/indexing
  stmt <- sub("^\\s*:\\s*", "", stmt)

  # Parse constraint body (lhs sense rhs)
  # For now, store as unparsed expression
  body <- trimws(stmt)

  # Determine sense (<=, >=, =) at top level only (not inside braces/brackets/parens)
  sense <- NULL
  depth <- 0
  chars <- strsplit(body, "")[[1]]
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch %in% c("(", "[", "{")) {
      depth <- depth + 1
    } else if (ch %in% c(")", "]", "}")) {
      depth <- depth - 1
    } else if (depth == 0 && i < length(chars)) {
      # Check for two-char operators
      two_char <- paste0(chars[i], chars[i+1])
      if (two_char == "<=" && is.null(sense)) {
        sense <- "<="
        break
      } else if (two_char == ">=" && is.null(sense)) {
        sense <- ">="
        break
      } else if (ch == "=" && is.null(sense)) {
        # Make sure it's not part of <= or >=
        prev_ch <- if (i > 1) chars[i-1] else ""
        next_ch <- if (i < length(chars)) chars[i+1] else ""
        if (!(prev_ch %in% c("<", ">", "!")) && !(next_ch == "=")) {
          sense <- "="
          break
        }
      }
    }
  }

  if (is.null(sense)) {
    warning("Could not determine sense for equation: ", name)
    sense <- "="  # Default fallback
  }

  # Extract dims from indexing and collect aliases
  dims <- if (!is.null(indexing)) {
    # Collect aliases: short var -> full set name
    for (spec in indexing$specs) {
      if (!is.null(spec$set) && length(spec$vars) == 1) {
        var_name <- spec$vars[1]
        set_name <- spec$set
        # Store alias: SETNAME -> c(SETNAME, short_alias)
        if (is.null(model$aliases[[set_name]])) {
          model$aliases[[set_name]] <- c(set_name, var_name)
        } else if (!var_name %in% model$aliases[[set_name]]) {
          model$aliases[[set_name]] <- c(model$aliases[[set_name]], var_name)
        }
      }
    }
    sapply(indexing$specs, function(spec) spec$set)
  } else {
    character(0)
  }

  # Create equation-specific index alias mapping
  dims_index_aliases <- if (!is.null(indexing)) {
    result <- character(length(indexing$specs))
    names(result) <- sapply(indexing$specs, function(spec) spec$set)
    for (i in seq_along(indexing$specs)) {
      spec <- indexing$specs[[i]]
      if (length(spec$vars) == 1) {
        result[i] <- spec$vars[1]
      } else {
        result[i] <- spec$set  # fallback to set name
      }
    }
    result
  } else {
    NULL
  }

  if (!is.null(dims_index_aliases) && length(dims_index_aliases) > 0) {
    for (i in seq_along(dims_index_aliases)) {
      set_name <- names(dims_index_aliases)[i]
      alias_value <- dims_index_aliases[[i]]
      model$index_aliases <- register_index_alias(model$index_aliases, set_name, alias_value)
    }
  }

  # Add to model (model_structure format)
  # Store as unparsed for now - Phase 2 will parse expressions
  model$equations[[name]] <- list(
    name = name,
    dims = dims,
    dims_index_aliases = dims_index_aliases,  # Store equation-specific index vars
    desc = "",
    body = body,  # Unparsed expression
    sense = sense,
    condition = condition
  )

  if (verbose) message("  Equation: ", name, " [", paste(dims, collapse=", "), "]")

  return(model)
}

#' Parse GMPL Objective Declaration
#'
#' @param stmt Objective declaration statement
#' @param model Model object
#' @param verbose Logical, print progress
#' @return Updated model object
#' @keywords internal
parse_gmpl_objective <- function(stmt, model, verbose = FALSE) {
  # Pattern: (minimize|maximize) VARNAME: expr;
  # Example: "minimize cost: sum{r in REGION, y in YEAR} TotalDiscountedCost[r,y];"
  # This creates:
  # 1. A variable named VARNAME (e.g., cost)
  # 2. An equation: VARNAME = expr
  # 3. Objective metadata for optimization direction

  # Store original statement for source_line
  original_stmt <- stmt

  # Extract sense (minimize or maximize)
  sense <- if (grepl("^minimize", stmt, ignore.case = TRUE)) {
    "minimize"
  } else {
    "maximize"
  }

  # Remove sense keyword
  stmt <- sub("^(minimize|maximize)\\s+", "", stmt, ignore.case = TRUE)

  # Extract variable name (up to colon) - this is the objective variable
  name_match <- regexpr("^[A-Za-z_][A-Za-z0-9_]*", stmt)
  var_name <- regmatches(stmt, name_match)
  stmt <- substring(stmt, name_match + attr(name_match, "match.length"))

  # Skip past colon
  stmt <- sub("^\\s*:\\s*", "", stmt)

  # Store expression (unparsed for now)
  expr <- trimws(stmt)

  # Store objective metadata in model$objectives list as unnamed element
  # Add to list (append)
  model$objectives[[length(model$objectives) + 1]] <- list(
    variable = var_name,
    sense = sense,
    gmpl = original_stmt,
    model = NULL  # No explicit model name in GMPL
  )

  # Add objective variable if it doesn't exist
  if (!var_name %in% names(model$variables)) {
    model$variables[[var_name]] <- list(
      name = var_name,
      dims = character(0),  # Scalar
      desc = paste0("Objective value (", sense, ")"),
      domain = NULL,
      vtype = "free",
      bounds = list(lo = NULL, up = NULL)
    )
  }

  # Parse expr to extract dims_index_aliases from sum/product if present
  dims_index_aliases <- NULL
  tryCatch({
    # Quick parse to check if there's a sum/product with indexing
    if (grepl("^sum\\s*\\{", expr) || grepl("^product\\s*\\{", expr)) {
      # Extract the indexing from sum{...} or product{...}
      idx_result <- extract_indexing(substring(expr, 4))  # Skip "sum" or "product"
      if (!is.null(idx_result$indexing)) {
        dims_index_aliases <- character(length(idx_result$indexing$specs))
        for (i in seq_along(idx_result$indexing$specs)) {
          spec <- idx_result$indexing$specs[[i]]
          if (length(spec$vars) == 1) {
            dims_index_aliases[i] <- spec$vars[1]
          } else {
            dims_index_aliases[i] <- tolower(substr(spec$set, 1, 1))
          }
        }
        names(dims_index_aliases) <- sapply(idx_result$indexing$specs, function(s) s$set)
      }
    }
  }, error = function(e) {
    # If extraction fails, leave as NULL
  })

  # Add objective equation: var_name = expr
  # Use "objective" as the equation name (standard convention)
  eq_name <- "objective"
  model$equations[[eq_name]] <- list(
    name = eq_name,
    dims = character(0),  # Scalar
    dims_index_aliases = dims_index_aliases,  # From sum/product iterators
    desc = paste0("Objective equation (", sense, ")"),
    body = paste0(var_name, " = ", expr),
    sense = sense  # Store sense to identify as objective later
  )

  if (verbose) message("  Objective: ", var_name, " (", sense, ")")

  return(model)
}

#' Extract Indexing Expression from GMPL Statement
#'
#' Parse {index_vars in sets: condition} pattern
#'
#' @param stmt Statement starting with indexing expression
#' @return List with indexing structure and remaining statement
#' @keywords internal
extract_indexing <- function(stmt) {
  # Find matching closing brace
  brace_start <- regexpr("\\{", stmt)
  if (brace_start < 0) {
    return(list(indexing = NULL, condition = NULL, remaining = stmt))
  }

  # Find matching }
  depth <- 0
  chars <- strsplit(stmt, "")[[1]]
  start_pos <- brace_start
  end_pos <- -1

  for (i in start_pos:length(chars)) {
    if (chars[i] == "{") depth <- depth + 1
    if (chars[i] == "}") depth <- depth - 1
    if (depth == 0) {
      end_pos <- i
      break
    }
  }

  if (end_pos < 0) {
    warning("Unmatched braces in indexing expression")
    return(list(indexing = NULL, condition = NULL, remaining = stmt))
  }

  # Extract indexing content
  idx_content <- substring(stmt, start_pos + 1, end_pos - 1)
  remaining <- substring(stmt, end_pos + 1)

  # Parse indexing content
  # Split by colon to separate indices from condition
  parts <- strsplit(idx_content, ":", fixed = TRUE)[[1]]

  indices_str <- trimws(parts[1])
  condition <- if (length(parts) > 1) trimws(parts[2]) else NULL

  # Parse index specifications
  # Split by comma (handling nested structures)
  index_specs <- parse_index_list(indices_str)

  indexing <- list(
    specs = index_specs,
    raw = idx_content
  )

  return(list(
    indexing = indexing,
    condition = condition,
    remaining = remaining
  ))
}

#' Parse Index List
#'
#' Parse comma-separated index specifications like "i in SET, j in SET2"
#'
#' @param str Index list string
#' @return List of index specifications
#' @keywords internal
parse_index_list <- function(str) {
  # Simple comma split for now (should handle nested braces properly)
  specs_raw <- strsplit(str, ",")[[1]]

  specs <- lapply(specs_raw, function(spec) {
    spec <- trimws(spec)

    # Pattern: "var in SET" or "(i,j) in SET"
    if (grepl("\\bin\\b", spec)) {
      parts <- strsplit(spec, "\\s+in\\s+", perl = TRUE)[[1]]
      var_part <- trimws(parts[1])
      set_part <- trimws(parts[2])

      # Handle tuple pattern (i,j)
      if (grepl("^\\(", var_part)) {
        var_part <- gsub("[()]", "", var_part)
        vars <- strsplit(var_part, ",")[[1]]
        vars <- trimws(vars)
      } else {
        vars <- var_part
      }

      return(list(
        vars = vars,
        set = set_part
      ))
    }

    # If no "in", might be just variable name
    return(list(
      vars = trimws(spec),
      set = NULL
    ))
  })

  return(specs)
}

#' Parse GMPL Expression to AST
#'
#' Convert a GMPL expression string to multimod AST format
#'
#' @param expr Character string containing GMPL expression
#' @param symbols Named list of known symbols (sets, parameters, variables)
#' @param depth Current recursion depth (internal use)
#' @param max_depth Maximum recursion depth
#' @return AST node
#' @keywords internal
gmpl_symbol_node <- function(name, symbols) {
  if (is.null(name)) {
    return(ast_symbol(name))
  }
  symbol_type <- tryCatch(
    suppressWarnings(detect_symbol_type(name, symbols = symbols)),
    error = function(e) "symbol"
  )
  canonical_name <- name
  if (!is.null(symbols)) {
    pool <- switch(symbol_type,
                   variable = symbols$variables,
                   parameter = symbols$parameters,
                   set = symbols$sets,
                   NULL
    )
    if (!is.null(pool)) {
      idx <- match(tolower(name), tolower(pool))
      if (!is.na(idx)) {
        canonical_name <- pool[idx]
      }
    }
  }
  ctor <- switch(symbol_type,
                 variable = ast_variable,
                 parameter = ast_parameter,
                 set = function(n, ...) ast_set(n),
                 symbol = ast_symbol,
                 func = ast_symbol,
                 func_indexed = ast_symbol,
                 ast_symbol
  )
  if (identical(ctor, ast_set)) {
    return(ctor(canonical_name))
  }
  ctor(canonical_name)
}

parse_gmpl_expr <- function(expr, symbols = list(), depth = 0, max_depth = 50) {
  if (depth > max_depth) {
    stop("Maximum expression nesting depth exceeded at: ", substr(expr, 1, 50))
  }

  expr <- trimws(expr)
  if (expr == "" || is.null(expr)) {
    return(NULL)
  }

  # Numeric constants
  if (grepl("^-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", expr)) {
    return(ast_constant(as.numeric(expr)))
  }

  # String literals (quoted)
  if (grepl('^["\'].*["\']$', expr)) {
    return(ast_constant(gsub('["\']', '', expr)))
  }

  # Check for index shift patterns (y-1, y+1, ls-1, etc.) BEFORE expression parsing
  # This must come before arithmetic operators are processed
  if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\s*[+-]\\s*[0-9]+$", expr)) {
    parts <- regmatches(expr, regexec("^([a-zA-Z_][a-zA-Z0-9_]*)\\s*([+-])\\s*([0-9]+)$", expr))[[1]]
    if (length(parts) == 4) {
      symbol <- parts[2]
      sign <- parts[3]
      offset <- as.integer(parts[4])
      if (sign == "-") offset <- -offset
      return(ast_shift(symbol, offset))
    }
  }

  # Binary operators (in precedence order, low to high)

  # Conditional: if-then-else (LOWEST precedence - checked first)
  # This must come BEFORE logical operators because conditions can contain && and ||
  if (grepl("\\bif\\b", expr)) {
    if_parts <- parse_if_then_else_expr(expr)
    if (!is.null(if_parts)) {
      return(ast_when(
        condition = parse_gmpl_expr(if_parts$condition, symbols, depth + 1, max_depth),
        then = parse_gmpl_expr(if_parts$then_expr, symbols, depth + 1, max_depth),
        otherwise = if (!is.null(if_parts$else_expr)) {
          parse_gmpl_expr(if_parts$else_expr, symbols, depth + 1, max_depth)
        } else {
          NULL
        }
      ))
    }
  }

  # Logical OR (word "or" or symbol ||)
  if (has_top_level_op(expr, "\\|\\|")) {
    parts <- split_by_op(expr, "\\|\\|")
    return(ast_expression("or",
                          lhs = parse_gmpl_expr(parts$lhs, symbols, depth + 1, max_depth),
                          rhs = parse_gmpl_expr(parts$rhs, symbols, depth + 1, max_depth)
    ))
  }
  if (has_top_level_op(expr, "\\bor\\b")) {
    parts <- split_by_op(expr, "\\bor\\b")
    return(ast_expression("or",
                          lhs = parse_gmpl_expr(parts$lhs, symbols, depth + 1, max_depth),
                          rhs = parse_gmpl_expr(parts$rhs, symbols, depth + 1, max_depth)
    ))
  }

  # Logical AND (symbol && or word "and")
  if (has_top_level_op(expr, "&&")) {
    parts <- split_by_op(expr, "&&")
    return(ast_expression("and",
                          lhs = parse_gmpl_expr(parts$lhs, symbols, depth + 1, max_depth),
                          rhs = parse_gmpl_expr(parts$rhs, symbols, depth + 1, max_depth)
    ))
  }
  if (has_top_level_op(expr, "\\band\\b")) {
    parts <- split_by_op(expr, "\\band\\b")
    return(ast_expression("and",
                          lhs = parse_gmpl_expr(parts$lhs, symbols, depth + 1, max_depth),
                          rhs = parse_gmpl_expr(parts$rhs, symbols, depth + 1, max_depth)
    ))
  }

  # Comparison operators
  for (op in c("<=", ">=", "<>", "!=", "=", "<", ">")) {
    op_pattern <- switch(op,
                         "<=" = "<=",
                         ">=" = ">=",
                         "<>" = "<>",
                         "!=" = "!=",
                         "=" = "(?<![<>!])=(?!=)",  # = but not <=, >=, !=, ==
                         "<" = "(?<!<)<(?![=>])",   # < but not <=, <>
                         ">" = "(?<!>)>(?!=)"        # > but not >=
    )

    if (has_top_level_op(expr, op_pattern)) {
      parts <- split_by_op(expr, op_pattern)
      return(ast_expression(op,
                            lhs = parse_gmpl_expr(parts$lhs, symbols, depth + 1, max_depth),
                            rhs = parse_gmpl_expr(parts$rhs, symbols, depth + 1, max_depth)
      ))
    }
  }

  # Addition and subtraction
  for (op in c("\\+", "-")) {
    if (has_top_level_op(expr, op)) {
      parts <- split_by_op(expr, op)
      if (!is.null(parts$lhs) && nchar(trimws(parts$lhs)) > 0) {
        return(ast_expression(gsub("\\\\", "", op),
                              lhs = parse_gmpl_expr(parts$lhs, symbols, depth + 1, max_depth),
                              rhs = parse_gmpl_expr(parts$rhs, symbols, depth + 1, max_depth)
        ))
      } else {
        # Unary minus/plus
        return(ast_unary(gsub("\\\\", "", op),
                         parse_gmpl_expr(parts$rhs, symbols, depth + 1, max_depth)
        ))
      }
    }
  }

  # Multiplication, division, mod, div
  for (op in c("\\*", "/", "\\bmod\\b", "\\bdiv\\b")) {
    if (has_top_level_op(expr, op)) {
      parts <- split_by_op(expr, op)
      return(ast_expression(gsub("\\\\|\\b", "", op),
                            lhs = parse_gmpl_expr(parts$lhs, symbols, depth + 1, max_depth),
                            rhs = parse_gmpl_expr(parts$rhs, symbols, depth + 1, max_depth)
      ))
    }
  }

  # Exponentiation (right-associative)
  if (has_top_level_op(expr, "\\^")) {
    parts <- split_by_op(expr, "\\^", rightmost = TRUE)
    return(ast_expression("^",
                          lhs = parse_gmpl_expr(parts$lhs, symbols, depth + 1, max_depth),
                          rhs = parse_gmpl_expr(parts$rhs, symbols, depth + 1, max_depth)
    ))
  }

  # Unary NOT
  if (grepl("^\\s*\\bnot\\b\\s+", expr, ignore.case = TRUE)) {
    inner <- sub("^\\s*\\bnot\\b\\s+", "", expr, ignore.case = TRUE)
    return(ast_unary("not", parse_gmpl_expr(inner, symbols, depth + 1, max_depth)))
  }

  # Parentheses
  if (startsWith(expr, "(") && endsWith(expr, ")") && matching_paren(expr) == nchar(expr)) {
    inner <- substring(expr, 2, nchar(expr) - 1)
    result <- parse_gmpl_expr(inner, symbols, depth + 1, max_depth)
    # Mark that this expression was explicitly parenthesized
    result$brackets <- TRUE
    return(result)
  }

  # Conditional: if condition then expr1 else expr2
  if (grepl("\\bif\\b", expr, ignore.case = TRUE)) {
    result <- parse_conditional(expr, symbols, depth, max_depth)
    if (!is.null(result)) return(result)
  }

  # Aggregation functions: sum, prod, min, max
  for (func in c("sum", "prod", "min", "max")) {
    pattern <- paste0("^", func, "\\s*\\{")
    if (grepl(pattern, expr, ignore.case = TRUE)) {
      result <- parse_indexed_func(expr, func, symbols, depth, max_depth)
      if (!is.null(result)) return(result)
    }
  }

  # Regular functions: abs, ceil, floor, exp, log, sqrt, sin, cos, etc.
  if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\s*\\(", expr)) {
    result <- parse_function_call(expr, symbols, depth, max_depth)
    if (!is.null(result)) return(result)
  }

  # Array/parameter access: name[index1, index2, ...]
  if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\s*\\[", expr)) {
    result <- parse_indexed_access(expr, symbols, depth, max_depth)
    if (!is.null(result)) return(result)
  }

  # Simple symbol (variable, parameter, set element)
  if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", expr)) {
    return(gmpl_symbol_node(expr, symbols))
  }

  # If we get here, couldn't parse
  warning("Could not parse GMPL expression: \n", expr)
  # "\n", substr(expr, 1, 50))
  return(ast_symbol(paste0("UNPARSED_", substr(gsub("[^a-zA-Z0-9]", "_", expr), 1, 20))))
}

#' Check if expression has operator at top level (outside parentheses/brackets)
#'
#' @param expr Character string
#' @param op_pattern Regular expression pattern for operator
#' @return Logical
#' @keywords internal
has_top_level_op <- function(expr, op_pattern) {
  depth <- 0
  chars <- strsplit(expr, "")[[1]]
  i <- 1
  while (i <= length(chars)) {
    ch <- chars[i]
    if (ch == "(" || ch == "[" || ch == "{") {
      depth <- depth + 1
    } else if (ch == ")" || ch == "]" || ch == "}") {
      depth <- depth - 1
    } else if (depth == 0) {
      # For word operators (and, or, mod, div, not), check we're not in middle of identifier
      if (grepl("\\\\b", op_pattern)) {
        # Check previous character - if it's alphanumeric or underscore, we're in an identifier
        if (i > 1) {
          prev_ch <- chars[i - 1]
          if (grepl("[a-zA-Z0-9_]", prev_ch)) {
            i <- i + 1
            next
          }
        }
      }

      # Check if pattern matches at this position
      remaining <- paste0(chars[i:length(chars)], collapse = "")
      match <- regexpr(paste0("^", op_pattern), remaining, perl = TRUE)
      if (match > 0) {
        return(TRUE)
      }
    }
    i <- i + 1
  }
  FALSE
}

#' Split expression by operator at top level
#'
#' @param expr Character string
#' @param op_pattern Regular expression pattern for operator
#' @param rightmost Logical; if TRUE, split at rightmost occurrence (for right-associative operators)
#' @return List with lhs and rhs
#' @keywords internal
split_by_op <- function(expr, op_pattern, rightmost = FALSE) {
  depth <- 0
  chars <- strsplit(expr, "")[[1]]
  positions <- integer()
  lengths <- integer()

  i <- 1
  while (i <= length(chars)) {
    ch <- chars[i]
    if (ch == "(" || ch == "[" || ch == "{") {
      depth <- depth + 1
    } else if (ch == ")" || ch == "]" || ch == "}") {
      depth <- depth - 1
    } else if (depth == 0) {
      # For word operators (and, or, mod, div, not), check we're not in middle of identifier
      if (grepl("\\\\b", op_pattern)) {
        # Check previous character - if it's alphanumeric or underscore, we're in an identifier
        if (i > 1) {
          prev_ch <- chars[i - 1]
          if (grepl("[a-zA-Z0-9_]", prev_ch)) {
            i <- i + 1
            next
          }
        }
      }

      remaining <- paste0(chars[i:length(chars)], collapse = "")
      match <- regexpr(paste0("^", op_pattern), remaining, perl = TRUE)
      if (match > 0) {
        match_len <- attr(match, "match.length")
        positions <- c(positions, i)
        lengths <- c(lengths, match_len)
      }
    }
    i <- i + 1
  }

  if (length(positions) == 0) {
    return(list(lhs = NULL, rhs = expr))
  }

  # Choose split position (leftmost for left-associative, rightmost for right-associative)
  split_idx <- if (rightmost) length(positions) else 1
  split_pos <- positions[split_idx]
  split_len <- lengths[split_idx]

  lhs <- substring(expr, 1, split_pos - 1)
  rhs <- substring(expr, split_pos + split_len)

  list(lhs = lhs, rhs = rhs)
}

#' Find matching closing parenthesis
#'
#' @param expr Character string starting with opening paren
#' @return Position of matching closing paren, or NA if not found
#' @keywords internal
matching_paren <- function(expr) {
  if (!startsWith(expr, "(")) return(NA)

  depth <- 0
  chars <- strsplit(expr, "")[[1]]
  for (i in seq_along(chars)) {
    if (chars[i] == "(") depth <- depth + 1
    if (chars[i] == ")") depth <- depth - 1
    if (depth == 0) return(i)
  }
  NA
}


#' Parse conditional expression (if-then-else)
#'
#' @param expr Character string
#' @param symbols Symbol table
#' @param depth Current depth
#' @param max_depth Maximum depth
#' @return AST node or NULL
#' @keywords internal
parse_conditional <- function(expr, symbols, depth, max_depth) {
  # Pattern: if condition then expr1 else expr2
  # Need to find 'if', 'then', 'else' at depth 0

  if_pos <- NULL
  then_pos <- NULL
  else_pos <- NULL

  depth_level <- 0
  chars <- strsplit(expr, "")[[1]]
  i <- 1

  while (i <= length(chars)) {
    ch <- chars[i]
    if (ch == "(" || ch == "[" || ch == "{") {
      depth_level <- depth_level + 1
    } else if (ch == ")" || ch == "]" || ch == "}") {
      depth_level <- depth_level - 1
    } else if (depth_level == 0) {
      remaining <- paste0(chars[i:length(chars)], collapse = "")
      if (is.null(if_pos) && grepl("^\\s*\\bif\\b", remaining, ignore.case = TRUE)) {
        match <- regexpr("^\\s*\\bif\\b", remaining, ignore.case = TRUE, perl = TRUE)
        if_pos <- i + attr(match, "match.length")
      } else if (!is.null(if_pos) && is.null(then_pos) &&
                 grepl("^\\s*\\bthen\\b", remaining, ignore.case = TRUE)) {
        then_pos <- i
        match <- regexpr("^\\s*\\bthen\\b", remaining, ignore.case = TRUE, perl = TRUE)
        i <- i + attr(match, "match.length") - 1
      } else if (!is.null(then_pos) && is.null(else_pos) &&
                 grepl("^\\s*\\belse\\b", remaining, ignore.case = TRUE)) {
        else_pos <- i
      }
    }
    i <- i + 1
  }

  if (!is.null(if_pos) && !is.null(then_pos) && !is.null(else_pos)) {
    condition <- substring(expr, if_pos, then_pos - 1)
    then_expr <- substring(expr, then_pos + 4, else_pos - 1)  # +4 for "then"
    else_expr <- substring(expr, else_pos + 4)  # +4 for "else"

    return(ast_conditional(
      condition = parse_gmpl_expr(trimws(condition), symbols, depth + 1, max_depth),
      then_expr = parse_gmpl_expr(trimws(then_expr), symbols, depth + 1, max_depth),
      else_expr = parse_gmpl_expr(trimws(else_expr), symbols, depth + 1, max_depth)
    ))
  }

  NULL
}

#' Parse indexed aggregation function (sum, prod, min, max)
#'
#' @param expr Character string
#' @param func Function name
#' @param symbols Symbol table
#' @param depth Current depth
#' @param max_depth Maximum depth
#' @return AST node or NULL
#' @keywords internal
parse_indexed_func <- function(expr, func, symbols, depth, max_depth) {
  # Pattern: func{indexing} expression
  # Example: sum{r in REGION, y in YEAR} TotalCost[r,y]

  pattern <- paste0("^", func, "\\s*\\{")
  match_start <- regexpr(pattern, expr, ignore.case = TRUE, perl = TRUE)
  if (match_start != 1) return(NULL)

  # Find matching }
  brace_start <- regexpr("\\{", expr)[[1]]
  depth_level <- 0
  chars <- strsplit(expr, "")[[1]]
  brace_end <- NA

  for (i in brace_start:length(chars)) {
    if (chars[i] == "{") depth_level <- depth_level + 1
    if (chars[i] == "}") {
      depth_level <- depth_level - 1
      if (depth_level == 0) {
        brace_end <- i
        break
      }
    }
  }

  if (is.na(brace_end)) {
    warning("Unmatched braces in indexed function: ", substr(expr, 1, 50))
    return(NULL)
  }

  indexing <- substring(expr, brace_start + 1, brace_end - 1)
  value_expr <- trimws(substring(expr, brace_end + 1))

  # Parse indexing with optional condition: "r in REGION, y in YEAR : condition"
  # Split by colon at depth 0 to separate indices from condition
  colon_pos <- NULL
  depth_level <- 0
  chars_idx <- strsplit(indexing, "")[[1]]
  for (i in seq_along(chars_idx)) {
    ch <- chars_idx[i]
    if (ch %in% c("(", "[", "{")) {
      depth_level <- depth_level + 1
    } else if (ch %in% c(")", "]", "}")) {
      depth_level <- depth_level - 1
    } else if (ch == ":" && depth_level == 0) {
      colon_pos <- i
      break
    }
  }

  indices_str <- if (!is.null(colon_pos)) {
    substring(indexing, 1, colon_pos - 1)
  } else {
    indexing
  }

  condition_str <- if (!is.null(colon_pos)) {
    substring(indexing, colon_pos + 1)
  } else {
    NULL
  }

  # Parse index specifications: "r in REGION, y in YEAR"
  index_parts <- strsplit(indices_str, ",")[[1]]
  index_list <- list()

  for (part in index_parts) {
    part <- trimws(part)
    # Pattern: "name in set" or just "name"
    if (grepl("\\bin\\b", part, ignore.case = TRUE)) {
      match <- regexec("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s+in\\s+(.+)$", part, ignore.case = TRUE)[[1]]
      if (match[1] > 0) {
        captures <- regmatches(part, regexec("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s+in\\s+(.+)$", part, ignore.case = TRUE))[[1]]
        idx_name <- captures[2]
        set_expr <- captures[3]
        # Store as mapping: r = REGION
        index_list[[idx_name]] <- parse_gmpl_expr(set_expr, symbols, depth + 1, max_depth)
      }
    } else {
      # Just a name (implicit set)
      idx_name <- part
      index_list[[idx_name]] <- ast_symbol(idx_name)
    }
  }

  # Create ast_dims from the named list
  index_ast <- do.call(ast_dims, index_list)

  value_ast <- parse_gmpl_expr(value_expr, symbols, depth + 1, max_depth)

  # If there's a condition, wrap the value in ast_when
  if (!is.null(condition_str)) {
    condition_ast <- parse_gmpl_expr(trimws(condition_str), symbols, depth + 1, max_depth)
    value_ast <- ast_when(condition = condition_ast, then = value_ast, otherwise = ast_constant(0))
  }

  # Return appropriate AST node
  switch(func,
         sum = ast_sum(index = index_ast, value = value_ast),
         prod = ast_prod(index = index_ast, value = value_ast),
         min = ast_setmin(index = index_ast, value = value_ast),
         max = ast_setmax(index = index_ast, value = value_ast),
         stop("Unknown indexed function: ", func)
  )
}

#' Parse function call
#'
#' @param expr Character string
#' @param symbols Symbol table
#' @param depth Current depth
#' @param max_depth Maximum depth
#' @return AST node or NULL
#' @keywords internal
parse_function_call <- function(expr, symbols, depth, max_depth) {
  # Pattern: func_name(arg1, arg2, ...)
  match <- regexec("^([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\((.*)\\)$", expr)[[1]]
  if (match[1] == -1) return(NULL)

  captures <- regmatches(expr, regexec("^([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\((.*)\\)$", expr))[[1]]
  func_name <- captures[2]
  args_str <- captures[3]

  # Parse arguments (split by comma at depth 0)
  args_list <- list()
  if (nchar(trimws(args_str)) > 0) {
    depth_level <- 0
    chars <- strsplit(args_str, "")[[1]]
    arg_start <- 1

    for (i in seq_along(chars)) {
      ch <- chars[i]
      if (ch == "(" || ch == "[" || ch == "{") {
        depth_level <- depth_level + 1
      } else if (ch == ")" || ch == "]" || ch == "}") {
        depth_level <- depth_level - 1
      } else if (ch == "," && depth_level == 0) {
        arg <- substring(args_str, arg_start, i - 1)
        args_list[[length(args_list) + 1]] <- parse_gmpl_expr(trimws(arg), symbols, depth + 1, max_depth)
        arg_start <- i + 1
      }
    }
    # Last argument
    arg <- substring(args_str, arg_start)
    args_list[[length(args_list) + 1]] <- parse_gmpl_expr(trimws(arg), symbols, depth + 1, max_depth)
  }

  # For single-argument standard functions, use ast_func
  # For others, use ast_call
  standard_funcs <- c("abs", "ceil", "floor", "exp", "log", "log10", "sqrt",
                      "sin", "cos", "tan", "asin", "acos", "atan")

  if (func_name %in% standard_funcs && length(args_list) == 1) {
    return(ast_func(func_name, args_list[[1]]))
  } else {
    return(ast_call(func_name, args_list))
  }
}

#' Parse indexed access (array/parameter subscript)
#'
#' @param expr Character string
#' @param symbols Symbol table
#' @param depth Current depth
#' @param max_depth Maximum depth
#' @return AST node or NULL
#' @keywords internal
parse_indexed_access <- function(expr, symbols, depth, max_depth) {
  # Pattern: name[index1, index2, ...]
  match <- regexec("^([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\[(.*)\\]$", expr)[[1]]
  if (match[1] == -1) return(NULL)

  captures <- regmatches(expr, regexec("^([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\[(.*)\\]$", expr))[[1]]
  name <- captures[2]
  indices_str <- captures[3]

  # Parse indices (split by comma at depth 0)
  indices_list <- list()
  if (nchar(trimws(indices_str)) > 0) {
    depth_level <- 0
    chars <- strsplit(indices_str, "")[[1]]
    idx_start <- 1

    for (i in seq_along(chars)) {
      ch <- chars[i]
      if (ch == "(" || ch == "[" || ch == "{") {
        depth_level <- depth_level + 1
      } else if (ch == ")" || ch == "]" || ch == "}") {
        depth_level <- depth_level - 1
      } else if (ch == "," && depth_level == 0) {
        idx <- substring(indices_str, idx_start, i - 1)
        indices_list[[length(indices_list) + 1]] <- parse_gmpl_expr(trimws(idx), symbols, depth + 1, max_depth)
        idx_start <- i + 1
      }
    }
    # Last index
    idx <- substring(indices_str, idx_start)
    indices_list[[length(indices_list) + 1]] <- parse_gmpl_expr(trimws(idx), symbols, depth + 1, max_depth)
  }

  # Return parameter/variable access node
  # Use symbol with dims attribute for indexed access
  result <- gmpl_symbol_node(name, symbols)
  if (length(indices_list) > 0) {
    # Strip any names to avoid nested structure issues
    names(indices_list) <- NULL
    result$dims <- do.call(ast_dims, indices_list)
  }
  result
}


#' Parse GMPL Equation to Multimod Equation
#'
#' Convert model_structure equation to multimod equation format
#'
#' @param eqn_info List containing equation information (name, body, sense, dims, condition)
#' @param symbols Symbol table for context
#' @return multimod equation object
#' @keywords internal
parse_gmpl_equation_to_ast <- function(eqn_info, symbols = list()) {
  # Parse the body (which contains both LHS and RHS)
  body <- eqn_info[['body']]
  sense <- eqn_info[['sense']]

  if (is.null(body) || nchar(trimws(body)) == 0) {
    stop("Empty equation body for: ", eqn_info[['name']])
  }

  # Convert GMPL sense to multimod relation
  # Note: objectives have sense="minimize"/"maximize" but body is already "var = expr"
  # so we parse them as regular equations with "==" relation
  is_objective <- sense %in% c("minimize", "maximize")

  if (is_objective) {
    # For objectives, the body is "var = expr", so we use "=" as the sense_pattern
    relation <- "=="
    sense_pattern <- "="
  } else {
    # For constraints, convert GMPL sense to multimod relation
    relation <- switch(sense,
                       "=" = "==",
                       "<=" = "<=",
                       ">=" = ">=",
                       stop("Unknown equation sense: ", sense)
    )
    sense_pattern <- switch(sense,
                            "=" = "=",
                            "<=" = "<=",
                            ">=" = ">=",
                            stop("Unknown equation sense: ", sense)
    )
  }

  # Find the sense operator at depth 0
  # For equations with if-then-else in the LHS, the sense operator will be the LAST
  # occurrence at depth 0, not the first. So we scan the entire body and keep the last match.
  depth <- 0
  chars <- strsplit(body, "")[[1]]
  split_pos <- NULL  # Will hold the position of the LAST sense operator found

  i <- 1
  while (i <= length(chars)) {
    ch <- chars[i]
    if (ch == "(" || ch == "[" || ch == "{") {
      depth <- depth + 1
    } else if (ch == ")" || ch == "]" || ch == "}") {
      depth <- depth - 1
    } else if (depth == 0) {
      remaining <- paste0(chars[i:length(chars)], collapse = "")
      match <- regexpr(paste0("^", sense_pattern), remaining, perl = TRUE)
      if (match > 0) {
        split_pos <- i  # Update to this position (will keep the last one)
        # DON'T break - keep looking for later occurrences
      }
    }
    i <- i + 1
  }

  if (is.null(split_pos)) {
    stop("Could not find sense operator '", sense_pattern, "' in equation: ", eqn_info[['name']])
  }

  lhs_str <- substring(body, 1, split_pos - 1)
  # Skip the actual matched pattern, not the original sense
  rhs_str <- substring(body, split_pos + nchar(sense_pattern))

  # Parse both sides
  lhs_ast <- parse_gmpl_expr(trimws(lhs_str), symbols)
  rhs_ast <- parse_gmpl_expr(trimws(rhs_str), symbols)

  # Parse domain/condition if present
  domain <- if (!is.null(eqn_info[['condition']]) && nzchar(eqn_info[['condition']])) {
    parse_gmpl_expr(eqn_info[['condition']], symbols)
  } else {
    NULL
  }

  # Return equation using new_equation constructor
  new_equation(
    name = eqn_info[['name']],
    desc = eqn_info[['desc']],
    dims = eqn_info[['dims']],
    lhs = lhs_ast,
    rhs = rhs_ast,
    relation = relation,
    domain = domain,
    dims_index_aliases = eqn_info[['dims_index_aliases']]
  )
}

#' Parse if-then-else expression
#'
#' Extracts condition, then_expr, and else_expr from GMPL if-then-else syntax.
#' Only matches if keywords are at depth 0 (not inside parentheses/brackets).
#'
#' @param expr Character string containing the expression
#' @return List with condition, then_expr, else_expr (or NULL if not valid if-then-else)
#' @keywords internal
parse_if_then_else_expr <- function(expr) {
  # Find 'if', 'then', 'else' keywords at depth 0
  # if (grepl("else if", expr)) browser()
  chars <- strsplit(expr, "")[[1]]
  depth <- 0
  if_pos <- then_pos <- else_pos <- NULL

  i <- 1
  while (i <= length(chars)) {
    ch <- chars[i]

    # Track depth
    if (ch %in% c("(", "[", "{")) {
      depth <- depth + 1
      i <- i + 1
      next
    } else if (ch %in% c(")", "]", "}")) {
      depth <- depth - 1
      i <- i + 1
      next
    } else if (depth == 0) {
      # Check for keywords at depth 0
      remaining <- paste0(chars[i:length(chars)], collapse = "")

      # Check for 'if' keyword
      if (is.null(if_pos) && grepl("^if\\b", remaining)) {
        # Verify it's not inside an identifier
        prev_ch <- if (i > 1) chars[i - 1] else ""
        if (!grepl("[A-Za-z0-9_]", prev_ch)) {
          if_pos <- i
        }
      }

      # Check for 'then' keyword (only after finding 'if')
      if (!is.null(if_pos) && is.null(then_pos) && grepl("^then\\b", remaining)) {
        prev_ch <- if (i > 1) chars[i - 1] else ""
        if (!grepl("[A-Za-z0-9_]", prev_ch)) {
          then_pos <- i
        }
      }

      # Check for 'else' keyword (only after finding 'then')
      if (!is.null(then_pos) && is.null(else_pos) && grepl("^else\\b", remaining)) {
        prev_ch <- if (i > 1) chars[i - 1] else ""
        if (!grepl("[A-Za-z0-9_]", prev_ch)) {
          else_pos <- i
        }
      }
    }

    i <- i + 1
  }

  # Validate we found at least if and then
  if (is.null(if_pos) || is.null(then_pos)) {
    return(NULL)
  }

  # Extract parts
  # Condition: from after 'if' to before 'then'
  condition_start <- if_pos + 2  # Skip 'if'
  condition_end <- then_pos - 1
  condition <- trimws(paste0(chars[condition_start:condition_end], collapse = ""))

  # Then expression: from after 'then' to before 'else' (or end)
  then_start <- then_pos + 4  # Skip 'then'
  then_end <- if (!is.null(else_pos)) else_pos - 1 else length(chars)
  then_expr <- trimws(paste0(chars[then_start:then_end], collapse = ""))

  # Else expression: from after 'else' to end (if present)
  else_expr <- if (!is.null(else_pos)) {
    else_start <- else_pos + 4  # Skip 'else'
    trimws(paste0(chars[else_start:length(chars)], collapse = ""))
  } else {
    NULL
  }

  # Determine end position of entire if-then-else expression
  # This is the end of the else expression (if present) or then expression
  end_pos <- if (!is.null(else_pos)) {
    length(chars)  # Entire string if there's an else
  } else {
    then_end  # End of then expression if no else
  }

  list(
    condition = condition,
    then_expr = then_expr,
    else_expr = else_expr,
    end_pos = end_pos  # Character position where if-then-else ends
  )
}

# Data ####
#' Read GMPL data file and parse its contents
#'
#' Parses a .dat file to extract sets, mappings, and parameters with their values.
#' Returns a structured list with metadata about each element.
#'
#' @param file Path to .dat file, or character vector containing data file lines
#' @return List with elements: sets, mappings, parameters. Each element contains
#'   name, type ("set", "mapping", "parameter"), dims, data, and default value.
#'
#' @export
read_gmpl_data <- function(file) {
  # Handle both file paths and character vectors
  if (length(file) == 1 && file.exists(file)) {
    # It's a file path
    lines <- readLines(file, warn = FALSE)
  } else {
    # It's already file contents (character vector)
    lines <- file
  }

  # Initialize result structure
  result <- list(
    sets = list(),
    mappings = list(),
    parameters = list()
  )

  i <- 1
  while (i <= length(lines)) {
    line <- trimws(lines[i])

    # Skip empty lines and comments
    if (nchar(line) == 0 || startsWith(line, "#")) {
      i <- i + 1
      next
    }

    # Check for set declaration
    if (grepl("^set\\s+", line)) {
      parsed <- parse_set_declaration(lines, i)

      if (parsed$dims == 1) {
        # Simple set
        result$sets[[parsed$name]] <- parsed
      } else {
        # Multi-dimensional set (mapping)
        result$mappings[[parsed$name]] <- parsed
      }

      i <- parsed$next_line
      next
    }

    # Check for parameter declaration
    if (grepl("^param\\s+", line)) {
      parsed <- parse_param_declaration(lines, i)
      result$parameters[[parsed$name]] <- parsed
      i <- parsed$next_line
      next
    }

    # Check for end statement
    if (grepl("^end\\s*;", line)) {
      break
    }

    i <- i + 1
  }

  result
}


#' Read GMPL declarations sequentially
#'
#' Scans a .dat file and returns every `set` or `param` declaration in the
#' original order they appear. Each entry keeps the parsed metadata while adding
#' `kind` (the keyword used in the file), `order` (1-based position), and the
#' raw declaration lines for reference. This avoids re-grouping declarations
#' into sets/mappings/parameters buckets.
#'
#' @param file Path to .dat file
#' @return List where each element represents a declaration with metadata,
#'   parsed content, and raw text
#' @export
read_gmpl_declarations <- function(file) {
  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  lines <- readLines(file, warn = FALSE)
  declarations <- list()

  i <- 1
  order_idx <- 1
  while (i <= length(lines)) {
    line <- trimws(lines[i])

    if (nchar(line) == 0 || startsWith(line, "#")) {
      i <- i + 1
      next
    }

    if (grepl("^end\\s*;", line, ignore.case = TRUE)) {
      break
    }

    if (grepl("^set\\s+", line, ignore.case = TRUE)) {
      parsed <- parse_set_declaration(lines, i)
      entry <- parsed
      entry$kind <- "set"
      entry$order <- order_idx
      entry$raw_lines <- trimws(lines[i:(parsed$next_line - 1)])
      declarations[[order_idx]] <- entry
      order_idx <- order_idx + 1
      i <- parsed$next_line
      next
    }

    if (grepl("^param\\s+", line, ignore.case = TRUE)) {
      parsed <- parse_param_declaration(lines, i)
      entry <- parsed
      entry$kind <- "param"
      entry$order <- order_idx
      entry$raw_lines <- trimws(lines[i:(parsed$next_line - 1)])
      declarations[[order_idx]] <- entry
      order_idx <- order_idx + 1
      i <- parsed$next_line
      next
    }

    i <- i + 1
  }

  declarations
}


#' Extract the data block body for a declaration ending with a semicolon
#'
#' @param lines All lines from the file
#' @param start_line Line number where the declaration starts
#' @return List with data_lines (character vector), header (text between : and :=),
#'   and next_line (first line after the terminating semicolon)
#'
#' @keywords internal
extract_data_block <- function(lines, start_line) {
  line <- trimws(lines[start_line])
  data_lines <- character()
  header <- ""
  remainder <- ""

  # Check if line has table format (: header := data)
  if (grepl(":\\s*[^:]+:=", line)) {
    # Extract header (between : and :=)
    header <- sub("^[^:]*:\\s*([^:]+):=.*$", "\\1", line, perl = TRUE)
    header <- trimws(header)
    # Extract data after :=
    remainder <- sub(".*:=\\s*", "", line, perl = TRUE)
  } else if (grepl(":=", line, fixed = TRUE)) {
    # Simple := without table header
    remainder <- sub(".*:=\\s*", "", line, perl = TRUE)
  }

  has_semicolon <- FALSE
  if (nchar(remainder) > 0) {
    has_semicolon <- grepl(";", remainder, fixed = TRUE)
    cleaned <- trimws(sub(";.*$", "", remainder))
    if (nchar(cleaned) > 0) {
      data_lines <- c(data_lines, cleaned)
    }
  }
  i <- start_line + 1
  while (!has_semicolon && i <= length(lines)) {
    current <- trimws(lines[i])
    has_semicolon <- grepl(";", current, fixed = TRUE)
    cleaned <- trimws(sub(";.*$", "", current))
    if (nchar(cleaned) > 0) {
      data_lines <- c(data_lines, cleaned)
    }
    i <- i + 1
  }
  list(
    header = header,
    data_lines = data_lines,
    next_line = if (has_semicolon) i else length(lines) + 1
  )
}


#' Parse set declaration from GMPL data file
#'
#' @param lines All lines from the file
#' @param start_line Line number where set declaration starts
#' @return List with name, type, dims, data, next_line
#'
#' @keywords internal
parse_set_declaration <- function(lines, start_line) {
  line <- trimws(lines[start_line])

  # Extract set name: "set NAME :="
  match <- regexpr("set\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*:=", line, perl = TRUE)
  if (match == -1) {
    stop("Cannot parse set declaration at line ", start_line, ": ", line)
  }

  name <- sub("set\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*:=.*", "\\1", line, perl = TRUE)
  block <- extract_data_block(lines, start_line)
  data_lines <- block$data_lines

  if (length(data_lines) == 0) {
    # Empty set
    return(list(
      name = name,
      type = "set",
      dims = 1,
      data = character(),
      next_line = block$next_line
    ))
  }

  parse_tuple_line <- function(line) {
    cleaned <- gsub(",\\s*$", "", line)
    parts <- trimws(strsplit(cleaned, ",")[[1]])
    parts[nchar(parts) > 0]
  }
  has_commas <- any(grepl(",", data_lines, fixed = TRUE))
  if (has_commas) {
    tuples <- lapply(data_lines, parse_tuple_line)
    dims <- max(vapply(tuples, length, integer(1)))
    tuples <- lapply(tuples, function(parts) {
      if (length(parts) < dims) {
        parts <- c(parts, rep(NA_character_, dims - length(parts)))
      }
      parts[seq_len(dims)]
    })
    data_matrix <- do.call(rbind, tuples)
    if (dims == 1) {
      set_data <- as.vector(data_matrix[, 1], mode = "character")
      return(list(
        name = name,
        type = "set",
        dims = 1,
        data = set_data,
        next_line = block$next_line
      ))
    }
    data_df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
    colnames(data_df) <- paste0("dim", seq_len(dims))
    return(list(
      name = name,
      type = "mapping",
      dims = dims,
      data = data_df,
      next_line = block$next_line
    ))
  }
  # Fallback: whitespace separated values interpreted as 1-D set
  tokens <- unlist(strsplit(paste(data_lines, collapse = " "), "\\s+"))
  tokens <- tokens[nchar(tokens) > 0]
  list(
    name = name,
    type = "set",
    dims = 1,
    data = tokens,
    next_line = block$next_line
  )
}


#' Parse parameter declaration from GMPL data file
#'
#' @param lines All lines from the file
#' @param start_line Line number where parameter declaration starts
#' @return List with name, type, dims, default, data, comment, next_line
#'
#' @keywords internal
parse_param_declaration <- function(lines, start_line) {
  line <- trimws(lines[start_line])

  # Extract preceding comments (look back up to 10 lines)
  comment_lines <- character()
  desc <- ""
  for (i in (start_line-1):max(1, start_line-10)) {
    if (i < 1 || i > length(lines)) break
    prev_line <- trimws(lines[i])
    if (startsWith(prev_line, "#")) {
      clean_line <- sub("^#\\s*", "", prev_line)
      comment_lines <- c(clean_line, comment_lines)
      # Last comment line before declaration becomes desc
      if (i == start_line - 1 && nchar(clean_line) > 0) {
        desc <- clean_line
      }
    } else if (nchar(prev_line) > 0) {
      # Stop at first non-comment, non-empty line
      break
    }
  }
  comment <- paste(comment_lines, collapse = "\n")

  # Extract units from comment (pattern: "Units: XXX")
  units <- ""
  units_match <- regexpr("Units:\\s*([^\\s]+)", comment, perl = TRUE, ignore.case = TRUE)
  if (units_match > 0) {
    units <- sub(".*Units:\\s*([^\\s]+).*", "\\1", comment, perl = TRUE, ignore.case = TRUE)
  }

  # Extract parameter name
  match <- regexpr("param\\s+([A-Za-z_][A-Za-z0-9_]*)", line, perl = TRUE)
  if (match == -1) {
    stop("Cannot parse parameter declaration at line ", start_line, ": ", line)
  }

  name <- sub("param\\s+([A-Za-z_][A-Za-z0-9_]*).*", "\\1", line, perl = TRUE)

  # Extract default value if present
  default_val <- 0
  if (grepl("default\\s+", line)) {
    default_match <- regexpr("default\\s+([0-9.eE+-]+)", line, perl = TRUE)
    if (default_match != -1) {
      default_str <- sub(".*default\\s+([0-9.eE+-]+).*", "\\1", line, perl = TRUE)
      default_val <- as.numeric(default_str)
    }
  }

  block <- extract_data_block(lines, start_line)
  data_lines <- block$data_lines
  header <- block$header

  if (length(data_lines) == 0 && nchar(header) == 0) {
    return(list(
      name = name,
      type = "parameter",
      dims = 0,
      defVal = default_val,
      data = data.frame(),
      desc = desc,
      comment = comment,
      units = units,
      next_line = block$next_line
    ))
  }

  # Detect format and parse accordingly
  block_text <- paste(data_lines, collapse = " ")

  # Format 1: Sliced table format [fixed1,fixed2,*,...] : cols := rows values
  if (grepl("^\\[", block_text)) {
    result <- parse_sliced_table_format(name, header, block_text, default_val)
    result$next_line <- block$next_line
    result$desc <- desc
    result$comment <- comment
    result$units <- units
    return(result)
  }

  # Format 2: Simple table format (has header with column names)
  if (nchar(header) > 0) {
    result <- parse_simple_table_format(name, header, block_text, default_val)
    result$next_line <- block$next_line
    result$desc <- desc
    result$comment <- comment
    result$units <- units
    return(result)
  }

  # Format 3: Bracketed tuple format [idx1,idx2] value [idx1,idx2] value
  if (grepl("\\[[^\\]]+\\]\\s+[0-9.eE+-]+", block_text)) {
    result <- parse_bracket_format(name, block_text, default_val)
    result$next_line <- block$next_line
    result$desc <- desc
    result$comment <- comment
    result$units <- units
    return(result)
  }

  # Format 4: Simple scalar assignment (just a number)
  if (grepl("^[0-9.eE+-]+\\s*$", block_text)) {
    value <- as.numeric(trimws(block_text))
    return(list(
      name = name,
      type = "parameter",
      dims = 0,
      defVal = default_val,
      data = data.frame(value = value),
      desc = desc,
      comment = comment,
      units = units,
      next_line = block$next_line
    ))
  }

  # Format 5: List format (idx value idx value ...)
  result <- parse_list_format(name, block_text, default_val)
  result$next_line <- block$next_line
  result$desc <- desc
  result$comment <- comment
  result$units <- units
  return(result)
}


#' Parse sliced table format with fixed dimensions and wildcards
#'
#' Parses GMPL sliced table format like `[fixed,*,*] : cols := rows values`.
#' Can have multiple sliced blocks in one parameter.
#'
#' @keywords internal
parse_sliced_table_format <- function(name, header, text, default_val) {
  # Multiple sliced blocks can appear: [slice1] : header := data [slice2] : header := data
  # Split by finding all slice block patterns

  all_rows <- list()

  # Find all slice blocks
  slice_pattern <- "\\[([^\\]]+)\\]\\s*:\\s*([^:]+):="
  slice_matches <- gregexpr(slice_pattern, text, perl = TRUE)

  if (slice_matches[[1]][1] < 0) {
    return(list(name = name, type = "parameter", dims = 0, defVal = default_val,
                data = data.frame()))
  }

  match_starts <- slice_matches[[1]]
  match_lengths <- attr(slice_matches[[1]], "match.length")

  # Process each sliced block
  for (idx in seq_along(match_starts)) {
    block_start <- match_starts[idx]
    block_end <- if (idx < length(match_starts)) match_starts[idx+1] - 1 else nchar(text)

    block_text <- substring(text, block_start, block_end)
    # Remove any leading whitespace or stray brackets
    block_text <- trimws(block_text)

    # Extract slice specification - more robust
    slice_str <- sub("^.*?\\[([^\\]]+)\\].*$", "\\1", block_text, perl = TRUE)
    slice_parts <- strsplit(slice_str, ",")[[1]]
    slice_parts <- trimws(slice_parts)

    # Extract column headers
    col_match <- regexpr(":\\s*([^:]+):=", block_text, perl = TRUE)
    col_text <- sub(".*:\\s*([^:]+):=.*", "\\1", block_text, perl = TRUE)
    col_names <- strsplit(trimws(col_text), "\\s+")[[1]]
    col_names <- col_names[nchar(col_names) > 0]

    # Extract data
    data_text <- sub(".*:=\\s*", "", block_text, perl = TRUE)
    data_tokens <- strsplit(trimws(data_text), "\\s+")[[1]]
    data_tokens <- data_tokens[nchar(data_tokens) > 0]

    if (length(data_tokens) == 0 || length(col_names) == 0) next

    # Parse table rows
    n_cols <- length(col_names)
    i <- 1

    while (i <= length(data_tokens)) {
      if (i + n_cols > length(data_tokens)) break

      row_name <- data_tokens[i]
      row_values <- data_tokens[(i+1):(i+n_cols)]

      # Create rows for each column
      for (j in seq_along(col_names)) {
        full_idx <- slice_parts
        wildcard_positions <- which(slice_parts == "*")

        if (length(wildcard_positions) == 2) {
          full_idx[wildcard_positions[1]] <- row_name
          full_idx[wildcard_positions[2]] <- col_names[j]
        } else if (length(wildcard_positions) == 1) {
          full_idx[wildcard_positions[1]] <- col_names[j]
        }

        all_rows[[length(all_rows) + 1]] <- c(full_idx, as.numeric(row_values[j]))
      }

      i <- i + n_cols + 1
    }
  }

  if (length(all_rows) == 0) {
    return(list(name = name, type = "parameter", dims = 0, defVal = default_val,
                data = data.frame()))
  }

  n_dims <- length(all_rows[[1]]) - 1
  data_matrix <- do.call(rbind, all_rows)
  data_df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
  colnames(data_df) <- c(paste0("dim", seq_len(n_dims)), "value")
  data_df$value <- as.numeric(data_df$value)

  list(
    name = name,
    type = "parameter",
    dims = n_dims,
    defVal = default_val,
    data = data_df
  )
}


#' Parse simple table format: : cols := rows values
#'
#' @keywords internal
parse_simple_table_format <- function(name, header, text, default_val) {
  # Column headers from extracted header
  col_names <- strsplit(trimws(header), "\\s+")[[1]]
  col_names <- col_names[nchar(col_names) > 0]

  # Extract data tokens
  data_tokens <- strsplit(trimws(text), "\\s+")[[1]]
  data_tokens <- data_tokens[nchar(data_tokens) > 0]

  if (length(data_tokens) == 0 || length(col_names) == 0) {
    return(list(name = name, type = "parameter", dims = 0, defVal = default_val,
                data = data.frame()))
  }

  # Parse rows: row_name value1 value2 ... valueN
  n_cols <- length(col_names)
  rows_list <- list()
  i <- 1

  while (i <= length(data_tokens)) {
    if (i + n_cols > length(data_tokens)) break

    row_name <- data_tokens[i]
    row_values <- data_tokens[(i+1):(i+n_cols)]

    # Create a row for each column value
    for (j in seq_along(col_names)) {
      rows_list[[length(rows_list) + 1]] <- data.frame(
        dim1 = row_name,
        dim2 = col_names[j],
        value = as.numeric(row_values[j]),
        stringsAsFactors = FALSE
      )
    }

    i <- i + n_cols + 1
  }

  if (length(rows_list) == 0) {
    return(list(name = name, type = "parameter", dims = 2, defVal = default_val,
                data = data.frame()))
  }

  data_df <- do.call(rbind, rows_list)

  list(
    name = name,
    type = "parameter",
    dims = 2,
    defVal = default_val,
    data = data_df
  )
}


#' Parse bracket format for parameter values
#'
#' Parses GMPL bracket format like `[idx1,idx2] value [idx1,idx2] value`.
#'
#' @keywords internal
parse_bracket_format <- function(name, text, default_val) {
  # Find all [indices] value pairs
  pattern <- "\\[([^\\]]+)\\]\\s+([0-9.eE+-]+)"
  matches <- gregexpr(pattern, text, perl = TRUE)

  if (matches[[1]][1] < 0) {
    return(list(name = name, type = "parameter", dims = 0, defVal = default_val,
                data = data.frame()))
  }

  rows_list <- list()
  match_data <- regmatches(text, matches)[[1]]

  for (match_text in match_data) {
    # Extract indices
    idx_match <- regexpr("\\[([^\\]]+)\\]", match_text)
    idx_text <- regmatches(match_text, idx_match)
    idx_text <- gsub("[\\[\\]]", "", idx_text)
    indices <- strsplit(idx_text, ",")[[1]]
    indices <- trimws(indices)

    # Extract value
    value_match <- regexpr("[0-9.eE+-]+$", match_text)
    value <- as.numeric(regmatches(match_text, value_match))

    rows_list[[length(rows_list) + 1]] <- c(indices, value)
  }

  if (length(rows_list) == 0) {
    return(list(name = name, type = "parameter", dims = 0, defVal = default_val,
                data = data.frame()))
  }

  # Convert to data frame
  n_dims <- length(rows_list[[1]]) - 1
  data_matrix <- do.call(rbind, rows_list)
  data_df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
  colnames(data_df) <- c(paste0("dim", seq_len(n_dims)), "value")
  data_df$value <- as.numeric(data_df$value)

  list(
    name = name,
    type = "parameter",
    dims = n_dims,
    defVal = default_val,
    data = data_df
  )
}


#' Parse list format: idx value idx value
#'
#' @keywords internal
parse_list_format <- function(name, text, default_val) {
  # Remove quoted strings (symbolic parameters like ResultsPath := "results")
  # These should be ignored as we don't store string parameter values
  text_cleaned <- gsub('"[^"]*"', '', text)
  text_cleaned <- gsub("'[^']*'", '', text_cleaned)
  text_cleaned <- trimws(text_cleaned)

  # If nothing left after removing strings, return empty
  if (nchar(text_cleaned) == 0) {
    return(list(name = name, type = "parameter", dims = 1, defVal = default_val,
                data = data.frame()))
  }

  tokens <- strsplit(text_cleaned, "\\s+")[[1]]
  tokens <- tokens[nchar(tokens) > 0]

  if (length(tokens) == 0) {
    # Empty parameter (no data provided)
    return(list(name = name, type = "parameter", dims = 1, defVal = default_val,
                data = data.frame()))
  }

  if (length(tokens) %% 2 != 0) {
    warning("Parameter ", name, ": odd number of tokens in list format")
    return(list(name = name, type = "parameter", dims = 1, defVal = default_val,
                data = data.frame()))
  }

  rows_list <- list()
  for (i in seq(1, length(tokens), 2)) {
    if (i + 1 <= length(tokens)) {
      rows_list[[length(rows_list) + 1]] <- data.frame(
        dim1 = tokens[i],
        value = as.numeric(tokens[i + 1]),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows_list) == 0) {
    return(list(name = name, type = "parameter", dims = 1, defVal = default_val,
                data = data.frame()))
  }

  data_df <- do.call(rbind, rows_list)

  list(
    name = name,
    type = "parameter",
    dims = 1,
    defVal = default_val,
    data = data_df
  )
}


#' Import GMPL Data to Model
#'
#' Read a GMPL data file and populate a multimod model's sets and parameters.
#' This is more efficient for large models as it directly populates the model
#' and can save to CSV/arrow format.
#'
#' @param model A multimod model object
#' @param gmpl_data Path to GMPL data file, character vector of data file contents,
#'   or a list from read_gmpl_data()
#' @param model_path Optional path to save parameter data as CSV files (default: model's base_path)
#' @param format Data storage format: "memory" (default), "csv", or "arrow"
#' @param verbose Logical. If TRUE, print progress (default FALSE)
#'
#' @return Updated model object with populated sets and parameters
#' @export
#'
#' @examples
#' \dontrun{
#' # Read model structure
#' model <- read_gmpl("osemosys.txt", as_multimod = TRUE)
#'
#' # Import data from file
#' model <- import_gmpl_data(model, gmpl_data = "utopia.txt")
#'
#' # Import from character vector (e.g., package data)
#' data(example_models)
#' model <- import_gmpl_data(model, gmpl_data = example_models$OSeMOSYS$gmpl$data)
#'
#' # Or import pre-parsed data
#' data <- read_gmpl_data("utopia.txt")
#' model <- import_gmpl_data(model, gmpl_data = data)
#'
#' # Save to specific directory as CSV
#' model <- import_gmpl_data(model, gmpl_data = "utopia.txt",
#'                           model_path = "path/to/model/data",
#'                           format = "csv")
#' }
import_gmpl_data <- function(model,
                             gmpl_data,
                             model_path = NULL,
                             format = c("memory", "csv", "arrow"),
                             verbose = FALSE) {

  format <- match.arg(format)

  # Determine model path
  if (is.null(model_path)) {
    model_path <- model$base_path
    if (is.null(model_path)) {
      model_path <- tempdir()
      model$base_path <- model_path
      if (verbose) cat("Using temporary directory:", model_path, "\n")
    }
  }

  # Read data if needed
  if (is.character(gmpl_data)) {
    if (length(gmpl_data) == 1 && file.exists(gmpl_data)) {
      # Single file path
      if (verbose) cat("Reading GMPL data file:", gmpl_data, "\n")
      gmpl_data <- read_gmpl_data(gmpl_data)
    } else if (length(gmpl_data) > 1) {
      # Character vector (file contents)
      if (verbose) cat("Parsing GMPL data from character vector\n")
      gmpl_data <- read_gmpl_data(gmpl_data)
    } else {
      stop("gmpl_data file not found: ", gmpl_data)
    }
  }

  if (!is.list(gmpl_data)) {
    stop("gmpl_data must be a file path, character vector, or output from read_gmpl_data()")
  }

  if (verbose) cat("\nPopulating model with data...\n")

  # Populate sets
  if ("sets" %in% names(gmpl_data) && length(gmpl_data$sets) > 0) {
    if (verbose) cat("  Sets:", length(gmpl_data$sets), "\n")
    for (set_name in names(gmpl_data$sets)) {
      set_info <- gmpl_data$sets[[set_name]]
      if (set_name %in% names(model$sets)) {
        # Update existing set with data
        model$sets[[set_name]]$data <- set_info$data
        if (verbose) {
          cat("    ", set_name, ": ", length(set_info$data), " elements\n", sep = "")
        }
      } else {
        if (verbose) warning("Set '", set_name, "' in data file not found in model")
      }
    }
  }

  # Populate mappings (multi-dimensional sets)
  if ("mappings" %in% names(gmpl_data) && length(gmpl_data$mappings) > 0) {
    if (verbose) cat("  Mappings:", length(gmpl_data$mappings), "\n")
    for (mapping_name in names(gmpl_data$mappings)) {
      mapping_info <- gmpl_data$mappings[[mapping_name]]
      if (mapping_name %in% names(model$sets)) {
        model$sets[[mapping_name]]$data <- mapping_info$data
        if (verbose) {
          cat("    ", mapping_name, ": ", nrow(mapping_info$data), " tuples\n", sep = "")
        }
      } else {
        if (verbose) warning("Mapping '", mapping_name, "' in data file not found in model")
      }
    }
  }

  # Populate parameters
  if ("parameters" %in% names(gmpl_data) && length(gmpl_data$parameters) > 0) {
    if (verbose) cat("  Parameters:\n")
    for (param_name in names(gmpl_data$parameters)) {
      param_info <- gmpl_data$parameters[[param_name]]
      param_data <- param_info$data

      if (param_name %in% names(model$parameters)) {
        # Update metadata from data file

        # Update desc (short description) if available
        if (!is.null(param_info$desc) && nchar(param_info$desc) > 0) {
          if (is.null(model$parameters[[param_name]]$desc) ||
              nchar(model$parameters[[param_name]]$desc) == 0) {
            model$parameters[[param_name]]$desc <- param_info$desc
          }
        }

        # Update comment (full multi-line) if available
        if (!is.null(param_info$comment) && nchar(param_info$comment) > 0) {
          model$parameters[[param_name]]$comment <- param_info$comment
        }

        # Update units if available and field exists
        if (!is.null(param_info$units) && nchar(param_info$units) > 0) {
          # Add units field if it doesn't exist
          model$parameters[[param_name]]$units <- param_info$units
        }

        # Update default value ONLY if model doesn't already have one
        # (model file defaults take precedence over data file defaults)
        if (!is.null(param_info$defVal) && is.null(model$parameters[[param_name]]$defVal)) {
          model$parameters[[param_name]]$defVal <- param_info$defVal
        }

        # Convert dimension column names to actual set names if possible
        if (!is.null(param_data) && nrow(param_data) > 0) {
          param_obj <- model$parameters[[param_name]]
          if (!is.null(param_obj$dims) && length(param_obj$dims) > 0) {
            # Map generic dim1, dim2, etc. to actual dimension names
            model_dims <- get_dim_names(param_obj$dims)
            data_cols <- setdiff(names(param_data), "value")

            if (length(data_cols) == length(model_dims)) {
              # Rename columns to match model dimensions
              new_names <- c(model_dims, "value")
              names(param_data) <- new_names
            }
          }
        }

        # Save or store parameter data based on format
        if (format == "csv" && !is.null(model_path)) {
          param_dir <- file.path(model_path, param_name)
          dir.create(param_dir, recursive = TRUE, showWarnings = FALSE)
          csv_file <- file.path(param_dir, "data.csv")
          write.csv(param_data, csv_file, row.names = FALSE)
          model$parameters[[param_name]]$data <- csv_file
          if (verbose) {
            cat("    ", param_name, ": ", nrow(param_data), " rows -> ", csv_file, "\n", sep = "")
          }
        } else if (format == "arrow" && !is.null(model_path)) {
          if (requireNamespace("arrow", quietly = TRUE)) {
            param_dir <- file.path(model_path, param_name)
            dir.create(param_dir, recursive = TRUE, showWarnings = FALSE)
            arrow_file <- file.path(param_dir, "data.arrow")
            arrow::write_feather(param_data, arrow_file)
            model$parameters[[param_name]]$data <- arrow_file
            if (verbose) {
              cat("    ", param_name, ": ", nrow(param_data), " rows -> ", arrow_file, "\n", sep = "")
            }
          } else {
            warning("arrow package not available, using memory format")
            model$parameters[[param_name]]$data <- param_data
            if (verbose) {
              cat("    ", param_name, ": ", nrow(param_data), " rows (in memory)\n", sep = "")
            }
          }
        } else {
          # Store in memory
          model$parameters[[param_name]]$data <- param_data
          if (verbose) {
            cat("    ", param_name, ": ", nrow(param_data), " rows (in memory)\n", sep = "")
          }
        }

        # Update active_dims based on actual data columns
        if (!is.null(param_data) && nrow(param_data) > 0) {
          data_cols <- setdiff(names(param_data), "value")
          if (length(data_cols) > 0) {
            model$parameters[[param_name]]$active_dims <- do.call(
              ast_dims,
              as.list(data_cols)
            )
          }
        }
      } else {
        if (verbose) warning("Parameter '", param_name, "' in data file not found in model")
      }
    }
  }

  if (verbose) cat("\nData import complete.\n")

  return(model)
}

