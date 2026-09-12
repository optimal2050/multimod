#' Log solution loading from solver to variables directory
#'
#' Creates or appends to solution_log.txt with timestamp and details of which
#' solver's solution was loaded into the model's variables/ directory.
#'
#' @param model_dir Path to model root directory
#' @param solver_name Name of solver (e.g., "gmpl", "jump", "gams")
#' @param variables_loaded Character vector of variable names that were loaded
#' @param verbose Logical; print log entry to console (default: TRUE)
#' @return NULL (writes to log file as side effect)
#' @export
log_solution_load <- function(model_dir, solver_name, variables_loaded, verbose = TRUE) {

  # Create log entry
  log_file <- file.path(model_dir, "solution_log.txt")
  log_entry <- sprintf(
    "[%s] Loaded %d variables from solvers/%s/solution -> variables/\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    length(variables_loaded),
    solver_name
  )

  # Add variable list if any were loaded
  if (length(variables_loaded) > 0) {
    log_entry <- paste0(log_entry, "  Variables: ", paste(variables_loaded, collapse = ", "), "\n")
  }

  # Write to log file (append mode)
  cat(log_entry, file = log_file, append = TRUE)

  # Print to console if verbose
  if (verbose) {
    cat("Solution log updated: ", log_file, "\n")
  }

  invisible(NULL)
}

#' Check if a string is a compound expression
#'
#' @param s Character string (GAMS-like expression)
#' @param ops Character vector of known operators (defaults: arithmetic, logical, relational)
#' @return Logical: TRUE if expression, FALSE if atomic
#' @export
#'
#' @examples
#' is_gams_expression("x + y") # TRUE
#' is_gams_expression("x") # FALSE
#'
is_gams_expression <- function(
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
  top_ops <- find_top_level_operators(s, ops = ops, precedence = NULL)
  return(nrow(top_ops) > 0)
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

sanitize_description <- function(desc, name = NULL) {
  if (is.null(desc) || length(desc) == 0) {
    value <- ""
  } else {
    value <- desc[1]
    if (is.na(value)) value <- ""
  }

  value <- trimws(value)

  if (nzchar(value) && !grepl("^\\*+$", value)) {
    return(value)
  }

  ""
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
#' replaced elements. Defaults to "m".
#'
#' @returns Modified AST object with remapped elements
#' @export
#'
remap_ast_elements <- function(obj,
                               ast_type = list("when" = "condition"),
                               name_prefix = "m",
                               n = 0L,
                               latex_max = 10,
                               ...
                               ) {
  # browser()
  if (is_empty(ast_type) || is_empty(obj)) return(obj)

  tmp_name <- "s"

  remap_fun <- function(ob) {
    if (inherits(ob, names(ast_type))) {
      slot_names <- names(ob)
      for (slot_name in slot_names) {
        if (slot_name %in% ast_type[[node_type(ob)]])  {
          tex_str <- as_latex(ob[[slot_name]])
          if (estimate_latex_length(tex_str) < latex_max) next
          n <<- n + 1L; map_name <- paste0(tmp_name, n)
          ob[[slot_name]] <- ast_where(name = map_name, content = ob[[slot_name]])
        } else {
          ob[[slot_name]] <- remap_fun(ob[[slot_name]])
        }
      }
    } else if (inherits(ob, c("ast", "multimod"))) {
      j <- 1
      for (j in names(ob)) {
        ob[[j]] <- remap_fun(ob[[j]])
      }
    } else if (is.list(ob) && !is.data.frame(ob)) {
      ob <- lapply(ob, remap_fun)
    }
    ob
  }

  obj <- remap_fun(obj)

  # remove duplicates by equations
  if (n > 0 && inherits(obj, "multimod") && !is.null(obj$equations)) {
    obj$equations <- lapply(obj$equations, replace_where_duplicates)
  } else if (n > 0 && inherits(obj, "equation")) {
    obj <- replace_where_duplicates(obj)
  }

  obj
}

#' Extract "where" nodes from an AST
extract_where_nodes <- function(ast) {
  result <- list()

  walk <- function(x) {
    if (inherits(x, "where")) {
      result[[x$name]] <<- x #$content
    }
    if (is.list(x) && !is.data.frame(x)) {
      lapply(x, walk)
    }
  }

  walk(ast)
  result
}

replace_where_duplicates <- function(obj) {

  w <- extract_where_nodes(obj)
  if (length(w) == 0) return(obj)  # no "where" nodes to process
  h <- sapply(w, extract_ast_elements, name = "hash")
  d <- data.frame(
    name = names(w),
    hash = unlist(h),
    stringsAsFactors = FALSE
  )
  d$duplicates <- duplicated(d$hash)
  # browser()
  if (nrow(d) == 0) return(obj)  # no duplicates, nothing to do
  d$new_name <- NA_character_
  d$new_name[!d$duplicates] <- paste0("m", seq_len(sum(!d$duplicates)))
  for (i in seq_len(nrow(d))) {
    if (d$duplicates[i]) {
      j <- which(d$hash == d$hash[i] & !d$duplicates)
      d$new_name[i] <- d$new_name[j]
    }
  }

  # replace names in obj with d$new_name
  rename_fun <- function(x) {
    # browser()
    if (is_empty(x)) return(x)
    # message(as.character(x))
    if (inherits(x, "where") && isTRUE(x$name %in% d$name)) {
      # browser()
      new_name <- d$new_name[d$name == x$name][1]
      x$name <- new_name
      # Recurse into subfields
      i <- 1
      while (i <= length(x)) {
        x[[i]] <- rename_fun(x[[i]])
        i <- i + 1
      }
    } else if (is.list(x) && !is.data.frame(x)) {
      # x <- lapply(x, rename_fun)
      i <- 1
      while (i <= length(x)) {
        x[[i]] <- rename_fun(x[[i]])
        i <- i + 1
      }
    }
    x
  }
  obj <- rename_fun(obj)
  return(obj)
}


#' Apply name aliases to AST nodes
#'
#' @param ast An AST object or list.
#' @param alias_map A named list or character vector. Keys are aliases, values are original names.
#' @param classes Character vector of AST classes to filter (optional).
#'
#' @return Modified AST with aliases applied.
#' @export
alias_ast_names <- function(ast, alias_map, classes = NULL, ...) {
  stopifnot(is.list(alias_map))

  # `alias_map[[nm]]` is a hashed lookup. The previous form was
  # `nm %in% names(alias_map)`, which allocated a fresh character vector of
  # every key at every node visited -- the map is constant for a traversal, so
  # that was pure waste repeated once per node.

  rename_walk <- function(x) {
    n <- length(x)
    if (n == 0L) return(x)

    if (inherits(x, "ast")) {
      if (is.null(classes) || class(x)[1L] %in% classes) {
        nm <- x$name
        if (!is.null(nm) && is.character(nm) && length(nm) == 1L) {
          a <- alias_map[[nm]]
          if (!is.null(a)) x$name <- a
        }
      }
    } else if (!is.list(x) || is.data.frame(x)) {
      # Atomic vectors and data frames carry no nested names to rewrite.
      return(x)
    }

    for (i in seq_len(n)) {
      xi <- x[[i]]
      # Only lists can contain further nodes. Recursing into atomic leaves cost
      # a function call each and returned them unchanged; an ast node is mostly
      # scalar fields, so that was the bulk of the calls.
      if (is.list(xi)) x[[i]] <- rename_walk(xi)
    }
    x
  }

  rename_walk(ast)
}

# short_names_sets <- list(
#   comm    = "c",  # commodity
#   region  = "r",  # region
#   year    = "y",  # year
#   slice   = "t",  # time slice
#   sup     = "s",  # supply
#   dem     = "d",  # demand
#   tech    = "n",  # technology
#   stg     = "g",  # storage
#   trade   = "z",  # interregional trade
#   expp    = "x",  # export to ROW
#   imp     = "m",  # import from ROW
#   weather = "w",  # weather
#   process = "p",  # process
#   aux     = "a",  # auxiliary indicator (e.g. flags, switches)
#   input   = "i",  # input flows to process
#   output  = "o",  # output flows from process
#   group   = "u",  # group of related commodities or tags
#   # shorts for aliases
#   techp   = "np",
#   regionp = "rp",
#   region2 = "r2",
#   src     = "rs",
#   dst     = "rd",
#   yearp   = "yp",
#   yeare   = "ye",
#   yearn   = "yn",
#   year2   = "y2",
#   slicep  = "tp",
#   slicepp = "tpp",
#   slice2  = "t2",
#   groupp  = "up",
#   commp   = "cp",
#   acomm   = "ca",
#   comme   = "ce",
#   supp    = "sp"
# ) |>
#   unique()

short_names_sets <- list(
  tech = "h",      # technology (avoid 't' conflict with time, trade)
  trade = "a",     # trade (avoid 't' conflict)
  sup = "u",       # supply (avoid 's' conflict with slice, stg)
  slice = "ts",    # time slice (avoid 's' conflict)
  stg = "o",       # storage (avoid 's' conflict)
  region = "r",
  comm = "c",
  year = "y",
  dem = "d",
  group = "g",
  imp = "i",
  expp = "e",
  weather = "w"
)


#' Resolve a dimension name to its alias
#'
#' @param name A character string or vector of dimension names.
#' @param base Named list of base aliases.
#' @param extended Named list of extended aliases.
#' @param default If no alias is found, return `name`, `NA`, or `"?"`.
#'
#' @return A character vector of resolved aliases.
#' @noRd
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


#' Recursively apply a function to all nodes of an AST
#'
#' @param node An AST node or list of nodes.
#' @param func A function to apply to each node. It should return a scalar or structured value.
#' @param include_class Logical: if TRUE, include class name as part of result tree.
#' @returns A tree with the same structure but values replaced by func(node)
map_ast <- function(node, func, include_class = TRUE) {
  recurse <- function(n) {
    if (inherits(n, "ast") || is.list(n)) {
      mapped <- lapply(n, recurse)
      name <- if (include_class && inherits(n, "ast")) node_type(n) else NULL
      if (!is.null(name)) structure(mapped, class = name) else mapped
    } else {
      func(n)
    }
  }
  recurse(node)
}

#' Collect all AST node classes from model or AST
#'
#' Walks through an AST or model structure and collects all unique AST node
#' classes encountered. Non-AST objects are ignored.
#'
#' @param obj An AST node, model object, or nested structure containing AST nodes
#'
#' @return Character vector of unique AST classes found
#' @export
#'
#' @examples
#' \dontrun{
#' gmpl <- read_gmpl("model.mod")
#' classes <- collect_ast_classes(gmpl)
#' # Returns: c("symbol", "constant", "expression", "setmin", ...)
#' }
collect_ast_classes <- function(obj) {
  classes_found <- character(0)
  
  walk <- function(node) {
    if (is.null(node)) return()
    
    # If it's an AST node, record its class
    if (inherits(node, "ast")) {
      node_class <- class(node)[1]  # First class is the specific type
      classes_found <<- c(classes_found, node_class)
    }
    
    # Recurse into lists (including AST nodes which are lists)
    if (is.list(node) && !is.data.frame(node)) {
      for (elem in node) {
        if (!is.null(elem)) {
          walk(elem)
        }
      }
    }
  }
  
  walk(obj)
  unique(classes_found)
}


#' Extract names of specific node types from AST
#'
#' Recursively traverses an AST and collects the names of nodes matching
#' the specified types (e.g., "variable", "parameter", "mapping").
#'
#' @param ast An AST object (equation LHS/RHS, or any expression node)
#' @param types Character vector of node types to extract (e.g., c("variable", "parameter"))
#'
#' @return Character vector of unique names found
#' @keywords internal
extract_ast_names <- function(ast, types = c("variable", "parameter", "mapping")) {
  if (is.null(ast)) return(character(0))

  names_found <- character(0)

  walk <- function(node) {
    if (is.null(node)) return()

    # Check if this node matches one of the target types
    node_class <- node_type(node)
    if (!is.null(node_class) && length(node_class) > 0 && node_class %in% types) {
      if (!is.null(node$name)) {
        names_found <<- c(names_found, node$name)
      }
    }

    # Recurse into child nodes based on node type
    if (!is.null(node_class) && length(node_class) > 0) {
      if (node_class == "expression") {
        walk(node$lhs)
        walk(node$rhs)
        if (!is.null(node$operands)) {
          lapply(node$operands, walk)
        }
      } else if (node_class %in% c("sum", "prod")) {
        if (!is.null(node$domain)) walk(node$domain)
        walk(node$value)
      } else if (node_class == "when") {
        walk(node$condition)
        walk(node$then)
        if (!is.null(node$else_)) walk(node$else_)
      } else if (node_class == "condition") {
        walk(node$lhs)
        walk(node$rhs)
      }
    }

    # For generic lists, recurse into all elements
    if (is.list(node) && !is.data.frame(node)) {
      for (elem in node) {
        if (!is.null(elem) && (inherits(elem, "ast") || is.list(elem))) {
          walk(elem)
        }
      }
    }
  }

  walk(ast)
  unique(names_found)
}

#' Compare JuMP model constraint statistics between two model directories
#'
#' Reads constraint statistics CSV files from two JuMP model solver directories
#' and compares constraint counts and non-zero counts. Useful for validating
#' model generation changes.
#'
#' @param model_dir1 Path to first model's solver/jump directory (e.g., "tmp/model_v1/solvers/jump")
#' @param model_dir2 Path to second model's solver/jump directory (e.g., "tmp/model_v2/solvers/jump")
#' @param save_comparison Logical; if TRUE, saves merged comparison to CSV in model_dir1's parent directory
#' @param verbose Logical; if TRUE, prints detailed comparison results
#'
#' @return A list with components:
#'   \item{merged}{Data frame with merged statistics from both models}
#'   \item{constraint_mismatches}{Data frame of equations with different constraint counts}
#'   \item{nnz_differences}{Data frame of equations with different non-zero counts}
#'   \item{summary}{Named list with total counts and match status}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare two JuMP model versions
#' comp <- compare_jump_stats(
#'   "tmp/model_v1/solvers/jump",
#'   "tmp/model_v2/solvers/jump",
#'   verbose = TRUE
#' )
#'
#' # Check if models match
#' if (comp$summary$all_match) {
#'   message("Models are identical!")
#' }
#' }
compare_jump_stats <- function(model_dir1,
                              model_dir2,
                              save_comparison = TRUE,
                              verbose = TRUE) {

  # Helper to find constraint_stats.csv
  find_stats_file <- function(dir) {
    # Try direct path
    if (file.exists(file.path(dir, "constraint_stats.csv"))) {
      return(file.path(dir, "constraint_stats.csv"))
    }
    # Try solvers/jump subdirectory
    if (file.exists(file.path(dir, "solvers", "jump", "constraint_stats.csv"))) {
      return(file.path(dir, "solvers", "jump", "constraint_stats.csv"))
    }
    return(NULL)
  }

  stats_file1 <- find_stats_file(model_dir1)
  stats_file2 <- find_stats_file(model_dir2)

  if (is.null(stats_file1)) {
    stop("Constraint stats not found in model_dir1: ", model_dir1)
  }
  if (is.null(stats_file2)) {
    stop("Constraint stats not found in model_dir2: ", model_dir2)
  }

  # Read statistics
  stats1 <- read.csv(stats_file1, stringsAsFactors = FALSE)
  stats2 <- read.csv(stats_file2, stringsAsFactors = FALSE)

  if (verbose) {
    cat("\n=== JuMP Model Comparison ===\n\n")
    cat("Model 1:", normalizePath(dirname(stats_file1)), "\n")
    cat("  Total constraints:", sum(stats1$count), "\n")
    cat("  Total non-zeros:", sum(stats1$nnz_total), "\n\n")

    cat("Model 2:", normalizePath(dirname(stats_file2)), "\n")
    cat("  Total constraints:", sum(stats2$count), "\n")
    cat("  Total non-zeros:", sum(stats2$nnz_total), "\n\n")
  }

  # Merge by equation name
  merged <- merge(stats1, stats2, by = "name", suffixes = c("_1", "_2"), all = TRUE)
  merged$count_1[is.na(merged$count_1)] <- 0
  merged$count_2[is.na(merged$count_2)] <- 0
  merged$count_diff <- merged$count_2 - merged$count_1

  # Check for constraint count differences
  constraint_mismatches <- merged[merged$count_diff != 0,
                                 c("name", "count_1", "count_2", "count_diff")]

  if (nrow(constraint_mismatches) > 0) {
    if (verbose) {
      cat("*** CONSTRAINT COUNT DIFFERENCES ***\n")
      print(constraint_mismatches, row.names = FALSE)
      cat("\n")
    }
  } else {
    if (verbose) cat("\u2713 All constraint counts match!\n\n")
  }

  # Check nnz differences
  merged$nnz_diff <- merged$nnz_total_2 - merged$nnz_total_1
  nnz_differences <- merged[abs(merged$nnz_diff) > 0,
                           c("name", "nnz_total_1", "nnz_total_2", "nnz_diff")]

  if (nrow(nnz_differences) > 0) {
    if (verbose) {
      cat("*** NON-ZERO COUNT DIFFERENCES ***\n")
      print(head(nnz_differences, 20), row.names = FALSE)
      if (nrow(nnz_differences) > 20) {
        cat(sprintf("\n... and %d more equations with differences\n\n", nrow(nnz_differences) - 20))
      } else {
        cat("\n")
      }
    }
  } else {
    if (verbose) cat("\u2713 All non-zero counts match!\n\n")
  }

  # Save comparison if requested
  if (save_comparison) {
    # Determine output directory
    out_dir <- if (file.exists(file.path(model_dir1, "solvers"))) {
      model_dir1
    } else {
      dirname(dirname(model_dir1))  # Go up from solvers/jump to model root
    }
    out_file <- file.path(out_dir, "constraint_comparison.csv")
    write.csv(merged, out_file, row.names = FALSE)
    if (verbose) {
      cat("Comparison saved to:", normalizePath(out_file), "\n\n")
    }
  }

  # Build summary
  summary_info <- list(
    total_constraints_1 = sum(stats1$count),
    total_constraints_2 = sum(stats2$count),
    total_nnz_1 = sum(stats1$nnz_total),
    total_nnz_2 = sum(stats2$nnz_total),
    constraints_match = nrow(constraint_mismatches) == 0,
    nnz_match = nrow(nnz_differences) == 0,
    all_match = nrow(constraint_mismatches) == 0 && nrow(nnz_differences) == 0
  )

  invisible(list(
    merged = merged,
    constraint_mismatches = constraint_mismatches,
    nnz_differences = nnz_differences,
    summary = summary_info
  ))
}




#' Marker for a state the author believed unreachable
#'
#' Replaces `browser()` calls left in package code. `browser()` stops an
#' interactive session and waits for input with no message -- indistinguishable
#' from a hang -- while under `Rscript` it is inert, so it survives CI and only
#' ever bites a human. This warns and continues: the signal is kept, the block
#' is not.
#'
#' @param context Where it fired, as "file.R:line".
#' @keywords internal
.dev_break <- function(context = "") {
  warning("multimod: reached a state believed unreachable",
          if (nzchar(context)) paste0(" (", context, ")") else "",
          call. = FALSE)
  invisible(NULL)
}
