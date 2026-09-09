#' Add index aliases to a multimod model
#'
#' Generates and adds short index aliases for sets in a multimod model.
#' These aliases are used as iteration variable names in constraint indexing
#' and array subscripts across different output formats (GMPL, Julia, Pyomo, LaTeX).
#'
#' @param model A multimod model object
#' @param index_aliases Optional named character vector of custom aliases.
#'   If NULL (default), aliases are auto-generated.
#' @param overwrite Logical. If TRUE, overwrites existing index_aliases. Default FALSE.
#'
#' @return The model with index_aliases added
#'
#' @details
#' Index aliases are short symbolic names used in equations:
#' - GMPL: `{h in tech, r in region}` where h, r are index aliases
#' - Julia: `@constraint(model, [h in tech, r in region], ...)`
#' - LaTeX: `\\sum_{h \\in \\mathcal{T}}`
#'
#' Auto-generated aliases follow these rules:
#' - Use first letter(s) of set name
#' - Handle plurals (e.g., "technologies" -> "t")
#' - Avoid conflicts with existing symbols
#' - For aliases like "commp", append suffix (e.g., "cp")
#'
#' @examples
#' model <- new_model(
#'   sets = list(
#'     tech = new_set("technology"),
#'     region = new_set("region"),
#'     comm = new_set("commodity")
#'   )
#' )
#'
#' # Auto-generate aliases
#' model <- add_index_aliases(model)
#' # model$index_aliases: c(tech = "h", region = "r", comm = "c")
#'
#' # Custom aliases
#' model <- add_index_aliases(model, index_aliases = c(tech = "t", region = "reg"))
#'
#' @export
add_index_aliases <- function(model, index_aliases = NULL, overwrite = FALSE) {
  stopifnot(inherits(model, "multimod"))

  # Check if index_aliases already exists
  if (!is.null(model$index_aliases) && !overwrite) {
    message("Model already has index_aliases. Use overwrite=TRUE to replace them.")
    return(model)
  }

  # Get all symbols to avoid conflicts
  all_symbols <- build_symbols_list(model)

  # all set names (including aliases)
  set_names <- names(model$sets)
  # set_aliases <- model$aliases |> lapply(function(x) x[-1]) |> unlist()
  # set_names <- c(all_symbols$sets, all_symbols$aliases) |> unique()
  if (length(set_names) == 0) {
    warning("Model has no sets. Cannot generate index aliases.")
    return(model)
  }

  # If custom index_aliases provided, validate and use them
  if (!is.null(index_aliases)) {
    if (!is.character(index_aliases) || is.null(names(index_aliases))) {
      stop("index_aliases must be a named character vector")
    }

    # Check that all names in index_aliases exist in sets
    unknown_sets <- setdiff(names(index_aliases), set_names)
    if (length(unknown_sets) > 0) {
      warning("Unknown sets in index_aliases: ", paste(unknown_sets, collapse = ", "))
    }

    # Generate index_aliases for remaining sets
    remaining_sets <- setdiff(set_names, names(index_aliases))
    if (length(remaining_sets) > 0) {
      auto_aliases <- generate_index_aliases(
        remaining_sets,
        all_set_names = set_names,
        all_symbols = all_symbols
      )
      index_aliases <- c(index_aliases, auto_aliases)
    # } else {
      # index_aliases <- index_aliases
    }
  } else {
    # Auto-generate all index_aliases
    index_aliases <- generate_index_aliases(
      set_names,
      all_set_names = set_names,
      all_symbols = all_symbols
    )
  }

  # Add to model
  model$index_aliases <- index_aliases
  model
}

#' Get index alias for a set
#'
#' Retrieves the index alias for a set name from a multimod model.
#' If no alias exists, generates one on-the-fly.
#'
#' @param model A multimod model object
#' @param set_name Character. Name of the set
#'
#' @return Character. The index alias for the set
#'
#' @examples
#' model <- add_index_aliases(model)
#' get_index_alias(model, "tech")  # "h"
#'
#' @export
get_index_alias <- function(model, set_name) {
  stopifnot(inherits(model, "multimod"))
  stopifnot(is.character(set_name) && length(set_name) == 1)

  # If model has index_aliases, use them
  if (!is.null(model$index_aliases) && set_name %in% names(model$index_aliases)) {
    return(model$index_aliases[[set_name]])
  }

  # Otherwise generate on-the-fly
  all_symbols <- build_symbols_list(model)
  set_names <- names(model$sets)

  generate_index_alias(
    set_name,
    used_names = character(0),
    all_set_names = set_names,
    all_symbols = all_symbols
  )
}

#' Check if model has index aliases
#'
#' @param model A multimod model object
#' @return Logical. TRUE if model has index_aliases
#'
#' @export
has_index_aliases <- function(model) {
  inherits(model, "multimod") && !is.null(model$index_aliases)
}

#' Preferred short names for common sets to avoid conflicts
#' @keywords internal
preferred_dummy_names <- list(
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

#' Generate dummy variable name from set name
#'
#' Creates a short dummy variable name from a set name following conventions:
#' - Uses preferred names for common sets (tech -> h, slice -> ts, etc.)
#' - First letter of the set name (lowercase) for others
#' - If it's an alias, use two letters (checking against all_set_names)
#' - Ensures uniqueness by checking against all symbols in model
#'
#' @param set_name Character. The name of the set
#' @param used_names Character vector. Names already used (to avoid conflicts)
#' @param all_set_names Character vector. All set names in the model (to detect aliases)
#' @param all_symbols List. All symbols in model (sets, parameters, variables, etc.) to avoid conflicts
#' @return Character. A short dummy variable name
#' @examples
#' generate_index_alias("tech") # "h"
#' generate_index_alias("slice") # "ts"
#' generate_index_alias("commp", all_set_names = c("comm", "commp")) # "cp"
#' @export
generate_index_alias <- function(set_name, used_names = character(0),
                                 all_set_names = character(0), all_symbols = NULL) {
  stopifnot(is.character(set_name), length(set_name) == 1)

  # Convert to lowercase
  name_lower <- tolower(set_name)

  # Get all symbol names to avoid conflicts
  reserved_names <- character(0)
  if (!is.null(all_symbols)) {
    reserved_names <- tolower(unique(c(
      all_symbols$sets,
      all_symbols$mappings,
      all_symbols$parameters,
      all_symbols$variables
    )))
  }

  # Check if this is an alias by looking for base name in all_set_names
  # Try removing common suffixes and see if base exists
  is_alias <- FALSE
  base <- name_lower
  suffix_used <- ""

  if (length(all_set_names) > 0) {
    # Try different suffix patterns
    for (pattern in c("pp$", "p$", "2$", "3$")) {
      potential_base <- sub(pattern, "", name_lower)
      # Check for NA values before comparison
      if (!is.na(potential_base) && !is.na(name_lower) && 
          potential_base != name_lower && nchar(potential_base) > 0) {
        # Check if base exists in all_set_names (case-insensitive)
        if (any(tolower(all_set_names) == potential_base)) {
          is_alias <- TRUE
          base <- potential_base
          suffix_used <- sub(paste0("^", potential_base), "", name_lower)
          break
        }
      }
    }
  }

  # Build candidates list
  candidates <- character(0)

  # First, check for preferred names
  if (!is_alias && name_lower %in% names(preferred_dummy_names)) {
    candidates <- c(candidates, preferred_dummy_names[[name_lower]])
  }

  if (is_alias) {
    # For aliases, use two letters: base_first_letter + suffix indicator
    # Check if base has a preferred name
    if (base %in% names(preferred_dummy_names)) {
      base_dummy <- preferred_dummy_names[[base]]
    } else {
      base_dummy <- substr(base, 1, 1)
    }

    # Map suffix to indicator
    suffix_indicator <- if (suffix_used == "pp") {
      "2"
    } else if (suffix_used %in% c("p", "2", "3")) {
      suffix_used
    } else {
      "p"  # default
    }

    candidates <- c(
      candidates,
      paste0(base_dummy, suffix_indicator),  # commp -> cp, groupp -> gp
      paste0(substr(base, 1, 1), suffix_indicator),  # fallback to first letter
      paste0(substr(base, 1, 1), "p"),       # another fallback
      substr(name_lower, 1, 2)               # first two letters of full name
    )
  } else {
    # For regular sets, try single letter or preferred name
    first_letter <- substr(name_lower, 1, 1)
    candidates <- c(
      candidates,
      first_letter,
      paste0(first_letter, "1"),
      substr(name_lower, 1, 2),
      substr(name_lower, 1, 3)
    )
  }

  # Find first unused candidate that doesn't conflict with reserved names
  for (candidate in candidates) {
    candidate_lower <- tolower(candidate)
    # Check not in used_names and not in reserved symbol names
    if (!(candidate %in% used_names) && !(candidate_lower %in% reserved_names)) {
      return(candidate)
    }
  }

  # If all candidates are used or conflict, append numbers
  base_candidate <- candidates[1]
  i <- 1
  while (TRUE) {
    candidate <- paste0(base_candidate, "_", i)
    candidate_lower <- tolower(candidate)
    if (!(candidate %in% used_names) && !(candidate_lower %in% reserved_names)) {
      return(candidate)
    }
    i <- i + 1
  }
}

#' Generate dummy variable names for a list of sets
#'
#' Creates unique dummy variable names for a collection of sets
#'
#' @param set_names Character vector. Names of the sets
#' @param all_set_names Character vector. All set names in model (for alias detection)
#' @param all_symbols List. All symbols in model (from build_symbols_list)
#' @return Named character vector. Dummy variable names with set names as names
#' @examples
#' generate_index_aliases(c("tech", "region", "comm", "commp", "year", "slice"))
#' @export
generate_index_aliases <- function(set_names, all_set_names = set_names, all_symbols = NULL) {
  stopifnot(is.character(set_names))

  if (length(set_names) == 0) {
    return(character(0))
  }

  index_aliases <- character(length(set_names))
  names(index_aliases) <- set_names
  used_names <- character(0)

  for (i in seq_along(set_names)) {
    index_aliases[i] <- generate_index_alias(set_names[i], used_names, all_set_names, all_symbols)
    used_names <- c(used_names, index_aliases[i])
  }

  index_aliases
}


