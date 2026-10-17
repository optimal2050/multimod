# Reading JuMP (Julia) constraints
#
# The reference syntax is what write_jump() emits:
#
#   @constraint(
#       model,
#       eqCapLim[(h, r) in mTR],
#       vGen[(h, r)] <= get(pAvail, (h, r), pAvailDef) * vCap[(h, r)]
#   )
#
# which this reads back, along with the forms a modeller writes by hand:
# `name[i in S, j in T]` headers, `sum(expr for i in S, j in T)` generators
# with an optional `if` filter, and plain `v[i, j]` indexing.

# Operators accepted in a JuMP expression. `&&` and `||` are normalised to the
# AST's `and`/`or` first, so the shared precedence table covers them.
jump_ops <- c(
  "+", "-", "*", "/", "^",
  "==", "!=", "<=", ">=", "<", ">",
  "and", "or", "not", "!"
)

# Julia functions that map onto an AST call rather than an aggregation.
jump_known_funcs <- c(
  "log", "log2", "log10", "exp", "sqrt", "abs", "floor", "ceil", "round",
  "min", "max", "sin", "cos", "tan"
)

# Bracket-aware character levels. The shared get_expression_level() counts only
# parentheses, which would expose the `-` in `vStock[r, y-1]` as a top-level
# operator. The convention is the same: an opening bracket carries the outer
# level and a closing bracket the level it returns to.
.jump_level <- function(expr) {
  chars <- strsplit(expr, "", fixed = TRUE)[[1]]
  lev <- integer(length(chars))
  level <- 0L
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "(" || ch == "[" || ch == "{") {
      lev[i] <- level
      level <- level + 1L
    } else if (ch == ")" || ch == "]" || ch == "}") {
      level <- max(0L, level - 1L)
      lev[i] <- level
    } else {
      lev[i] <- level
    }
  }
  data.frame(index = seq_along(chars), char = chars, level = lev)
}

# Split on commas that are outside every bracket.
.jump_split_args <- function(str) {
  if (!nzchar(trimws(str))) return(character())
  lv <- .jump_level(str)
  cuts <- which(lv$char == "," & lv$level == 0L)
  starts <- c(1L, cuts + 1L)
  ends <- c(cuts - 1L, nrow(lv))
  out <- vapply(seq_along(starts), function(k) {
    if (ends[k] < starts[k]) "" else substr(str, starts[k], ends[k])
  }, character(1))
  trimws(out)
}

# Position of a keyword (`for`, `if`, `in`) outside every bracket, or NA.
.jump_find_keyword <- function(str, word) {
  lv <- .jump_level(str)
  at <- gregexpr(paste0("\\b", word, "\\b"), str, perl = TRUE)[[1]]
  if (at[1] < 0L) return(NA_integer_)
  at <- at[lv$level[at] == 0L]
  if (length(at) == 0L) NA_integer_ else at[1]
}

# TRUE when the whole string is one bracketed group, e.g. "(a + b)".
.jump_is_enclosed <- function(expr, open = "(", close = ")") {
  n <- nchar(expr)
  if (n < 2L) return(FALSE)
  if (substr(expr, 1L, 1L) != open || substr(expr, n, n) != close) return(FALSE)
  lv <- .jump_level(expr)
  all(lv$level[2:(n - 1L)] >= 1L)
}

# `name` followed by a bracketed index list: returns the two parts, or NULL.
.jump_split_indexed <- function(expr) {
  at <- regexpr("^[A-Za-z_][A-Za-z0-9_]*", expr)
  if (at < 0L) return(NULL)
  name <- regmatches(expr, at)
  rest <- substring(expr, attr(at, "match.length") + 1L)
  if (!startsWith(rest, "[") || !endsWith(rest, "]")) return(NULL)
  if (!.jump_is_enclosed(rest, "[", "]")) return(NULL)
  list(name = name, index = substr(rest, 2L, nchar(rest) - 1L))
}

# `name(` ... `)` spanning the whole string: returns name and argument text.
.jump_split_call <- function(expr) {
  at <- regexpr("^[A-Za-z_][A-Za-z0-9_]*", expr)
  if (at < 0L) return(NULL)
  name <- regmatches(expr, at)
  rest <- substring(expr, attr(at, "match.length") + 1L)
  if (!.jump_is_enclosed(rest, "(", ")")) return(NULL)
  list(name = name, args = substr(rest, 2L, nchar(rest) - 1L))
}

# One index entry: a name, an alias of one, or a shift like `y-1`.
.jump_index_entry <- function(txt, aliases, symbols = NULL, model = NULL) {
  txt <- trimws(txt)
  sh <- regmatches(txt, regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*([+-])\\s*([0-9]+)$", txt))[[1]]
  if (length(sh) == 4L) {
    nm <- .jump_resolve_alias(sh[2], aliases)
    off <- as.integer(sh[4]) * if (sh[3] == "-") -1L else 1L
    return(ast_shift(nm, off))
  }
  if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", txt)) {
    return(.jump_resolve_alias(txt, aliases))
  }
  # Anything else is an expression in subscript position. ast_dims() would try
  # to parse a bare string itself, which needs a language it has not been
  # given, so it is parsed here.
  parse_jump_expr(txt, symbols, aliases, model)
}

.jump_resolve_alias <- function(nm, aliases) {
  if (length(aliases) && nm %in% names(aliases)) aliases[[nm]] else nm
}

# The index list of a reference: `(h, r)` and `h, r` are the same thing.
.jump_index_dims <- function(index_str, aliases, symbols = NULL, model = NULL) {
  if (is.null(index_str) || length(index_str) == 0L) return(NULL)
  s <- trimws(index_str)
  if (!nzchar(s)) return(NULL)
  if (.jump_is_enclosed(s, "(", ")")) s <- substr(s, 2L, nchar(s) - 1L)
  parts <- .jump_split_args(s)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(NULL)
  entries <- lapply(parts, .jump_index_entry, aliases = aliases,
                    symbols = symbols, model = model)
  do.call(ast_dims, entries)
}

# A declared symbol, classified by the symbol table.
.jump_reference <- function(name, index_str, symbols, aliases, model = NULL) {
  dims <- .jump_index_dims(index_str, aliases, symbols, model)
  if (name %in% symbols$variables) return(ast_variable(name, dims = dims))
  if (name %in% symbols$parameters) return(ast_parameter(name, dims = dims))
  if (name %in% symbols$mappings) return(ast_mapping(name, dims = dims))
  if (name %in% symbols$sets) return(ast_set(name))
  if (is.null(dims)) return(ast_symbol(name))
  # Unknown name carrying indices: keep it as a call so nothing is lost.
  ast_call(name, args = list(ast_symbol(trimws(index_str))))
}

# Iterator specifications of a header or a generator: `i in S`, `(i,j) in S`.
# Each returns the index names as the model declares them, the alias the JuMP
# text used, and the set the indices range over.
.jump_iterators <- function(str, symbols, model, used = character()) {
  specs <- .jump_split_args(str)
  specs <- specs[nzchar(specs)]
  out <- list()
  taken <- used
  for (sp in specs) {
    at <- .jump_find_keyword(sp, "in")
    if (is.na(at)) {
      # A bare name iterates over a set of the same name.
      nm <- trimws(sp)
      taken <- c(taken, nm)
      out[[length(out) + 1L]] <- list(vars = nm, dims = nm, set = nm,
                                      tuple = FALSE, rename = FALSE)
      next
    }
    var_part <- trimws(substr(sp, 1L, at - 1L))
    set_part <- trimws(substring(sp, at + 2L))
    tuple <- .jump_is_enclosed(var_part, "(", ")")
    if (tuple) var_part <- substr(var_part, 2L, nchar(var_part) - 1L)
    vars <- trimws(.jump_split_args(var_part))
    vars <- vars[nzchar(vars)]
    cand <- .jump_set_dim_names(set_part, vars, symbols, model)
    rename <- TRUE
    if (all(vapply(vars, .jump_declared_alias, logical(1),
                   set_name = set_part, model = model))) {
      # The model declares this name as an alias of the set. That is how the
      # other readers represent it - dims carry the set, references the alias -
      # so the references are left as the text wrote them.
      rename <- FALSE
    } else if (any(cand %in% taken)) {
      # A second iterator over a set already bound. The dimension is still that
      # set, but renaming the references would collapse the two indices into
      # one, so they keep the names the text gave them.
      rename <- FALSE
    }
    taken <- c(taken, cand)
    out[[length(out) + 1L]] <- list(
      vars = vars,
      dims = cand,
      set = set_part,
      tuple = tuple,
      rename = rename
    )
  }
  out
}

# What the model calls the indices a set or mapping ranges over. JuMP text uses
# short aliases (`h`, `r`), and the AST the other readers build uses the
# declared names (`tech`, `region`), so the two are matched up positionally.
.jump_set_dim_names <- function(set_name, vars, symbols, model) {
  if (!is.null(model) && set_name %in% symbols$mappings) {
    mp <- model$mappings[[set_name]]
    nms <- if (!is.null(mp)) get_dim_names(mp$dims) else NULL
    if (length(nms) == length(vars)) return(nms)
  }
  if (length(vars) == 1L && set_name %in% symbols$sets) return(set_name)
  vars
}

# Rewrite map (name in the text -> name in the model), for the iterators whose
# references should be renamed.
.jump_alias_map <- function(iters) {
  out <- character()
  for (it in iters) {
    if (isTRUE(it$rename) && length(it$vars) == length(it$dims)) {
      out[it$vars] <- it$dims
    }
  }
  out
}

# Index aliases (name in the model -> name in the text), recorded on the
# equation whether or not its references were rewritten, so that writing it
# back out uses the names it came in with.
.jump_index_alias_map <- function(iters) {
  out <- character()
  for (it in iters) {
    if (length(it$vars) == length(it$dims)) {
      out[it$dims] <- it$vars
    }
  }
  out
}

# Does the model declare `nm` as an alias of `set_name`?
.jump_declared_alias <- function(nm, set_name, model) {
  if (is.null(model) || is.null(model$aliases)) return(FALSE)
  for (grp in model$aliases) {
    if (set_name %in% grp && nm %in% grp) return(TRUE)
  }
  FALSE
}

# Tidy a statement without folding it onto one line. Julia separates the
# condition of an `if` block from its body with a newline or a semicolon, and
# write_jump() uses the newline, so it carries meaning here.
.jump_tidy <- function(txt) {
  s <- gsub("\r\n", "\n", txt, fixed = TRUE)
  s <- gsub("[ \t]+", " ", s)
  s <- gsub(" *\n *", "\n", s)
  s <- gsub("\n+", "\n", s)
  sub(";[[:space:]]*$", "", trimws(s))
}

# `&&` / `||` are the AST's `and` / `or`.
.jump_normalise <- function(expr) {
  expr <- gsub("&&", " and ", expr, fixed = TRUE)
  expr <- gsub("||", " or ", expr, fixed = TRUE)
  .jump_tidy(expr)
}

# `if cond <sep> then else otherwise end`, the form write_jump() uses for a
# conditional because it is valid inside a comprehension. The condition ends at
# the first newline or semicolon, as it does in Julia. Returns NULL when the
# text does not have that shape, so the caller can carry on.
.jump_if_block <- function(expr, symbols, aliases, model, depth, max_depth) {
  inner <- trimws(substr(expr, 3L, nchar(expr) - 3L))
  if (!nzchar(inner)) return(NULL)

  at_else <- .jump_find_keyword(inner, "else")
  if (is.na(at_else)) {
    head_txt <- inner
    other_txt <- NULL
  } else {
    head_txt <- trimws(substr(inner, 1L, at_else - 1L))
    other_txt <- trimws(substring(inner, at_else + 4L))
  }

  at_sep <- .jump_find_separator(head_txt)
  if (is.na(at_sep)) return(NULL)
  cond_txt <- trimws(substr(head_txt, 1L, at_sep - 1L))
  then_txt <- trimws(substring(head_txt, at_sep + 1L))
  if (!nzchar(cond_txt) || !nzchar(then_txt)) return(NULL)

  ast_when(
    condition = .jump_condition(cond_txt, symbols, aliases, model, depth, max_depth),
    then = parse_jump_expr(then_txt, symbols, aliases, model, depth + 1L, max_depth),
    otherwise = if (!is.null(other_txt) && nzchar(other_txt)) {
      parse_jump_expr(other_txt, symbols, aliases, model, depth + 1L, max_depth)
    } else {
      NULL
    }
  )
}

# A condition is an ordinary expression, except for `(i, j) in S`, which
# write_jump() emits as a membership test for a mapping or a sparse parameter.
.jump_condition <- function(txt, symbols, aliases, model, depth, max_depth) {
  at <- .jump_find_keyword(txt, "in")
  if (!is.na(at)) {
    lhs <- trimws(substr(txt, 1L, at - 1L))
    nm <- trimws(substring(txt, at + 2L))
    if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", nm) &&
        (nm %in% symbols$mappings || nm %in% symbols$parameters)) {
      return(.jump_reference(nm, lhs, symbols, aliases, model))
    }
  }
  parse_jump_expr(txt, symbols, aliases, model, depth + 1L, max_depth)
}

# First newline or semicolon outside every bracket, or NA.
.jump_find_separator <- function(str) {
  lv <- .jump_level(str)
  at <- which((lv$char == "\n" | lv$char == ";") & lv$level == 0L)
  if (length(at) == 0L) NA_integer_ else at[1]
}

#' Parse a JuMP expression into multimod AST
#'
#' @param expr Character string holding one JuMP (Julia) expression.
#' @param symbols Symbol table from [build_symbols_list()].
#' @param aliases Named character vector mapping the short index names the
#'   expression uses onto the names the model declares.
#' @param model The model, used to resolve a mapping's declared index names.
#' @param depth,max_depth Recursion guard.
#' @return An AST node.
#' @keywords internal
parse_jump_expr <- function(expr, symbols, aliases = character(),
                            model = NULL, depth = 0L, max_depth = 100L) {
  if (depth > max_depth) {
    stop("Expression nests deeper than ", max_depth, " levels: ",
         substr(expr, 1L, 60L), call. = FALSE)
  }
  expr <- trimws(expr)
  if (!nzchar(expr)) stop("Empty JuMP expression.", call. = FALSE)

  # Drop one layer of enclosing parentheses.
  if (.jump_is_enclosed(expr, "(", ")")) {
    inner <- parse_jump_expr(substr(expr, 2L, nchar(expr) - 1L), symbols,
                             aliases, model, depth + 1L, max_depth)
    if (inherits(inner, "expression")) inner$brackets <- TRUE
    return(inner)
  }

  # Julia's `if cond <newline> then else otherwise end`, which is how
  # write_jump() renders a conditional. This has to be recognised before the
  # operator split, or an operator in the condition would tear the block apart.
  if (grepl("^if\\b", expr) && grepl("\\bend$", expr)) {
    blk <- .jump_if_block(expr, symbols, aliases, model, depth, max_depth)
    if (!is.null(blk)) return(blk)
  }

  # Top-level operators, loosest binding first. A left-associative group splits
  # at its rightmost operator, so `a - b + c` nests as `(a - b) + c`; `^` is
  # right-associative and splits at its leftmost.
  lv <- .jump_level(expr)
  op_tbl <- find_top_level_operators(
    expr, ops = jump_ops, precedence = operator_precedence,
    descending = TRUE, expr_lev = lv)
  op_tbl <- op_tbl[!is.na(op_tbl$op), , drop = FALSE]

  for (p in unique(op_tbl$prec)) {
    grp <- op_tbl[op_tbl$prec == p, , drop = FALSE]
    k <- if (all(grp$op == "^")) which.min(grp$pos) else which.max(grp$pos)
    op <- grp$op[k]
    at <- grp$pos[k]

    lhs_txt <- trimws(substr(expr, 1L, at - 1L))
    rhs_txt <- trimws(substr(expr, at + nchar(op), nchar(expr)))

    if (nzchar(lhs_txt) && nzchar(rhs_txt)) {
      return(ast_expression(
        op,
        lhs = parse_jump_expr(lhs_txt, symbols, aliases, model, depth + 1L, max_depth),
        rhs = parse_jump_expr(rhs_txt, symbols, aliases, model, depth + 1L, max_depth)))
    }
    if (!nzchar(lhs_txt) && nzchar(rhs_txt)) {
      if (op %in% c("-", "!", "not")) {
        return(ast_unary(
          if (op == "!") "not" else op,
          parse_jump_expr(rhs_txt, symbols, aliases, model, depth + 1L, max_depth)))
      }
      if (op == "+") {
        return(parse_jump_expr(rhs_txt, symbols, aliases, model, depth, max_depth))
      }
      stop("Unrecognised unary operator '", op, "' in: ", expr, call. = FALSE)
    }
  }

  # A function call: an aggregation with a generator, the writer's `get()`
  # default wrapper, or an ordinary call.
  cl <- .jump_split_call(expr)
  if (!is.null(cl)) {
    low <- tolower(cl$name)
    if (low %in% c("sum", "prod", "minimum", "maximum")) {
      agg <- .jump_generator(cl$name, cl$args, symbols, aliases, model,
                             depth, max_depth)
      if (!is.null(agg)) return(agg)
    }
    if (cl$name == "parse") {
      pargs <- .jump_split_args(cl$args)
      if (length(pargs) == 2L &&
          trimws(pargs[1]) %in% c("Int", "Int64", "Float64")) {
        # Julia casts a set element to a number to use it in arithmetic;
        # multimod keeps the plain index.
        return(parse_jump_expr(pargs[2], symbols, aliases, model,
                               depth + 1L, max_depth))
      }
    }
    if (cl$name == "get") {
      g <- .jump_get_default(cl$args, symbols, aliases, model)
      if (!is.null(g)) return(g)
    }
    args <- .jump_split_args(cl$args)
    args <- args[nzchar(args)]
    parsed <- lapply(args, parse_jump_expr, symbols = symbols,
                     aliases = aliases, model = model, depth = depth + 1L,
                     max_depth = max_depth)
    if (cl$name %in% jump_known_funcs || !(cl$name %in% unlist(symbols[1:5]))) {
      return(ast_call(cl$name, args = parsed))
    }
    # A declared symbol written with parentheses rather than brackets.
    return(.jump_reference(cl$name, cl$args, symbols, aliases, model))
  }

  # An indexed reference.
  ix <- .jump_split_indexed(expr)
  if (!is.null(ix)) {
    return(.jump_reference(ix$name, ix$index, symbols, aliases, model))
  }

  # A number.
  if (grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", expr)) {
    return(ast_constant(as.numeric(expr)))
  }

  # A bare name.
  if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", expr)) {
    return(.jump_reference(.jump_resolve_alias(expr, aliases), NULL,
                           symbols, aliases, model))
  }

  stop("Could not parse JuMP expression: ", expr, call. = FALSE)
}

# `get(pAvail, (h, r), pAvailDef)` is how write_jump() reads a parameter that
# has a default. The default lives on the declaration, so the reference is all
# that needs to come back.
.jump_get_default <- function(args_str, symbols, aliases, model = NULL) {
  args <- .jump_split_args(args_str)
  if (length(args) != 3L) return(NULL)
  nm <- trimws(args[1])
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", nm)) return(NULL)
  if (!(nm %in% symbols$parameters)) return(NULL)
  .jump_reference(nm, args[2], symbols, aliases, model)
}

# `sum(expr for i in S, j in T)`, with an optional `if` filter on the
# iterators. Returns NULL when there is no generator, so a plain `sum(a, b)`
# falls through to the ordinary call branch.
.jump_generator <- function(func, args_str, symbols, aliases, model,
                            depth, max_depth) {
  at <- .jump_find_keyword(args_str, "for")
  if (is.na(at)) return(NULL)

  value_txt <- trimws(substr(args_str, 1L, at - 1L))
  iter_txt <- trimws(substring(args_str, at + 3L))

  # A trailing `if` filters the iterators.
  cond_txt <- NULL
  ifat <- .jump_find_keyword(iter_txt, "if")
  if (!is.na(ifat)) {
    cond_txt <- trimws(substring(iter_txt, ifat + 2L))
    iter_txt <- trimws(substr(iter_txt, 1L, ifat - 1L))
  }

  bound <- attr(aliases, "bound")
  if (is.null(bound)) bound <- character()
  iters <- .jump_iterators(iter_txt, symbols, model, used = bound)
  if (length(iters) == 0L) return(NULL)

  dim_names <- unlist(lapply(iters, function(it) it$dims), use.names = FALSE)
  inner <- c(aliases, .jump_alias_map(iters))
  attr(inner, "bound") <- c(bound, dim_names)
  index <- do.call(ast_dims, as.list(dim_names))

  # A tuple iterator binds several indices to one mapping: the indices become
  # plain dims and the mapping restricts them, the same node the GAMS reader
  # builds for `sum((i,j)$S(i,j), ...)`.
  maps <- list()
  for (it in iters) {
    if (it$tuple && it$set %in% symbols$mappings) {
      maps[[length(maps) + 1L]] <- ast_mapping(
        it$set, dims = do.call(ast_dims, as.list(it$dims)))
    }
  }
  if (length(maps) > 0L) {
    cond <- maps[[1]]
    for (k in seq_along(maps)[-1]) {
      cond <- ast_expression("and", lhs = cond, rhs = maps[[k]])
    }
    index <- ast_when(condition = cond, then = index)
  }

  value <- parse_jump_expr(value_txt, symbols, inner, model, depth + 1L, max_depth)
  if (!is.null(cond_txt) && nzchar(cond_txt)) {
    value <- ast_when(
      condition = parse_jump_expr(cond_txt, symbols, inner, model,
                                  depth + 1L, max_depth),
      then = value, otherwise = ast_constant(0))
  }

  switch(
    tolower(func),
    sum = ast_sum(index = index, value = value),
    prod = ast_prod(index = index, value = value),
    minimum = ast_setmin(index = index, value = value),
    maximum = ast_setmax(index = index, value = value),
    stop("Unknown aggregation: ", func, call. = FALSE)
  )
}

# Split `@constraint(model, header, body)` into its fields. The macro may be
# omitted, in which case the text is read as `header, body`.
.split_jump_statement <- function(txt, symbols, model) {
  s <- .jump_normalise(txt)
  s <- sub("^@(constraint|NLconstraint)[[:space:]]*", "@constraint", s)

  if (startsWith(s, "@constraint")) {
    rest <- trimws(substring(s, 12L))
    if (!.jump_is_enclosed(rest, "(", ")")) {
      stop("Not a @constraint statement - its arguments are not bracketed:\n  ",
           s, call. = FALSE)
    }
    args <- .jump_split_args(substr(rest, 2L, nchar(rest) - 1L))
    args <- args[nzchar(args)]
    if (length(args) == 3L) {
      header <- args[2]
      body <- args[3]
    } else if (length(args) == 2L) {
      # JuMP allows an anonymous constraint; a multimod equation is keyed on
      # its name, so there is nothing to store it under.
      stop("This constraint has no name, and a multimod equation needs one: ",
           "write @constraint(model, name, expression).", call. = FALSE)
    } else {
      stop("A @constraint takes the model, an optional name and the ",
           "expression; found ", length(args), " arguments:\n  ", s,
           call. = FALSE)
    }
  } else {
    stop("Not a JuMP constraint - it does not start with @constraint:\n  ", s,
         call. = FALSE)
  }

  name <- NULL
  dims <- character(0)
  domain <- NULL
  condition <- NULL
  aliases <- character()
  index_aliases <- character()

  if (!is.null(header)) {
    ix <- .jump_split_indexed(header)
    if (is.null(ix)) {
      if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", trimws(header))) {
        stop("Could not read the constraint name from:\n  ", header,
             call. = FALSE)
      }
      name <- trimws(header)
    } else {
      name <- ix$name
      idx <- ix$index
      # A `;` in the header introduces a filter on the index set.
      lv <- .jump_level(idx)
      semi <- which(lv$char == ";" & lv$level == 0L)
      if (length(semi) > 0L) {
        condition <- trimws(substring(idx, semi[1] + 1L))
        idx <- trimws(substr(idx, 1L, semi[1] - 1L))
      }
      iters <- .jump_iterators(idx, symbols, model)
      aliases <- .jump_alias_map(iters)
      index_aliases <- .jump_index_alias_map(iters)
      dims <- unlist(lapply(iters, function(it) it$dims), use.names = FALSE)
      # The indices already bound travel with the rename map, so a generator
      # inside the body can tell that a set of its own is taken. `c()` drops
      # the attribute, so it is set again wherever the map is extended.
      attr(aliases, "bound") <- dims
      # One mapping over the whole index tuple is the equation's domain.
      if (length(iters) == 1L && iters[[1]]$set %in% symbols$mappings) {
        domain <- ast_mapping(
          iters[[1]]$set,
          dims = do.call(ast_dims, as.list(iters[[1]]$dims)))
      }
    }
  }

  list(name = name, dims = dims, domain = domain, condition = condition,
       aliases = aliases, index_aliases = index_aliases, body = trimws(body))
}

# The first ==, <= or >= outside every bracket.
.jump_relation <- function(body) {
  lv <- .jump_level(body)
  for (op in c("==", "<=", ">=")) {
    at <- gregexpr(op, body, fixed = TRUE)[[1]]
    if (at[1] < 0L) next
    at <- at[lv$level[at] == 0L]
    if (length(at) > 0L) return(list(op = op, at = at[1]))
  }
  NULL
}

#' Parse one JuMP constraint into a multimod equation
#'
#' @param text One `@constraint(...)` statement, as [write_jump()] emits it or
#'   as a modeller writes it. Newlines are allowed.
#' @param symbols Symbol table from [build_symbols_list()].
#' @param model The model, used to resolve a mapping's declared index names so
#'   that JuMP's short aliases come back as the names the model uses.
#' @param desc Description to attach to the equation.
#' @return An equation node.
#' @seealso [mm_equation()] reaches this with `lang = "jump"`.
#' @export
parse_jump_equation <- function(text, symbols, model = NULL, desc = "") {
  spec <- .split_jump_statement(text, symbols, model)
  body <- .jump_normalise(spec$body)

  rel <- .jump_relation(body)
  if (is.null(rel)) {
    stop("No relational operator (==, <=, >=) found in:\n  ", body,
         call. = FALSE)
  }

  lhs_txt <- trimws(substr(body, 1L, rel$at - 1L))
  rhs_txt <- trimws(substring(body, rel$at + nchar(rel$op)))
  if (!nzchar(lhs_txt) || !nzchar(rhs_txt)) {
    stop("The constraint has an empty side:\n  ", body, call. = FALSE)
  }

  # `domain` carries either the mapping the equation is indexed over or the
  # filter in its header; the writer renders whichever it finds. With both, the
  # mapping and the filter have to hold at once.
  dom <- spec$domain
  if (!is.null(spec$condition) && nzchar(spec$condition)) {
    cnd <- parse_jump_expr(spec$condition, symbols, spec$aliases, model)
    dom <- if (is.null(dom)) cnd else ast_expression("and", lhs = dom, rhs = cnd)
  }

  eq <- new_equation(
    name = spec$name,
    desc = desc,
    dims = spec$dims,
    lhs = parse_jump_expr(lhs_txt, symbols, spec$aliases, model),
    rhs = parse_jump_expr(rhs_txt, symbols, spec$aliases, model),
    relation = rel$op,
    domain = dom,
    dims_index_aliases = if (length(spec$index_aliases)) {
      spec$index_aliases
    } else {
      NULL
    }
  )

  eq
}
