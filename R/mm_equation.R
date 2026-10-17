#' Write an equation as MathProg or GAMS text
#'
#' Equations are the one part of a model that does not read well as nested
#' constructors: a cost balance becomes several levels of [ast_expression()]
#' before it says anything. `mm_equation()` takes the statement as a modeller
#' would write it and parses it into the same AST the constructors build.
#'
#' Declarations stay in R. [new_set()], [new_parameter()] and
#' [new_variable()] are flat and read well; only the expression needs the
#' notation.
#'
#' @param model A multimod model. Its declarations resolve the names used in
#'   `text`, so every set, parameter and variable the equation refers to must
#'   already be in it.
#' @param text One equation statement. For `lang = "gmpl"` this is MathProg,
#'   `name{indexing}: lhs <op> rhs` with an optional `s.t.` prefix and
#'   trailing `;`, where `<op>` is `<=`, `>=` or `=`. For `lang = "gams"` it
#'   is `name(dims)$domain(dims) .. lhs <op> rhs` with `=l=`, `=g=` or `=e=`.
#'   For `lang = "jump"` it is one `@constraint(model, name[i in S],
#'   lhs <op> rhs)` statement, with `==` for equality. Newlines are
#'   allowed in all three.
#' @param desc Description to attach to the equation.
#' @param lang The notation `text` is written in. MathProg is the default: it
#'   is an open language and uses the ordinary relational operators. JuMP
#'   and GAMS are read as well; all three produce the same AST.
#'
#' @return An equation node, ready to assign into `model$equations`.
#' @seealso [new_equation()] builds the same object from constructors, which
#'   is the route for an equation that is generated rather than written.
#' @export
#' @examples
#' m <- example_models$toy
#'
#' # MathProg (default)
#' eq <- mm_equation(
#'   m,
#'   "eqCapLim{(tech,region) in mTR}:
#'      vGen[tech,region] <= pAvail[tech,region] * vCap[tech,region]",
#'   desc = "generation is limited by available capacity"
#' )
#' cat(as_latex(eq))
#'
#' # the same equation in GAMS
#' eq2 <- mm_equation(
#'   m,
#'   "eqCapLim(tech,region)$mTR(tech,region) ..
#'      vGen(tech,region) =l= pAvail(tech,region) * vCap(tech,region)",
#'   lang = "gams"
#' )
#'
#' # and in JuMP
#' eq3 <- mm_equation(
#'   m,
#'   "@constraint(model, eqCapLim[(h, r) in mTR],
#'      vGen[(h, r)] <= get(pAvail, (h, r), pAvailDef) * vCap[(h, r)])",
#'   lang = "jump"
#' )
mm_equation <- function(model, text, desc = "",
                        lang = c("gmpl", "gams", "jump")) {
  lang <- match.arg(lang)
  if (!(inherits(model, "multimod") || inherits(model, "model"))) {
    stop("`model` must be a multimod model.", call. = FALSE)
  }
  if (!is.character(text) || length(text) != 1L || !nzchar(trimws(text))) {
    stop("`text` must be a single non-empty equation statement.", call. = FALSE)
  }

  if (lang == "jump") {
    return(parse_jump_equation(text, build_symbols_list(model),
                               model = model, desc = desc))
  }

  if (lang == "gams") {
    parts <- .split_gams_statement(text)
    # parse_equation_header() expects the separator, which the split removed
    hdr <- parse_equation_header(paste0(parts$header, ".."))
    if (is.null(hdr)) {
      stop("Could not read the equation header:\n  ", parts$header, call. = FALSE)
    }
    return(parse_gams_equation(
      list(name = hdr$name, dims = hdr$dims, desc = desc,
           domain = hdr$condition, gams = parts$body),
      build_symbols_list(model)))
  }

  spec <- .split_gmpl_statement(text)
  eq <- parse_gmpl_equation_to_ast(
    list(name = spec$name, dims = spec$dims, desc = desc,
         condition = spec$condition, sense = spec$sense, body = spec$body),
    build_symbols_list(model))
  # `condition` is the `{... : filter}` test; the indexing set is the domain
  # mapping, which the MathProg reader does not carry and which the matrix
  # route needs, so attach it here.
  if (!is.null(spec$domain)) eq$domain <- spec$domain
  eq
}

# Split "header .. body;" into its two parts. Newlines are folded first, so a
# statement may be written across several lines.
.split_gams_statement <- function(txt) {
  s <- .one_line(txt)
  at <- regexpr("..", s, fixed = TRUE)
  if (at < 0L) {
    stop("Not an equation statement - no '..' between header and body:\n  ", s,
         call. = FALSE)
  }
  list(header = trimws(substr(s, 1L, at - 1L)),
       body   = trimws(substr(s, at + 2L, nchar(s))))
}

# Split "s.t. name{indexing}: lhs <op> rhs;" into the fields the AST builder
# wants. The indexing is read with extract_indexing(), so the tuple form
# `{(i,j) in S}` yields the index names as dims and S as the domain mapping.
.split_gmpl_statement <- function(txt) {
  s <- .one_line(txt)
  low <- tolower(s)
  if (startsWith(low, "s.t. "))      s <- trimws(substring(s, 6L))
  if (startsWith(low, "subject to ")) s <- trimws(substring(s, 12L))

  at <- regexpr("^[A-Za-z_][A-Za-z0-9_]*", s)
  if (at < 0L) {
    stop("Not an equation statement - it does not start with a name:\n  ", s,
         call. = FALSE)
  }
  name <- regmatches(s, at)
  rest <- substring(s, attr(at, "match.length") + 1L)

  dims <- character(0)
  domain <- NULL
  condition <- NULL
  if (grepl("^[[:space:]]*[{]", rest)) {
    ix <- extract_indexing(rest)
    condition <- ix$condition
    specs <- ix$indexing$specs
    dims <- unlist(lapply(specs, function(sp) sp$vars), use.names = FALSE)
    sets <- unlist(lapply(specs, function(sp) sp$set), use.names = FALSE)
    # one set over the whole index tuple is a domain mapping; several sets are
    # a plain product and leave the equation without one
    if (length(sets) == 1L && length(specs) == 1L) {
      domain <- ast_mapping(sets, dims = do.call(ast_dims, as.list(dims)))
    }
    rest <- ix$remaining
  }
  rest <- sub("^[[:space:]]*:[[:space:]]*", "", rest)

  sense <- .gmpl_sense(rest)
  if (is.null(sense)) {
    stop("No relational operator (<=, >=, =) found in:\n  ", rest, call. = FALSE)
  }
  list(name = name, dims = dims, domain = domain, condition = condition,
       sense = sense, body = trimws(rest))
}

# The first <=, >= or = that is not inside brackets.
.gmpl_sense <- function(body) {
  chars <- strsplit(body, "", fixed = TRUE)[[1]]
  depth <- 0L
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch %in% c("(", "[", "{")) depth <- depth + 1L
    else if (ch %in% c(")", "]", "}")) depth <- depth - 1L
    else if (depth == 0L && ch %in% c("<", ">", "=")) {
      nxt <- if (i < length(chars)) chars[i + 1L] else ""
      if (ch %in% c("<", ">") && nxt == "=") return(paste0(ch, "="))
      if (ch == "=" && nxt != "=") return("=")
      if (ch == "=" && nxt == "=") return("=")
    }
  }
  NULL
}

# Fold a statement onto one line and drop a trailing semicolon.
.one_line <- function(txt) {
  s <- trimws(gsub("[[:space:]]+", " ", txt))
  sub(";[[:space:]]*$", "", s)
}
