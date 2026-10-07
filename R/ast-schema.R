# =============================================================================
# The multimod AST schema - one declarative source of truth
# =============================================================================
#
# This file declares the shape of every AST node type in one R data structure.
# Everything that needs to know that shape is generated from it rather than
# restating it:
#
#   * `ast_json_schema()`   emits a JSON Schema, for non-R readers
#   * `R/ast-json.R`        serialises and parses against it
#   * `validate_ast()`      checks a node against its declared fields
#
# A C++ core and a Python binding need exactly one schema, expressed once. A
# second hand-maintained copy would drift.
#
# -----------------------------------------------------------------------------
# Declaration vs reference
# -----------------------------------------------------------------------------
# The S3 AST uses ONE class name for two structurally different things.
# Counted over every node of the packaged energyRt model:
#
#            declaration   reference
#   set               13         109
#   mapping          316         539
#   parameter        218         418
#   variable          93         417
#   equation         152           0
#
# A declaration carries `data`, `defVal`, `desc` and friends; a reference
# carries a name and the dims it is used at. Today both are class `"set"`, and
# the only thing telling them apart is the `"multimod"` marker in the class
# vector - which, measured across the whole model, sits on exactly the
# declarations and on nothing else.
#
# So `"multimod"` is not an orthogonal marker needing a union: it IS the
# declaration marker. The schema makes that explicit by giving the two shapes
# two names (`set_decl` / `set_ref`). A C++ struct or a Python dataclass has to
# do the same, since neither can have one type that is sometimes `{name}` and
# sometimes `{name, dims, data, defVal, desc, misc, active_dims}`.
#
# -----------------------------------------------------------------------------
# Field types
# -----------------------------------------------------------------------------
#   "string" "number" "integer" "logical"   scalar atoms
#   "string_vector"                         character vector (e.g. set members)
#   "node"                                  one nested AST node
#   "node_list"                             an unnamed list of AST nodes
#   "node_or_list"                          either (`func$value` is both)
#   "table"                                 a data.frame payload - NOT inlined
#                                           in JSON; see R/ast-json.R
#   "decl_list"                             a named list of declarations
#   "any"                                   opaque; round-tripped, not validated
#
# Fields marked required must be present and non-NULL. Everything else may be
# absent, and is omitted from the JSON when NULL rather than written as null.
#
# Several fields are never non-NULL in the packaged models (`defInt`,
# `formula`, `dims_index_aliases`, `subset_of`, `otherwise`) but ARE read
# elsewhere in R/, so they stay in the schema as optional rather than being
# dropped.
# =============================================================================

.mm_schema_version <- "1.0"

# a field spec
.f <- function(type, required = FALSE) list(type = type, required = required)

.mm_schema_nodes <- list(

  # --- declarations: S3 class c(<s3>, "multimod", "ast") ------------------

  set_decl = list(
    kind = "decl", s3 = "mm_set",
    fields = list(
      name      = .f("string", TRUE),
      desc      = .f("string"),
      subset_of = .f("string"),
      data      = .f("string_vector")   # set members are a character vector
    )
  ),

  mapping_decl = list(
    kind = "decl", s3 = "mm_mapping",
    fields = list(
      name        = .f("string", TRUE),
      desc        = .f("string"),
      subset_of   = .f("string"),
      dims        = .f("node"),
      active_dims = .f("node"),
      data        = .f("table"),
      defVal      = .f("number"),
      auto_fold   = .f("logical"),
      misc        = .f("any")
    )
  ),

  parameter_decl = list(
    kind = "decl", s3 = "mm_parameter",
    fields = list(
      name        = .f("string", TRUE),
      desc        = .f("string"),
      dims        = .f("node", TRUE),
      active_dims = .f("node"),
      data        = .f("table"),
      defVal      = .f("number"),
      defInt      = .f("number"),
      symbolic    = .f("logical"),
      formula     = .f("any"),
      comment     = .f("string"),
      auto_fold   = .f("logical"),
      misc        = .f("any")
    )
  ),

  variable_decl = list(
    kind = "decl", s3 = "mm_variable",
    fields = list(
      name        = .f("string", TRUE),
      desc        = .f("string"),
      dims        = .f("node", TRUE),
      active_dims = .f("node"),
      domain      = .f("string"),
      vtype       = .f("string"),
      bounds      = .f("any"),
      comment     = .f("string"),
      data        = .f("table"),
      auto_fold   = .f("logical"),
      misc        = .f("any")
    )
  ),

  equation_decl = list(
    kind = "decl", s3 = "mm_equation",
    fields = list(
      name               = .f("string", TRUE),
      desc               = .f("string"),
      lhs                = .f("node", TRUE),
      rhs                = .f("node", TRUE),
      relation           = .f("string", TRUE),
      dims               = .f("node"),
      domain             = .f("node"),
      comment            = .f("string"),
      dims_index_aliases = .f("any"),
      misc               = .f("any")
    )
  ),

  # --- references: S3 class c(<s3>, "ast"), no "multimod" marker ----------

  set_ref       = list(kind = "ref", s3 = "mm_set",
                       fields = list(name = .f("string", TRUE))),

  mapping_ref   = list(kind = "ref", s3 = "mm_mapping",
                       fields = list(name = .f("string", TRUE),
                                     dims = .f("node"))),

  parameter_ref = list(kind = "ref", s3 = "mm_parameter",
                       fields = list(name = .f("string", TRUE),
                                     dims = .f("node"))),

  variable_ref  = list(kind = "ref", s3 = "mm_variable",
                       fields = list(name = .f("string", TRUE),
                                     dims = .f("node"))),

  symbol        = list(kind = "ref", s3 = "symbol",
                       fields = list(name = .f("string", TRUE))),

  # --- containers and expressions ----------------------------------------

  # `dims` is the one node with UNNAMED contents: a positional list of
  # set/symbol nodes, not a record. It gets an explicit `items` field in JSON
  # so the node type survives the round trip.
  dims = list(kind = "expr", s3 = "dims", unnamed = TRUE,
              fields = list(items = .f("node_list", TRUE))),

  constant   = list(kind = "expr", s3 = "constant",
                    fields = list(value = .f("any", TRUE))),

  expression = list(kind = "expr", s3 = "expression",
                    fields = list(op       = .f("string", TRUE),
                                  lhs      = .f("node", TRUE),
                                  rhs      = .f("node", TRUE),
                                  brackets = .f("logical"))),

  unary      = list(kind = "expr", s3 = "unary",
                    fields = list(op  = .f("string", TRUE),
                                  rhs = .f("node", TRUE))),

  when       = list(kind = "expr", s3 = "when",
                    fields = list(condition = .f("node", TRUE),
                                  then      = .f("node", TRUE),
                                  otherwise = .f("node"))),

  func       = list(kind = "expr", s3 = "func",
                    fields = list(name  = .f("string", TRUE),
                                  value = .f("node_or_list", TRUE),
                                  index = .f("node"))),

  sum        = list(kind = "expr", s3 = "sum",
                    fields = list(index  = .f("node", TRUE),
                                  value  = .f("node", TRUE),
                                  domain = .f("node"))),

  prod       = list(kind = "expr", s3 = "prod",
                    fields = list(index  = .f("node", TRUE),
                                  value  = .f("node", TRUE),
                                  domain = .f("node"))),

  setmin     = list(kind = "expr", s3 = "setmin",
                    fields = list(index = .f("node", TRUE),
                                  value = .f("node", TRUE))),

  setmax     = list(kind = "expr", s3 = "setmax",
                    fields = list(index = .f("node", TRUE),
                                  value = .f("node", TRUE))),

  shift      = list(kind = "expr", s3 = "shift",
                    fields = list(symbol = .f("node", TRUE),
                                  offset = .f("integer", TRUE))),

  call       = list(kind = "expr", s3 = "call",
                    fields = list(name = .f("string", TRUE),
                                  args = .f("node_list", TRUE))),

  where      = list(kind = "expr", s3 = "where",
                    fields = list(name    = .f("string", TRUE),
                                  content = .f("node", TRUE),
                                  hash    = .f("string")))
)

# The model container itself.
#
# `aliases` and `index_aliases` are NOT optional decoration: an equation
# indexed over `tech, techp` needs the alias to expand its domain. Omitting
# `aliases` from an earlier version of this schema silently cost eqEmsFuelTot
# 672 of its 700 nonzeros - the matrix still assembled, at the right shape,
# with the wrong contents. That is the failure mode CLAUDE.md warns about, so
# `model_to_json()` now refuses a model carrying a slot the schema does not
# declare rather than dropping it quietly.
.mm_schema_model <- list(
  s3 = "model",
  fields = list(
    name          = .f("string"),
    desc          = .f("string"),
    sets          = .f("decl_list"),
    aliases       = .f("any"),
    mappings      = .f("decl_list"),
    parameters    = .f("decl_list"),
    variables     = .f("decl_list"),
    equations     = .f("decl_list"),
    objectives    = .f("any"),
    index_aliases = .f("any"),
    inMemory      = .f("logical"),
    base_path     = .f("string"),
    metadata      = .f("any"),
    misc          = .f("any")
  )
)

#' The multimod AST schema
#'
#' A declarative description of every AST node type: its kind (`decl`, `ref` or
#' `expr`), the S3 class it corresponds to, and its fields with their types and
#' whether they are required.
#'
#' Serialisation ([model_to_json()], [model_from_json()]), validation
#' ([validate_ast()]) and the emitted JSON Schema ([ast_json_schema()]) all read
#' this rather than restating the node shapes, so there is one definition to
#' keep correct.
#'
#' @return A list with `version`, `nodes` and `model`.
#' @seealso [schema_type()], [ast_json_schema()], [validate_ast()]
#' @export
#' @examples
#' s <- ast_schema()
#' s$version
#' names(s$nodes)
#' s$nodes$expression$fields
ast_schema <- function() {
  list(version = .mm_schema_version,
       nodes   = .mm_schema_nodes,
       model   = .mm_schema_model)
}

# Lookup tables, built once when the package is sourced. `schema_type()` and
# `.validate_node()` run per constructed node, so recomputing `paste0()`,
# `Filter()` and `names()` on every call showed up as a 2.5x construction
# penalty; precomputing brings it down to a couple of vector lookups.
.mm_lookup <- local({
  # lists, not character vectors: `[[` on a named character vector errors for
  # an absent name, where a list returns NULL
  decl <- list(); ref <- list()
  for (ty in names(.mm_schema_nodes)) {
    sp <- .mm_schema_nodes[[ty]]
    if (identical(sp$kind, "decl")) decl[[sp$s3]] <- ty else ref[[sp$s3]] <- ty
  }
  list(
    decl     = decl,
    ref      = ref,
    required = lapply(.mm_schema_nodes,
                      function(sp) names(Filter(function(f) isTRUE(f$required),
                                                sp$fields))),
    allowed  = lapply(.mm_schema_nodes, function(sp) names(sp$fields)),
    unnamed  = vapply(.mm_schema_nodes, function(sp) isTRUE(sp$unnamed),
                      logical(1))
  )
})

#' Schema node type of an AST node
#'
#' Maps an S3 AST node onto its schema node type, resolving the
#' declaration/reference ambiguity through the `"multimod"` class marker: a
#' node carrying it is a declaration, one without it is a reference.
#'
#' @param x An AST node.
#' @return A single string naming a node type in [ast_schema()], or
#'   `NA_character_` if `x` is not an AST node or its class is unknown.
#' @seealso [ast_schema()]
#' @export
#' @examples
#' schema_type(ast_symbol("t"))
#' schema_type(ast_set("tech"))   # a reference
#' schema_type(new_set("tech"))   # a declaration
schema_type <- function(x) {
  if (!inherits(x, "ast")) return(NA_character_)
  cl <- class(x)[1]
  ty <- if (inherits(x, "multimod")) .mm_lookup$decl[[cl]] else NULL
  if (is.null(ty)) ty <- .mm_lookup$ref[[cl]]
  if (is.null(ty)) NA_character_ else ty
}

#' Node types known to the schema
#'
#' @return Character vector of node type names.
#' @seealso [ast_schema()]
#' @export
#' @examples
#' schema_node_types()
schema_node_types <- function() names(.mm_schema_nodes)

# =============================================================================
# Validation
# =============================================================================
#
# `.validate_node()` checks ONE node and does not recurse. That matters: the
# constructors call it, and a recursive check at every construction level would
# re-walk the whole subtree each time - quadratic in the depth of an equation,
# which for eqCost is 21 levels.
#
# `validate_ast()` is the recursive form, for checking a model that already
# exists.

.validate_node <- function(x, path = "") {
  ty <- schema_type(x)
  if (is.na(ty)) {
    return(sprintf("%s: unknown node class '%s'", path,
                   paste(class(x), collapse = "/")))
  }
  if (.mm_lookup$unnamed[[ty]]) return(character())
  nms <- names(x)
  probs <- character()
  for (fn in .mm_lookup$required[[ty]]) {
    if (is.null(x[[fn]])) {
      probs <- c(probs, sprintf("%s (%s): required field '%s' is missing",
                                path, ty, fn))
    }
  }
  extra <- nms[!(nms %in% .mm_lookup$allowed[[ty]])]
  if (length(extra)) {
    probs <- c(probs, sprintf("%s (%s): undeclared field(s) %s", path, ty,
                              paste(extra, collapse = ", ")))
  }
  probs
}

# Called by the constructors. Off via options(multimod.validate = FALSE) for
# code that builds nodes in a hot loop and knows what it is doing.
.check_node <- function(x, what = NULL) {
  if (!isTRUE(getOption("multimod.validate", TRUE))) return(invisible(x))
  p <- .validate_node(x, what %||% class(x)[1])
  if (length(p)) {
    stop("multimod: invalid AST node.\n  ", paste(p, collapse = "\n  "),
         "\n  The node shapes are declared in ast_schema().", call. = FALSE)
  }
  invisible(x)
}

#' Validate an AST node or model against the schema
#'
#' Checks that a node's class is known, that every required field is present
#' and non-`NULL`, and that it carries no field the schema does not declare.
#'
#' The same check runs automatically inside the constructors, so a malformed
#' node is normally refused where it is built rather than surfacing later as a
#' renderer that silently emits nothing - the trap described in `CLAUDE.md`.
#' Use this function to check a model that already exists, for instance one
#' read back from disk. Set `options(multimod.validate = FALSE)` to switch the
#' constructor-time check off.
#'
#' @param x An AST node or a model.
#' @param path Internal; the path reported in messages.
#' @param recursive Logical; also check nested nodes (default `TRUE`).
#' @return A character vector of problems, empty if `x` is valid.
#' @seealso [ast_schema()], [schema_type()]
#' @export
#' @examples
#' validate_ast(ast_expression("*", ast_symbol("a"), ast_symbol("b")))
#'
#' # a node missing a required field
#' bad <- ast_expression("*", ast_symbol("a"), ast_symbol("b"))
#' bad$rhs <- NULL
#' validate_ast(bad)
validate_ast <- function(x, path = "", recursive = TRUE) {
  probs <- character()
  # a plain container (the model, or a list of declarations)
  if (!inherits(x, "ast") && is.list(x) && !is.data.frame(x)) {
    for (nm in names(x)) {
      v <- x[[nm]]
      if (is.list(v) && !is.data.frame(v)) {
        probs <- c(probs, validate_ast(v, paste0(path, "$", nm), recursive))
      }
    }
    return(probs)
  }
  if (!inherits(x, "ast")) return(probs)
  probs <- .validate_node(x, path)
  if (!recursive) return(probs)
  for (i in seq_along(x)) {
    nm <- if (is.null(names(x))) as.character(i) else names(x)[i]
    v <- x[[i]]
    if (is.list(v) && !is.data.frame(v)) {
      probs <- c(probs, validate_ast(v, paste0(path, "$", nm), recursive))
    }
  }
  probs
}
