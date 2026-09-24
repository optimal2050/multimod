# =============================================================================
# Language-neutral serialisation of the AST
# =============================================================================
#
# `save_model()` persists structure with `saveRDS()`, which is readable only by
# R. A C++ core or a Python binding needs the AST in a form it can parse, so
# this file writes and reads it as JSON, driven entirely by `R/ast-schema.R`.
#
# -----------------------------------------------------------------------------
# What the encoding looks like
# -----------------------------------------------------------------------------
# Every node is a JSON object tagged with its schema type:
#
#   {"node": "expression", "op": "*",
#    "lhs": {"node": "parameter_ref", "name": "pCost", "dims": {...}},
#    "rhs": {"node": "variable_ref",  "name": "vTechAct", "dims": {...}}}
#
# The tag is the SCHEMA type, not the S3 class, so `set_decl` and `set_ref` are
# distinguishable - which they are not from the class name alone. See the
# declaration/reference note in R/ast-schema.R.
#
# `dims` is the one node with unnamed contents, so it carries an explicit
# `items` array rather than named fields.
#
# -----------------------------------------------------------------------------
# Three things deliberately do NOT go in the JSON by default
# -----------------------------------------------------------------------------
# * `data` - the bulk payload. Parameter and mapping tables run to millions of
#   rows on real models; JSON is the wrong container and Arrow is the right
#   one. `save_model()` already writes Parquet. Pass `data = "inline"` to embed
#   it anyway, which is useful for small models and for the round-trip tests.
#
# * present-but-NULL vs absent. R distinguishes `list(a = NULL)` from `list()`,
#   and the AST relies on it - `equation$comment` is present and NULL on all
#   152 equations. A present-but-NULL field is written as JSON `null`; an
#   absent one is omitted. Reading restores the distinction.
#
# * field order. Nodes are written in their own field order, not schema order,
#   so a round trip is `identical()` rather than merely equivalent.
#
# -----------------------------------------------------------------------------
# `any` fields
# -----------------------------------------------------------------------------
# `misc`, `bounds`, `formula` and `dims_index_aliases` hold arbitrary R values -
# mostly R-side bookkeeping (`inMemory` flags, on-disk paths) rather than model
# semantics. Plain JSON would lose their types (integer becomes double, a named
# list becomes an object, `character(0)` becomes null), so they are written
# with an explicit type tag:
#
#   {"$t": "integer", "$v": [1, 2]}
#   {"$t": "list", "$n": ["a", "b"], "$v": [...]}
#
# A non-R reader can ignore `$t` and read `$v`. Nothing in the model's
# mathematics depends on these fields.
# =============================================================================

# --- type-tagged encoding for `any` -----------------------------------------

# Attributes are carried explicitly. Without this a classed atomic - a POSIXct,
# say - is written by toJSON() as its FORMATTED form ("2026-09-08 21:53:42"),
# which then coerces back to NA with a warning. Writing the underlying double
# plus its class restores it exactly, and the same mechanism preserves the
# names on a named vector.
.attr_to_json <- function(v, skip = character()) {
  a <- attributes(v)
  a <- a[setdiff(names(a), skip)]
  if (!length(a)) return(NULL)
  lapply(a, .any_to_json)
}

.attr_from_json <- function(out, j) {
  a <- j[["$a"]]
  if (is.null(a)) return(out)
  # class last: setting it first can change how later attributes are assigned
  for (nm in setdiff(names(a), "class")) attr(out, nm) <- .any_from_json(a[[nm]])
  if (!is.null(a[["class"]])) class(out) <- .any_from_json(a[["class"]])
  out
}

.any_to_json <- function(v) {
  if (is.null(v)) return(NULL)
  if (is.data.frame(v)) return(c(list(`$t` = "data.frame"), .table_to_json(v)))
  if (is.list(v)) {
    return(list(`$t` = "list",
                `$n` = if (is.null(names(v))) NULL else as.list(names(v)),
                `$v` = unname(lapply(v, .any_to_json)),
                `$a` = .attr_to_json(v, skip = c("names"))))
  }
  list(`$t` = typeof(v),
       `$v` = if (length(v)) as.list(unclass(unname(v))) else list(),
       `$a` = .attr_to_json(v))
}

.any_from_json <- function(j) {
  if (is.null(j)) return(NULL)
  t <- j[["$t"]]
  if (is.null(t)) return(j)                       # untagged: hand back as-is
  t <- as.character(t)[1]
  if (identical(t, "data.frame")) return(.table_from_json(j))
  if (identical(t, "list")) {
    out <- lapply(j[["$v"]], .any_from_json)
    nm <- j[["$n"]]
    if (!is.null(nm)) names(out) <- as.character(unlist(nm))
    return(.attr_from_json(out, j))
  }
  v <- j[["$v"]]
  # an empty JSON array is a zero-length vector of the recorded type
  if (is.null(v) || !length(v)) return(.attr_from_json(vector(t, 0L), j))
  # NULLs inside an atomic vector are JSON nulls standing in for NA
  v <- lapply(v, function(e) if (is.null(e)) NA else e)
  out <- unlist(v, use.names = FALSE)
  storage.mode(out) <- t
  .attr_from_json(out, j)
}

# --- tables ------------------------------------------------------------------
# Columns plus their storage types, so an integer column does not come back as
# double. `data.table` is recorded so the class survives.

.table_to_json <- function(d) {
  list(
    `$class` = as.list(class(d)),
    `$types` = as.list(vapply(d, function(c1) class(c1)[1], character(1))),
    # `row.names` is recorded only when it is not the default 1:n. Tables
    # carved out of a larger one keep their original numbering (8, 9, 10, ...)
    # and dropping it makes the round trip merely equivalent, not identical().
    `$rn`    = {
      rn <- attr(d, "row.names")
      if (identical(rn, seq_len(nrow(d))) || identical(rn, as.integer(seq_len(nrow(d))))) NULL
      else as.list(rn)
    },
    `$cols`  = lapply(as.list(d), function(c1) {
      c1 <- if (is.factor(c1)) as.character(c1) else c1
      lapply(seq_along(c1), function(i) if (is.na(c1[i])) NULL else c1[i])
    })
  )
}

.table_from_json <- function(j) {
  cols <- j[["$cols"]]
  types <- vapply(j[["$types"]], as.character, character(1))
  out <- lapply(names(cols), function(nm) {
    v <- lapply(cols[[nm]], function(e) if (is.null(e)) NA else e)
    v <- unlist(v, use.names = FALSE)
    if (is.null(v)) v <- logical(0)
    switch(types[[nm]],
           character = as.character(v),
           integer   = as.integer(v),
           numeric   = as.numeric(v),
           double    = as.numeric(v),
           logical   = as.logical(v),
           v)
  })
  names(out) <- names(cols)
  n <- if (length(out)) length(out[[1]]) else 0L
  rn <- if (is.null(j[["$rn"]])) seq_len(n) else unlist(j[["$rn"]], use.names = FALSE)
  cl <- as.character(unlist(j[["$class"]]))
  if ("data.table" %in% cl && requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::setDT(out)[])
  }
  # attributes in base R's own order - names, row.names, class - so that a
  # round trip is identical() and not merely all.equal()
  attr(out, "row.names") <- rn
  class(out) <- "data.frame"
  out
}

# --- one value, by its declared schema type ---------------------------------

.value_to_json <- function(v, type, data_mode) {
  if (is.null(v)) return(NULL)
  switch(type,
    node          = .node_to_json(v, data_mode),
    node_list     = unname(lapply(v, .node_to_json, data_mode = data_mode)),
    node_or_list  = if (inherits(v, "ast")) .node_to_json(v, data_mode)
                    else unname(lapply(v, .node_to_json, data_mode = data_mode)),
    table         = if (identical(data_mode, "inline")) .table_to_json(v) else NULL,
    string_vector = if (identical(data_mode, "inline")) as.list(v) else NULL,
    any           = .any_to_json(v),
    v                                            # scalars pass through
  )
}

.value_from_json <- function(j, type) {
  if (is.null(j)) return(NULL)
  switch(type,
    node          = .node_from_json(j),
    node_list     = lapply(j, .node_from_json),
    node_or_list  = if (!is.null(j$node)) .node_from_json(j)
                    else lapply(j, .node_from_json),
    table         = .table_from_json(j),
    string_vector = as.character(unlist(j, use.names = FALSE)),
    any           = .any_from_json(j),
    string        = as.character(unlist(j, use.names = FALSE)),
    number        = as.numeric(unlist(j, use.names = FALSE)),
    integer       = as.integer(unlist(j, use.names = FALSE)),
    logical       = as.logical(unlist(j, use.names = FALSE)),
    unlist(j, use.names = FALSE)
  )
}

# --- nodes -------------------------------------------------------------------

.node_to_json <- function(x, data_mode = "omit") {
  ty <- schema_type(x)
  if (is.na(ty)) {
    stop("ast_to_json: no schema entry for a node of class ",
         paste(class(x), collapse = "/"),
         ". Add it to R/ast-schema.R.", call. = FALSE)
  }
  sp <- .mm_schema_nodes[[ty]]
  if (isTRUE(sp$unnamed)) {
    return(list(node = ty,
                items = unname(lapply(unclass(x), .node_to_json,
                                      data_mode = data_mode))))
  }
  out <- list(node = ty)
  # the node's OWN field order, so the round trip is identical()
  for (fn in names(x)) {
    spec <- sp$fields[[fn]]
    if (is.null(spec)) {
      stop("ast_to_json: node type '", ty, "' has an undeclared field '", fn,
           "'. Add it to R/ast-schema.R.", call. = FALSE)
    }
    v <- .value_to_json(x[[fn]], spec$type, data_mode)
    # present-but-NULL must survive as JSON null, not vanish
    if (is.null(v)) out[fn] <- list(NULL) else out[[fn]] <- v
  }
  out
}

.s3_class_for <- function(ty) {
  sp <- .mm_schema_nodes[[ty]]
  if (identical(sp$kind, "decl")) c(sp$s3, "multimod", "ast") else c(sp$s3, "ast")
}

.node_from_json <- function(j) {
  ty <- j$node
  if (is.null(ty)) stop("ast_from_json: object has no 'node' tag.", call. = FALSE)
  ty <- as.character(ty)[1]
  sp <- .mm_schema_nodes[[ty]]
  if (is.null(sp)) {
    stop("ast_from_json: unknown node type '", ty, "'. Known types: ",
         paste(schema_node_types(), collapse = ", "), call. = FALSE)
  }
  if (isTRUE(sp$unnamed)) {
    return(structure(lapply(j$items, .node_from_json),
                     class = .s3_class_for(ty)))
  }
  flds <- list()
  for (fn in setdiff(names(j), "node")) {
    spec <- sp$fields[[fn]]
    if (is.null(spec)) next
    v <- .value_from_json(j[[fn]], spec$type)
    if (is.null(v)) flds[fn] <- list(NULL) else flds[[fn]] <- v
  }
  structure(flds, class = .s3_class_for(ty))
}

# --- public API --------------------------------------------------------------

#' Convert an AST node to a language-neutral list
#'
#' The list is plain R data - no classes - ready for [jsonlite::toJSON()] or any
#' other serialiser. Node types are tagged with their schema name, so
#' declarations and references stay distinguishable.
#'
#' @param x An AST node.
#' @param data How to treat `data` payloads: `"omit"` (the default) leaves them
#'   out, on the basis that bulk data belongs in the Parquet store;
#'   `"inline"` embeds them.
#' @return A nested list.
#' @seealso [ast_from_json()], [model_to_json()], [ast_schema()]
#' @export
#' @examples
#' n <- ast_expression("*", ast_symbol("a"), ast_symbol("b"))
#' str(ast_to_json(n), max.level = 2)
ast_to_json <- function(x, data = c("omit", "inline")) {
  data <- match.arg(data)
  .node_to_json(x, data_mode = data)
}

#' Rebuild an AST node from a language-neutral list
#'
#' @param j A list as produced by [ast_to_json()], or the result of
#'   [jsonlite::fromJSON()] with `simplifyVector = FALSE`.
#' @return An AST node.
#' @seealso [ast_to_json()]
#' @export
#' @examples
#' n <- ast_expression("*", ast_symbol("a"), ast_symbol("b"))
#' identical(ast_from_json(ast_to_json(n)), n)
ast_from_json <- function(j) .node_from_json(j)

#' Serialise a model to JSON
#'
#' Writes the model's structure - sets, mappings, parameters, variables and
#' equations - in the language-neutral encoding described in [ast_schema()].
#'
#' Bulk `data` payloads are omitted by default: they belong in the Parquet
#' store that [save_model()] writes, not in a JSON document. Pass
#' `data = "inline"` for a self-contained file, which is practical for small
#' models.
#'
#' @param model A multimod model.
#' @param path Optional file to write to. If `NULL`, the JSON is returned as a
#'   character string.
#' @param data `"omit"` (default) or `"inline"`; see Details.
#' @param pretty Logical; pretty-print the JSON.
#' @return The JSON string, invisibly if `path` was given.
#' @seealso [model_from_json()], [ast_schema()], [save_model()]
#' @export
#' @examples
#' \dontrun{
#' data(example_models)
#' js <- model_to_json(example_models$energyRt$multimod)
#' m2 <- model_from_json(js)
#' }
model_to_json <- function(model, path = NULL, data = c("omit", "inline"),
                          pretty = TRUE) {
  data <- match.arg(data)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("model_to_json() needs the 'jsonlite' package.", call. = FALSE)
  }
  # A model slot the schema does not declare would otherwise be dropped in
  # silence, and the reader would get a model that assembles into a matrix of
  # the right SHAPE with the wrong contents - which is much harder to notice
  # than a failure. `aliases` went missing exactly this way.
  unknown <- setdiff(names(model), names(.mm_schema_model$fields))
  if (length(unknown)) {
    stop("model_to_json: the model has slot(s) the schema does not declare: ",
         paste(unknown, collapse = ", "),
         ".\n  Dropping them would silently change the model. Add them to ",
         ".mm_schema_model in R/ast-schema.R.", call. = FALSE)
  }
  out <- list(multimod_schema = .mm_schema_version,
              node = "model",
              data_inlined = identical(data, "inline"))
  # the model's own slot order, so a round trip preserves it
  for (fn in names(model)) {
    spec <- .mm_schema_model$fields[[fn]]
    v <- model[[fn]]
    if (is.null(v)) { out[fn] <- list(NULL); next }
    out[[fn]] <- switch(spec$type,
      decl_list = {
        lst <- lapply(v, .node_to_json, data_mode = data)
        names(lst) <- names(v)
        lst
      },
      any    = .any_to_json(v),
      string = v,
      v)
  }
  # 17 SIGNIFICANT digits is the IEEE 754 guarantee that a double survives the
  # text round trip exactly. jsonlite's `digits = NA` ("max precision") goes
  # through as.character() and loses ~3e-15 relative - enough to make
  # coefficients differ in the last bits, which is the kind of drift that is
  # invisible until two backends disagree.
  js <- jsonlite::toJSON(out, auto_unbox = TRUE, null = "null",
                         digits = I(17), pretty = pretty)
  if (is.null(path)) return(as.character(js))
  writeLines(as.character(js), path)
  invisible(as.character(js))
}

#' Rebuild a model from JSON
#'
#' @param json A JSON string, or a path to a file written by [model_to_json()].
#' @return A multimod model.
#' @seealso [model_to_json()]
#' @export
#' @examples
#' \dontrun{
#' m2 <- model_from_json("model.json")
#' }
model_from_json <- function(json) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("model_from_json() needs the 'jsonlite' package.", call. = FALSE)
  }
  if (length(json) == 1L && !grepl("[{}]", json) && file.exists(json)) {
    json <- paste(readLines(json, warn = FALSE), collapse = "\n")
  }
  j <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  v <- j$multimod_schema
  if (is.null(v)) {
    stop("model_from_json: not a multimod document (no 'multimod_schema').",
         call. = FALSE)
  }
  if (!identical(as.character(v)[1], .mm_schema_version)) {
    warning("model_from_json: document schema ", as.character(v)[1],
            " but this multimod speaks ", .mm_schema_version, ".")
  }
  out <- list()
  meta <- c("multimod_schema", "node", "data_inlined")
  for (fn in setdiff(names(j), meta)) {
    spec <- .mm_schema_model$fields[[fn]]
    if (is.null(spec)) next
    if (is.null(j[[fn]])) { out[fn] <- list(NULL); next }
    out[[fn]] <- switch(spec$type,
      decl_list = {
        lst <- lapply(j[[fn]], .node_from_json)
        names(lst) <- names(j[[fn]])
        lst
      },
      any     = .any_from_json(j[[fn]]),
      string  = as.character(unlist(j[[fn]], use.names = FALSE)),
      logical = as.logical(unlist(j[[fn]], use.names = FALSE)),
      number  = as.numeric(unlist(j[[fn]], use.names = FALSE)),
      j[[fn]])
  }
  structure(out, class = c("model", "multimod"))
}

#' Emit a JSON Schema for the multimod AST
#'
#' Generated from [ast_schema()], so it cannot drift from what R actually
#' writes. Intended for non-R readers of the serialised model - a C++ core, a
#' Python binding, or an editor with schema-aware completion.
#'
#' @param path Optional file to write to.
#' @return The JSON Schema as a character string.
#' @seealso [ast_schema()], [model_to_json()]
#' @export
#' @examples
#' \dontrun{
#' cat(ast_json_schema())
#' }
ast_json_schema <- function(path = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("ast_json_schema() needs the 'jsonlite' package.", call. = FALSE)
  }
  jtype <- function(t) switch(t,
    string        = list(type = "string"),
    number        = list(type = "number"),
    integer       = list(type = "integer"),
    logical       = list(type = "boolean"),
    string_vector = list(type = "array", items = list(type = "string")),
    node          = list(`$ref` = "#/$defs/node"),
    node_list     = list(type = "array", items = list(`$ref` = "#/$defs/node")),
    node_or_list  = list(anyOf = list(list(`$ref` = "#/$defs/node"),
                                      list(type = "array",
                                           items = list(`$ref` = "#/$defs/node")))),
    table         = list(`$ref` = "#/$defs/table"),
    list()                                   # "any": unconstrained
  )
  defs <- list()
  for (ty in names(.mm_schema_nodes)) {
    sp <- .mm_schema_nodes[[ty]]
    props <- list(node = list(const = ty))
    req <- "node"
    for (fn in names(sp$fields)) {
      props[[fn]] <- jtype(sp$fields[[fn]]$type)
      if (isTRUE(sp$fields[[fn]]$required)) req <- c(req, fn)
    }
    defs[[ty]] <- list(type = "object", properties = props,
                       required = as.list(req))
  }
  defs$table <- list(type = "object", description =
    "A tabular payload: $class, $types and $cols. Bulk data normally lives in the Parquet store instead.")
  defs$node <- list(oneOf = unname(lapply(names(.mm_schema_nodes),
                                          function(t) list(`$ref` = paste0("#/$defs/", t)))))
  schema <- list(
    `$schema` = "https://json-schema.org/draft/2020-12/schema",
    `$id` = paste0("https://multimod.dev/schema/ast-v", .mm_schema_version, ".json"),
    title = "multimod model",
    description = paste("Abstract syntax tree of a multimod optimization model.",
                        "Generated from ast_schema(); do not edit by hand."),
    type = "object",
    properties = list(
      multimod_schema = list(type = "string", const = .mm_schema_version),
      node = list(const = "model"),
      name = list(type = "string"),
      desc = list(type = "string"),
      sets = list(type = "object", additionalProperties = list(`$ref` = "#/$defs/set_decl")),
      mappings = list(type = "object", additionalProperties = list(`$ref` = "#/$defs/mapping_decl")),
      parameters = list(type = "object", additionalProperties = list(`$ref` = "#/$defs/parameter_decl")),
      variables = list(type = "object", additionalProperties = list(`$ref` = "#/$defs/variable_decl")),
      equations = list(type = "object", additionalProperties = list(`$ref` = "#/$defs/equation_decl"))
    ),
    required = list("multimod_schema"),
    `$defs` = defs
  )
  js <- as.character(jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE,
                                      null = "null"))
  if (!is.null(path)) writeLines(js, path)
  js
}
