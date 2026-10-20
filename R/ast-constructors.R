# =============================================================================
# Constructors under the cross-language names
# =============================================================================
#
# multimod is to be reproduced as a C++ core with R and Python bindings. In
# C++ and in Python a type's constructor is spelled as the type:
#
#     Set("tech")        Python
#     Set("tech")        C++
#     new_set("tech")    R, today
#
# These are the R spellings that match. They are ALIASES, not replacements:
# `new_set()` and `ast_set()` keep working unchanged, and nothing in the
# package is rewritten to use the new names. Adding a spelling costs nothing;
# removing one breaks user code.
#
# -----------------------------------------------------------------------------
# Why `Set` and `set` can coexist
# -----------------------------------------------------------------------------
# R is case-sensitive and dispatch is on the class attribute, not the function
# name, so `Set()` (a constructor) and the S3 class `"set"` never collide - and
# neither does `data.table::set()`. Verified, including that `inherits()` is
# unaffected.
#
# -----------------------------------------------------------------------------
# Declaration vs reference, in the names
# -----------------------------------------------------------------------------
# The schema splits each of set/mapping/parameter/variable into a declaration
# and a reference, because they are structurally different objects that today
# share one class name (see R/ast-schema.R). The names follow that split:
#
#     Set()        a DECLARATION - what a model author writes
#     SetRef()     a REFERENCE   - what the parser emits inside an equation
#
# The bare name is the declaration because that is the one users write.
# =============================================================================

#' Constructors under the cross-language names
#'
#' Aliases for the AST and model constructors, spelled the way the same type is
#' spelled in C++ and Python (`Set("tech")` rather than `new_set("tech")`).
#' They call the existing constructors and return the same objects; the
#' original names are unaffected.
#'
#' Declarations take the bare name (`Set()`, `Parameter()`, `Variable()`,
#' `Mapping()`, `Equation()`); references take a `Ref` suffix (`SetRef()`,
#' `ParameterRef()`, `VariableRef()`, `MappingRef()`). The two are structurally
#' different objects - see [ast_schema()].
#'
#' @param ... Arguments passed to the underlying constructor.
#' @return An AST node.
#' @seealso [ast_schema()], [new_set()], [ast_set()]
#' @name multimod-constructors
#' @examples
#' Set("tech")                      # a declaration
#' SetRef("tech")                   # a reference
#' Expression("*", Symbol("a"), Symbol("b"))
NULL

# --- declarations ------------------------------------------------------------

#' @rdname multimod-constructors
#' @export
Set <- function(...) new_set(...)

#' @rdname multimod-constructors
#' @export
Mapping <- function(...) new_mapping(...)

#' @rdname multimod-constructors
#' @export
Parameter <- function(...) new_parameter(...)

#' @rdname multimod-constructors
#' @export
Variable <- function(...) new_variable(...)

#' @rdname multimod-constructors
#' @export
Equation <- function(...) new_equation(...)

# --- references --------------------------------------------------------------

#' @rdname multimod-constructors
#' @export
SetRef <- function(...) ast_set(...)

#' @rdname multimod-constructors
#' @export
MappingRef <- function(...) ast_mapping(...)

#' @rdname multimod-constructors
#' @export
ParameterRef <- function(...) ast_parameter(...)

#' @rdname multimod-constructors
#' @export
VariableRef <- function(...) ast_variable(...)

#' @rdname multimod-constructors
#' @export
Symbol <- function(...) ast_symbol(...)

# --- expressions -------------------------------------------------------------

#' @rdname multimod-constructors
#' @export
Dims <- function(...) ast_dims(...)

#' @rdname multimod-constructors
#' @export
Constant <- function(...) ast_constant(...)

#' @rdname multimod-constructors
#' @export
Expression <- function(...) ast_expression(...)

#' @rdname multimod-constructors
#' @export
Unary <- function(...) ast_unary(...)

#' @rdname multimod-constructors
#' @export
When <- function(...) ast_when(...)

#' @rdname multimod-constructors
#' @export
Func <- function(...) ast_func(...)

#' @rdname multimod-constructors
#' @export
Sum <- function(...) ast_sum(...)

#' @rdname multimod-constructors
#' @export
Prod <- function(...) ast_prod(...)

#' @rdname multimod-constructors
#' @export
SetMin <- function(...) ast_setmin(...)

#' @rdname multimod-constructors
#' @export
SetMax <- function(...) ast_setmax(...)

#' @rdname multimod-constructors
#' @export
Shift <- function(...) ast_shift(...)

#' @rdname multimod-constructors
#' @export
Call <- function(...) ast_call(...)

#' @rdname multimod-constructors
#' @export
Where <- function(...) ast_where(...)

#' Is this node a declaration rather than a reference?
#'
#' The AST uses one class name for two structurally different things: a
#' declaration (`new_set()`, carrying `data`, `desc` and friends) and a
#' reference (`ast_set()`, carrying a name and the dims it is used at). The
#' `"multimod"` class marker is what separates them - measured across the whole
#' packaged model, it sits on every declaration and on nothing else.
#'
#' Prefer this to `inherits(x, "multimod")`, which says what the marker *is*
#' rather than what it *means*.
#'
#' @param x An AST node.
#' @return `TRUE` for a declaration, `FALSE` otherwise.
#' @seealso [ast_schema()], [schema_type()]
#' @export
#' @examples
#' is_declaration(new_set("tech"))   # TRUE
#' is_declaration(ast_set("tech"))   # FALSE
is_declaration <- function(x) inherits(x, "multimod") && inherits(x, "ast")
