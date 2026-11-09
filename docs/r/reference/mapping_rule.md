# A mapping rule

The tuples of the mapping are the natural join of the tables named in
`of`, in order (tables without a shared column form a cross product),
kept where they match every table in `gate` (semi-joins) and none in
`minus` (anti-joins), projected to `cols` and made unique. A name
resolves to an embedded table in `tables`, else to a mapping of the
model, else to a set of the model (a one-column table under the set's
name). Columns of a join that are not in `cols` are hidden: they take
part in the joins and are dropped by the projection.

## Usage

``` r
mapping_rule(of, gate = NULL, minus = NULL, tables = NULL, cols = NULL)

materialise_rule(model, rule)
```

## Arguments

- of:

  Character; the tables joined.

- gate, minus:

  Character; tables whose matching tuples are kept, or dropped. Matching
  is on the columns shared with the join.

- tables:

  A named list of data frames embedded in the rule; small tables that
  are not symbols of the model.

- cols:

  Character; the columns of the result, in the mapping's dimension
  order.
  [`new_mapping()`](https://optimal2050.github.io/multimod/r/reference/new_mapping.md)
  fills it from `dims` when `NULL`.

- model:

  The model whose mappings and sets the rule names; may be an empty list
  when every leaf is embedded.

- rule:

  An `mm_rule`.

## Value

An object of class `mm_rule`; its `hash` identifies the rule's content
(names and embedded tables).

A data.frame of character columns `rule$cols`, one row per tuple.

## Examples

``` r
pf <- data.frame(tech = c("coal", "wind"), frame = c("ANNUAL", "HOUR"))
fs <- data.frame(
  frame = c("ANNUAL", "HOUR", "HOUR"),
  slice = c("y", "h1", "h2")
)
r <- mapping_rule(
  of = c("pf", "fs"), tables = list(pf = pf, fs = fs),
  cols = c("tech", "slice")
)
m <- new_mapping("mTechSlice", dims = c("tech", "slice"), rule = r)
materialise_rule(list(), r)
#>   tech slice
#> 1 coal     y
#> 2 wind    h1
#> 3 wind    h2
```
