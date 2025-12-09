# Add index aliases to a multimod model

Generates and adds short index aliases for sets in a multimod model.
These aliases are used as iteration variable names in constraint
indexing and array subscripts across different output formats (GMPL,
Julia, Pyomo, LaTeX).

## Usage

``` r
add_index_aliases(model, index_aliases = NULL, overwrite = FALSE)
```

## Arguments

- model:

  A multimod model object

- index_aliases:

  Optional named character vector of custom aliases. If NULL (default),
  aliases are auto-generated.

- overwrite:

  Logical. If TRUE, overwrites existing index_aliases. Default FALSE.

## Value

The model with index_aliases added

## Details

Index aliases are short symbolic names used in equations:

- GMPL: `{h in tech, r in region}` where h, r are index aliases

- Julia: `@constraint(model, [h in tech, r in region], ...)`

- LaTeX: `\\sum_{h \\in \\mathcal{T}}`

Auto-generated aliases follow these rules:

- Use first letter(s) of set name

- Handle plurals (e.g., "technologies" -\> "t")

- Avoid conflicts with existing symbols

- For aliases like "commp", append suffix (e.g., "cp")

## Examples

``` r
model <- new_model(
  sets = list(
    tech = new_set("technology"),
    region = new_set("region"),
    comm = new_set("commodity")
  )
)

# Auto-generate aliases
model <- add_index_aliases(model)
# model$index_aliases: c(tech = "h", region = "r", comm = "c")

# Custom aliases
model <- add_index_aliases(model, index_aliases = c(tech = "t", region = "reg"))
#> Model already has index_aliases. Use overwrite=TRUE to replace them.
```
