# Read GMPL declarations sequentially

Scans a .dat file and returns every `set` or `param` declaration in the
original order they appear. Each entry keeps the parsed metadata while
adding `kind` (the keyword used in the file), `order` (1-based
position), and the raw declaration lines for reference. This avoids
re-grouping declarations into sets/mappings/parameters buckets.

## Usage

``` r
read_gmpl_declarations(file)
```

## Arguments

- file:

  Path to .dat file

## Value

List where each element represents a declaration with metadata, parsed
content, and raw text
