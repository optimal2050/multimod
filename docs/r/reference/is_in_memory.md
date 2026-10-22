# Data management utilities for multimod models

Functions to manage in-memory vs on-disk data storage for model
parameters and mappings, following the energyRt pattern. Check if data
is in memory

## Usage

``` r
is_in_memory(obj)
```

## Arguments

- obj:

  A multimod model or parameter/mapping object

## Value

Logical. TRUE if data is in memory, FALSE if on disk

## Details

Checks hierarchy:

1.  Object's misc\$inMemory if set

2.  Model's inMemory default if object is a parameter/mapping

3.  TRUE by default (conservative)
