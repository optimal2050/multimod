# Marker for a state the author believed unreachable

Replaces [`browser()`](https://rdrr.io/r/base/browser.html) calls left
in package code. [`browser()`](https://rdrr.io/r/base/browser.html)
stops an interactive session and waits for input with no message –
indistinguishable from a hang – while under `Rscript` it is inert, so it
survives CI and only ever bites a human. This warns and continues: the
signal is kept, the block is not.

## Usage

``` r
.dev_break(context = "")
```

## Arguments

- context:

  Where it fired, as "file.R:line".

## Value

Invisibly, `NULL`; called for the warning it raises.
